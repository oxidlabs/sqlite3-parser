use crate::ast::{
    AlterTable, AlterTableStmt, ColumnConstraint, ColumnDef, ColumnOrder, CreateTableBody, Expr,
    FromClause, JoinClause, JoinConstraint, JoinType, LimitClause, Literal, ResultColumn,
    SelectCore, SelectStmt, Stmt, TableConstraint, TableOrSubquery,
};
use crate::lexer::Token;
use logos::{Logos, Span};
use tracing::debug;
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType<'a> {
    Valid(Token<'a>),
    EndOfInput,
    Error,
}

pub struct Parser<'a> {
    pub input: &'a str,
    pub current_token: TokenType<'a>,
    lexer: logos::Lexer<'a, Token<'a>>,
    span: Span,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        debug!("Parser::new called with input: {:?}", input);
        let mut lexer = Token::lexer(input);
        let lexer_next_result = lexer.next();
        debug!("lexer.next() result: {:?}", lexer_next_result);
        let current_token = match lexer_next_result {
            Some(Ok(token)) => TokenType::Valid(token),
            Some(Err(_)) => TokenType::Error,
            None => TokenType::EndOfInput,
        };
        debug!("current_token: {:?}", current_token);
        let span = lexer.span();
        debug!("span after lexer init: {:?}", span);
        Parser {
            input,
            current_token,
            lexer,
            span,
        }
    }

    /// Parses a WITH clause (Common Table Expressions)
    fn parse_with_clause(&mut self) -> Result<crate::ast::WithClause<'static>, String> {
        // Consume WITH
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("WITH") {
                self.advance();
            } else {
                return Err("Expected WITH keyword".to_string());
            }
        } else {
            return Err("Expected WITH keyword".to_string());
        }

        // Optional RECURSIVE
        let mut recursive = false;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("RECURSIVE") {
                recursive = true;
                self.advance();
            }
        }

        let mut cte_tables = Vec::new();
        loop {
            // Parse CTE name
            let table_name = if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                let name = Box::leak(id.to_string().into_boxed_str());
                self.advance();
                name
            } else {
                return Err(format!("Expected CTE table name, found {:?}", self.current_token));
            };

            // Optional column list
            let column_names = if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                self.advance();
                let mut columns = Vec::new();
                loop {
                    if let TokenType::Valid(Token::IDENTIFIER(col_id)) = self.current_token {
                        columns.push(convert_str(col_id.to_string()));
                        self.advance();
                        if let TokenType::Valid(Token::COMMA) = &self.current_token {
                            self.advance();
                        } else if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                            self.advance();
                            break;
                        } else {
                            return Err(format!("Expected ',' or ')' in CTE column list, found {:?}", self.current_token));
                        }
                    } else {
                        return Err(format!("Expected column name in CTE column list, found {:?}", self.current_token));
                    }
                }
                Some(columns)
            } else {
                None
            };

            // Optional MATERIALIZED or NOT MATERIALIZED
            let mut materialized = None;
            if let TokenType::Valid(Token::IDENTIFIER(mat_kw)) = &self.current_token {
                if mat_kw.eq_ignore_ascii_case("MATERIALIZED") {
                    materialized = Some(true);
                    self.advance();
                } else if mat_kw.eq_ignore_ascii_case("NOT") {
                    // Check for NOT MATERIALIZED
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(mat2)) = &self.current_token {
                        if mat2.eq_ignore_ascii_case("MATERIALIZED") {
                            materialized = Some(false);
                            self.advance();
                        } else {
                            return Err(format!("Expected MATERIALIZED after NOT, found {:?}", self.current_token));
                        }
                    } else {
                        return Err(format!("Expected MATERIALIZED after NOT, found {:?}", self.current_token));
                    }
                }
            }

            // Expect AS
            if let TokenType::Valid(Token::IDENTIFIER(as_kw)) = &self.current_token {
                if as_kw.eq_ignore_ascii_case("AS") {
                    self.advance();
                } else {
                    return Err(format!("Expected AS after CTE name, found {:?}", self.current_token));
                }
            } else {
                return Err(format!("Expected AS after CTE name, found {:?}", self.current_token));
            }

            // Expect (
            if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                self.advance();
            } else {
                return Err(format!("Expected '(' before CTE subquery, found {:?}", self.current_token));
            }

            // Parse the subquery (must be SELECT)
            if let TokenType::Valid(Token::IDENTIFIER(sel_kw)) = &self.current_token {
                if sel_kw.eq_ignore_ascii_case("SELECT") {
                    self.advance();
                } else {
                    return Err(format!("Expected SELECT at start of CTE subquery, found {:?}", self.current_token));
                }
            } else {
                return Err(format!("Expected SELECT at start of CTE subquery, found {:?}", self.current_token));
            }
            let select_stmt = match self.parse_select(None) {
                Ok(Stmt::Select(stmt)) => stmt,
                Ok(_) => return Err("CTE subquery did not produce a SELECT statement".to_string()),
                Err(e) => return Err(format!("Error parsing CTE subquery: {}", e)),
            };

            // Expect ) after subquery
            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                self.advance();
            } else {
                return Err(format!("Expected ')' after CTE subquery, found {:?}", self.current_token));
            }

            cte_tables.push(crate::ast::CommonTableExpression {
                table_name,
                column_names,
                select_stmt,
                materialized,
            });

            // If comma, continue to next CTE
            if let TokenType::Valid(Token::COMMA) = &self.current_token {
                self.advance();
                continue;
            } else {
                break;
            }
        }

        Ok(crate::ast::WithClause {
            recursive,
            cte_tables,
        })
    }

    fn advance(&mut self) {
        debug!("advance() called, current_token: {:?}", self.current_token);
        let lexer_next_result = self.lexer.next();
        debug!("lexer.next() result: {:?}", lexer_next_result);
        self.current_token = match lexer_next_result {
            Some(Ok(token)) => TokenType::Valid(token),
            Some(Err(_)) => TokenType::Error,
            None => TokenType::EndOfInput,
        };
        self.span = self.lexer.span();
        debug!("new current_token: {:?}, new span: {:?}", self.current_token, self.span);
    }

    pub fn parse_statement(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_statement() called, current_token: {:?}", self.current_token);
        // --- WITH clause support ---
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("WITH") {
                let with_clause = self.parse_with_clause()?;
                // After WITH, expect SELECT/INSERT/UPDATE/DELETE
                if let TokenType::Valid(Token::IDENTIFIER(next)) = &self.current_token {
                    if next.eq_ignore_ascii_case("SELECT") {
                        self.advance();
                        return self.parse_select(Some(with_clause));
                    } else if next.eq_ignore_ascii_case("INSERT") {
                        self.advance();
                        return self.parse_insert(Some(with_clause));
                    } else if next.eq_ignore_ascii_case("UPDATE") {
                        self.advance();
                        return self.parse_update(Some(with_clause));
                    } else if next.eq_ignore_ascii_case("DELETE") {
                        self.advance();
                        return self.parse_delete(Some(with_clause));
                    } else {
                        return Err(format!("Expected SELECT, INSERT, UPDATE, or DELETE after WITH, found {:?}", self.current_token));
                    }
                } else {
                    return Err(format!("Expected statement after WITH clause, found {:?}", self.current_token));
                }
            }
        }
        // --- END WITH clause support ---
        match &self.current_token {
            TokenType::Valid(Token::IDENTIFIER(id)) => {
                if id.eq_ignore_ascii_case("SELECT") {
                    debug!("parse_statement: found SELECT");
                    self.advance();
                    self.parse_select(None)
                } else if id.eq_ignore_ascii_case("INSERT") {
                    debug!("parse_statement: found INSERT");
                    self.advance();
                    self.parse_insert(None)
                } else if id.eq_ignore_ascii_case("UPDATE") {
                    debug!("parse_statement: found UPDATE");
                    self.advance();
                    self.parse_update(None)
                } else if id.eq_ignore_ascii_case("DELETE") {
                    debug!("parse_statement: found DELETE");
                    self.advance();
                    self.parse_delete(None)
                } else if id.eq_ignore_ascii_case("CREATE") {
                    debug!("parse_statement: found CREATE");
                    self.advance();
                    self.parse_create()
                } else if id.eq_ignore_ascii_case("ALTER") {
                    debug!("parse_statement: found ALTER");
                    self.advance();
                    self.parse_alter()
                } else if id.eq_ignore_ascii_case("DROP") {
                    debug!("parse_statement: found DROP");
                    self.advance();
                    self.parse_drop()
                } else {
                    debug!("parse_statement: found expression, parsing as SELECT expr");
                    self.parse_expr().map(|expr| {
                        Stmt::Select(Box::new(SelectStmt {
                            with_clause: None,
                            compound_operator: None,
                            select_core: SelectCore {
                                distinct: false,
                                result_columns: vec![ResultColumn::Expr {
                                    expr: Box::new(expr),
                                    alias: None,
                                }],
                                from_clause: None,
                                where_clause: None,
                                group_by_clause: None,
                                having_clause: None,
                                window_clause: None,
                            },
                            order_by_clause: None,
                            limit_clause: None,
                        }))
                    })
                }
            }
            TokenType::Valid(Token::SELECT_DISTINCT) => {
                debug!("parse_statement: found SELECT DISTINCT");
                self.advance();
                self.parse_select_distinct()
            }
            TokenType::EndOfInput => {
                debug!("parse_statement: Unexpected end of input");
                Err("Unexpected end of input".to_string())
            },
            TokenType::Error => {
                debug!("parse_statement: Lexical error");
                Err("Lexical error".to_string())
            },
            _ => {
                debug!("parse_statement: Unexpected token: {:?}", self.current_token);
                Err(format!("Unexpected token: {:?}", self.current_token))
            },
        }
    }

    fn parse_select(&mut self, with_clause: Option<crate::ast::WithClause<'static>>) -> Result<Stmt<'static>, String> {
        debug!("parse_select() called, current_token: {:?}", self.current_token);
        // If entering with SELECT or WITH, advance past it
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("SELECT") || keyword.eq_ignore_ascii_case("WITH") {
                debug!("parse_select: advancing past {:?}", self.current_token);
                self.advance();
            }
        }
        let result_columns = self.parse_result_columns()?;
        debug!("parse_select: after parse_result_columns, current_token: {:?}", self.current_token);
        debug!("parse_select: result_columns: {:?}", result_columns);
        // Expect FROM
        match &self.current_token {
            TokenType::Valid(Token::IDENTIFIER(keyword))
                if keyword.eq_ignore_ascii_case("FROM") =>
            {
                self.advance();
            }
            TokenType::EndOfInput | TokenType::Valid(Token::SEMICOLON) => {
                debug!("parse_select: Expected FROM clause after result columns, found {:?}", self.current_token);
                return Err("Expected FROM clause after result columns".to_string());
            }
            TokenType::Valid(Token::IDENTIFIER(keyword)) => {
                debug!("parse_select: Unexpected clause '{}' after result columns, expected FROM", keyword);
                return Err(format!(
                    "Unexpected clause '{}' after result columns, expected FROM",
                    keyword
                ));
            }
            _ => {
                debug!("parse_select: Unexpected token after result columns: {:?}", self.current_token);
                return Err(format!(
                    "Unexpected token after result columns: {:?}",
                    self.current_token
                ));
            }
        }
        // Parse full FROM clause, including JOINs and aliases
        let from_clause = Some(self.parse_from_clause()?);
        debug!("parse_select: after parse_from_clause, current_token: {:?}", self.current_token);
        // Optional WHERE clause
        let mut where_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("WHERE") {
                debug!("parse_select: found WHERE");
                self.advance();
                where_clause = Some(Box::new(self.parse_expr()?));
                debug!("parse_select: after WHERE, current_token: {:?}", self.current_token);
            }
        }
        // Parse optional GROUP BY, HAVING, ORDER BY, LIMIT
        let mut group_by_clause = None;
        let mut having_clause = None;
        let mut order_by_clause = None;
        let mut limit_clause = None;
        loop {
            debug!("parse_select: current_token after WHERE: {:?}", self.current_token);
            match &self.current_token {
                TokenType::Valid(Token::GROUP_BY) => {
                    debug!("parse_select: found GROUP BY");
                    self.advance();
                    group_by_clause = Some(self.parse_group_by_clause()?);
                    debug!("parse_select: after GROUP BY, current_token: {:?}", self.current_token);
                }
                TokenType::Valid(Token::IDENTIFIER(keyword)) if keyword.eq_ignore_ascii_case("GROUP") => {
                    debug!("parse_select: found GROUP");
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(by_kw)) = &self.current_token {
                        if by_kw.eq_ignore_ascii_case("BY") {
                            debug!("parse_select: found GROUP BY");
                            self.advance();
                            group_by_clause = Some(self.parse_group_by_clause()?);
                            debug!("parse_select: after GROUP BY, current_token: {:?}", self.current_token);
                        } else {
                            return Err(format!(
                                "Expected BY after GROUP, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Expected BY after GROUP, found {:?}",
                            self.current_token
                        ));
                    }
                }
                TokenType::Valid(Token::IDENTIFIER(keyword)) if keyword.eq_ignore_ascii_case("HAVING") => {
                    debug!("parse_select: found HAVING");
                    self.advance();
                    having_clause = Some(Box::new(self.parse_expr()?));
                    debug!("parse_select: after HAVING, current_token: {:?}", self.current_token);
                }
                TokenType::Valid(Token::ORDER_BY) => {
                    debug!("parse_select: found ORDER BY");
                    self.advance();
                    order_by_clause = Some(crate::ast::OrderByClause {
                        terms: self.parse_order_by_clause()?,
                    });
                    debug!("parse_select: after ORDER BY, current_token: {:?}", self.current_token);
                }
                TokenType::Valid(Token::IDENTIFIER(keyword)) if keyword.eq_ignore_ascii_case("ORDER") => {
                    debug!("parse_select: found ORDER");
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(by_kw)) = &self.current_token {
                        if by_kw.eq_ignore_ascii_case("BY") {
                            debug!("parse_select: found ORDER BY");
                            self.advance();
                            order_by_clause = Some(crate::ast::OrderByClause {
                                terms: self.parse_order_by_clause()?,
                            });
                            debug!("parse_select: after ORDER BY, current_token: {:?}", self.current_token);
                        }
                    }
                }
                TokenType::Valid(Token::IDENTIFIER(keyword)) if keyword.eq_ignore_ascii_case("LIMIT") => {
                    debug!("parse_select: found LIMIT");
                    self.advance();
                    limit_clause = Some(self.parse_limit_clause()?);
                    debug!("parse_select: after LIMIT, current_token: {:?}", self.current_token);
                }
                _ => break,
            }
        }
        // Only allow end-of-input, semicolon, right paren, or next clause keyword after
        match &self.current_token {
            TokenType::EndOfInput => {}
            TokenType::Valid(Token::SEMICOLON) => {}
            TokenType::Valid(Token::RIGHT_PAREN) => {} // Allow closing paren for subqueries
            TokenType::Valid(Token::IDENTIFIER(keyword))
                if keyword.eq_ignore_ascii_case("LIMIT") => {}
            TokenType::Valid(Token::IDENTIFIER(keyword))
                if keyword.eq_ignore_ascii_case("GROUP") => {}
            TokenType::Valid(Token::IDENTIFIER(keyword))
                if keyword.eq_ignore_ascii_case("HAVING") => {}
            // Do not treat identifiers as an error here; table aliases are already handled by parse_table_or_subquery.
            // Only error if truly unexpected (should not reach here for valid SQL).
            TokenType::Valid(Token::IDENTIFIER(_)) => {
                // Accept and ignore any identifier here; valid aliases should have been consumed.
                // This avoids false errors for valid queries like: FROM users u
            }
            _ => {
                debug!("parse_select: Unexpected token after clause, current_token: {:?}", self.current_token);
                return Err(format!(
                    "Unexpected token after {}: {:?}",
                    if where_clause.is_some() {
                        "WHERE/ORDER/LIMIT"
                    } else {
                        "table name"
                    },
                    self.current_token
                ));
            }
        }
        let select_core = SelectCore {
            distinct: false,
            result_columns,
            from_clause,
            where_clause,
            group_by_clause,
            having_clause,
            window_clause: None,
        };
        let select_stmt = SelectStmt {
            with_clause,
            compound_operator: None,
            select_core,
            order_by_clause,
            limit_clause,
        };
        debug!("parse_select: returning select_stmt: {:#?}", select_stmt);
        Ok(Stmt::Select(Box::new(select_stmt)))
    }

    fn parse_select_distinct(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_select_distinct() called, current_token: {:?}", self.current_token);
        let result_columns = self.parse_result_columns()?;
        debug!("parse_select_distinct: after parse_result_columns, current_token: {:?}", self.current_token);
        let from_clause = if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("FROM") {
                debug!("parse_select_distinct: found FROM");
                self.advance();
                Some(self.parse_from_clause()?)
            } else {
                None
            }
        } else {
            None
        };
        let mut where_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("WHERE") {
                debug!("parse_select_distinct: found WHERE");
                self.advance();
                where_clause = Some(Box::new(self.parse_expr()?));
            }
        }
        let mut group_by_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("GROUP") {
                debug!("parse_select_distinct: found GROUP");
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(by_kw)) = &self.current_token {
                    if by_kw.eq_ignore_ascii_case("BY") {
                        debug!("parse_select_distinct: found GROUP BY");
                        self.advance();
                        group_by_clause = Some(self.parse_group_by_clause()?);
                    } else {
                        debug!("parse_select_distinct: Expected BY after GROUP, found {:?}", self.current_token);
                        return Err(format!(
                            "Expected BY after GROUP, found {:?}",
                            self.current_token
                        ));
                    }
                } else {
                    debug!("parse_select_distinct: Expected BY after GROUP, found {:?}", self.current_token);
                    return Err(format!(
                        "Expected BY after GROUP, found {:?}",
                        self.current_token
                    ));
                }
            }
        }
        let mut having_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("HAVING") {
                debug!("parse_select_distinct: found HAVING");
                self.advance();
                having_clause = Some(Box::new(self.parse_expr()?));
            }
        }
        let select_core = SelectCore {
            distinct: true,
            result_columns,
            from_clause,
            where_clause,
            group_by_clause,
            having_clause,
            window_clause: None,
        };
        let select_stmt = SelectStmt {
            with_clause: None,
            compound_operator: None,
            select_core,
            order_by_clause: None,
            limit_clause: None,
        };
        debug!("parse_select_distinct: returning select_stmt: {:#?}", select_stmt);
        Ok(Stmt::Select(Box::new(select_stmt)))
    }

    fn parse_result_columns(&mut self) -> Result<Vec<ResultColumn<'static>>, String> {
        debug!("parse_result_columns() called, current_token: {:?}", self.current_token);
        let mut columns = Vec::new();
        if let TokenType::Valid(Token::ASTERISK) = &self.current_token {
            debug!("parse_result_columns: found *");
            self.advance();
            columns.push(ResultColumn::AllColumns);
            debug!("parse_result_columns: returning AllColumns");
            return Ok(columns);
        }
        debug!("parse_result_columns: about to parse first result column, current_token: {:?}", self.current_token);
        columns.push(self.parse_result_column()?);
        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            debug!("parse_result_columns: found COMMA, current_token: {:?}", self.current_token);
            self.advance();
            debug!("parse_result_columns: after advance, current_token: {:?}", self.current_token);
            columns.push(self.parse_result_column()?);
        }
        debug!("parse_result_columns: returning columns: {:?}, current_token: {:?}", columns, self.current_token);
        Ok(columns)
    }

    fn parse_result_column(&mut self) -> Result<ResultColumn<'static>, String> {
        debug!("parse_result_column() called, current_token: {:?}", self.current_token);
        match &self.current_token {
            TokenType::Valid(Token::IDENTIFIER(id)) => {
                // Disallow clause keywords as result columns
                let clause_keywords = ["FROM", "WHERE", "GROUP", "HAVING", "ORDER", "LIMIT"];
                if clause_keywords
                    .iter()
                    .any(|&kw| id.eq_ignore_ascii_case(kw))
                {
                    debug!("parse_result_column: Unexpected clause keyword '{}' in result columns", id);
                    return Err(format!(
                        "Unexpected clause keyword '{}' in result columns",
                        id
                    ));
                }
                let id_str = Box::leak(id.to_string().into_boxed_str());
                self.advance();
                if let TokenType::Valid(Token::DOT) = &self.current_token {
                    self.advance();
                    if let TokenType::Valid(Token::ASTERISK) = &self.current_token {
                        self.advance();
                        return Ok(ResultColumn::TableAllColumns(id_str));
                    }
                    if let TokenType::Valid(Token::IDENTIFIER(col_id)) = &self.current_token {
                        let col_str = Box::leak(col_id.to_string().into_boxed_str());
                        self.advance();
                        let col_expr = crate::ast::Expr::Column {
                            schema_name: None,
                            table_name: Some(id_str),
                            column_name: col_str,
                        };
                        let mut expr = col_expr;
                        if let TokenType::Valid(Token::PLUS) = &self.current_token {
                            self.advance();
                            let right = self.parse_expr()?;
                            expr = crate::ast::Expr::BinaryOp {
                                left: Box::new(expr),
                                op: crate::ast::BinaryOperator::Plus,
                                right: Box::new(right),
                            };
                        } else if let TokenType::Valid(Token::MINUS) = &self.current_token {
                            self.advance();
                            let right = self.parse_expr()?;
                            expr = crate::ast::Expr::BinaryOp {
                                left: Box::new(expr),
                                op: crate::ast::BinaryOperator::Minus,
                                right: Box::new(right),
                            };
                        } else if let TokenType::Valid(Token::ASTERISK) = &self.current_token {
                            self.advance();
                            let right = self.parse_expr()?;
                            expr = crate::ast::Expr::BinaryOp {
                                left: Box::new(expr),
                                op: crate::ast::BinaryOperator::Multiply,
                                right: Box::new(right),
                            };
                        } else if let TokenType::Valid(Token::SLASH) = &self.current_token {
                            self.advance();
                            let right = self.parse_expr()?;
                            expr = crate::ast::Expr::BinaryOp {
                                left: Box::new(expr),
                                op: crate::ast::BinaryOperator::Divide,
                                right: Box::new(right),
                            };
                        }
                        let mut alias: Option<&'static str> = None;
                        if let TokenType::Valid(Token::IDENTIFIER(as_id)) = &self.current_token {
                            if as_id.eq_ignore_ascii_case("AS") {
                                self.advance();
                                if let TokenType::Valid(Token::IDENTIFIER(alias_id)) =
                                    &self.current_token
                                {
                                    alias = Some(Box::leak(alias_id.to_string().into_boxed_str()));
                                    self.advance();
                                } else {
                                    debug!("parse_result_column: Expected alias name after AS, found {:?}", self.current_token);
                                    return Err(format!(
                                        "Expected alias name after AS, found {:?}",
                                        self.current_token
                                    ));
                                }
                            }
                        }
                        let rc = ResultColumn::Expr {
                            expr: Box::new(expr),
                            alias: convert_option_str(alias),
                        };
                        debug!("parse_result_column: returning table column expr: {:?}", rc);
                        return Ok(rc);
                    }
                    debug!("parse_result_column: Expected * or column name after ., found {:?}", self.current_token);
                    return Err(format!(
                        "Expected * or column name after ., found {:?}",
                        self.current_token
                    ));
                }
                let expr = if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                    debug!("parse_result_column: Detected function call for identifier '{}', current_token: {:?}", id_str, self.current_token);
                    self.parse_function_call(&id_str.to_string())?
                } else {
                    debug!("parse_result_column: Treating '{}' as column, current_token: {:?}", id_str, self.current_token);
                    crate::ast::Expr::Column {
                        schema_name: None,
                        table_name: None,
                        column_name: id_str,
                    }
                };
                let mut final_expr = expr;
                if let TokenType::Valid(Token::PLUS) = &self.current_token {
                    self.advance();
                    let right = self.parse_expr()?;
                    final_expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(final_expr),
                        op: crate::ast::BinaryOperator::Plus,
                        right: Box::new(right),
                    };
                } else if let TokenType::Valid(Token::MINUS) = &self.current_token {
                    self.advance();
                    let right = self.parse_expr()?;
                    final_expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(final_expr),
                        op: crate::ast::BinaryOperator::Minus,
                        right: Box::new(right),
                    };
                } else if let TokenType::Valid(Token::ASTERISK) = &self.current_token {
                    self.advance();
                    let right = self.parse_expr()?;
                    final_expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(final_expr),
                        op: crate::ast::BinaryOperator::Multiply,
                        right: Box::new(right),
                    };
                } else if let TokenType::Valid(Token::SLASH) = &self.current_token {
                    self.advance();
                    let right = self.parse_expr()?;
                    final_expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(final_expr),
                        op: crate::ast::BinaryOperator::Divide,
                        right: Box::new(right),
                    };
                }
                let mut alias: Option<&'static str> = None;
                if let TokenType::Valid(Token::IDENTIFIER(as_id)) = &self.current_token {
                    if as_id.eq_ignore_ascii_case("AS") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(alias_id)) = &self.current_token {
                            alias = Some(Box::leak(alias_id.to_string().into_boxed_str()));
                            self.advance();
                        } else {
                            return Err(format!(
                                "Expected alias name after AS, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                let rc = ResultColumn::Expr {
                    expr: Box::new(final_expr),
                    alias: convert_option_str(alias),
                };
                debug!("parse_result_column: returning expr: {:?}", rc);
                Ok(rc)
            }
            _ => {
                debug!("parse_result_column: About to parse general expr for result column, current_token: {:?}", self.current_token);
                let expr = self.parse_expr()?;
                debug!("parse_result_column: Parsed expr: {:?}, current_token: {:?}", expr, self.current_token);
                let mut alias: Option<&'static str> = None;
                if let TokenType::Valid(Token::IDENTIFIER(as_id)) = &self.current_token {
                    if as_id.eq_ignore_ascii_case("AS") {
                        debug!("parse_result_column: Found AS for alias, current_token: {:?}", self.current_token);
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(alias_id)) = &self.current_token {
                            alias = Some(Box::leak(alias_id.to_string().into_boxed_str()));
                            debug!("parse_result_column: Found alias '{}', advancing", alias_id);
                            self.advance();
                        } else {
                            debug!("parse_result_column: Expected alias name after AS, found {:?}", self.current_token);
                            return Err(format!(
                                "Expected alias name after AS, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                let rc = ResultColumn::Expr {
                    expr: Box::new(expr),
                    alias: convert_option_str(alias),
                };
                debug!("parse_result_column: returning expr: {:?}, current_token: {:?}", rc, self.current_token);
                Ok(rc)
            }
        }
    }

    fn parse_from_clause(&mut self) -> Result<FromClause<'static>, String> {
        debug!("parse_from_clause() called, current_token: {:?}", self.current_token);
        let first_table = self.parse_table_or_subquery()?;
        let mut tables = vec![first_table];
        while let TokenType::Valid(Token::COMMA) = self.current_token {
            debug!("parse_from_clause: found COMMA, parsing next table");
            self.advance();
            let table = self.parse_table_or_subquery()?;
            tables.push(table);
        }
        let mut join_clauses = Vec::new();
        loop {
            if let TokenType::Valid(Token::IDENTIFIER(join_keyword)) = &self.current_token {
                if join_keyword.eq_ignore_ascii_case("JOIN") {
                    debug!("parse_from_clause: found JOIN");
                    self.advance();
                    let table = self.parse_table_or_subquery()?;
                    let constraint = self.parse_join_constraint()?;
                    join_clauses.push(JoinClause {
                        join_type: JoinType::Inner,
                        table_or_subquery: table,
                        constraint,
                    });
                } else if join_keyword.eq_ignore_ascii_case("LEFT") {
                    debug!("parse_from_clause: found LEFT");
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(next)) = &self.current_token {
                        if next.eq_ignore_ascii_case("JOIN") {
                            debug!("parse_from_clause: found LEFT JOIN");
                            self.advance();
                            let table = self.parse_table_or_subquery()?;
                            let constraint = self.parse_join_constraint()?;
                            join_clauses.push(JoinClause {
                                join_type: JoinType::Left,
                                table_or_subquery: table,
                                constraint,
                            });
                        } else {
                            debug!("parse_from_clause: Expected JOIN after LEFT, got {:?}", self.current_token);
                            return Err(format!(
                                "Expected JOIN after LEFT, got {:?}",
                                self.current_token
                            ));
                        }
                    } else if let TokenType::Valid(Token::LEFT_JOIN) = &self.current_token {
                        debug!("parse_from_clause: found LEFT_JOIN token");
                        self.advance();
                        let table = self.parse_table_or_subquery()?;
                        let constraint = self.parse_join_constraint()?;
                        join_clauses.push(JoinClause {
                            join_type: JoinType::Left,
                            table_or_subquery: table,
                            constraint,
                        });
                    } else {
                        debug!("parse_from_clause: Expected JOIN after LEFT, got {:?}", self.current_token);
                        return Err(format!(
                            "Expected JOIN after LEFT, got {:?}",
                            self.current_token
                        ));
                    }
                } else if join_keyword.eq_ignore_ascii_case("INNER") {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(next)) = &self.current_token {
                        if next.eq_ignore_ascii_case("JOIN") {
                            self.advance();
                            let table = self.parse_table_or_subquery()?;
                            let constraint = self.parse_join_constraint()?;
                            join_clauses.push(JoinClause {
                                join_type: JoinType::Inner,
                                table_or_subquery: table,
                                constraint,
                            });
                        } else {
                            return Err(format!(
                                "Expected JOIN after INNER, got {:?}",
                                self.current_token
                            ));
                        }
                    } else if let TokenType::Valid(Token::INNER_JOIN) = &self.current_token {
                        self.advance();
                        let table = self.parse_table_or_subquery()?;
                        let constraint = self.parse_join_constraint()?;
                        join_clauses.push(JoinClause {
                            join_type: JoinType::Inner,
                            table_or_subquery: table,
                            constraint,
                        });
                    } else {
                        return Err(format!(
                            "Expected JOIN after INNER, got {:?}",
                            self.current_token
                        ));
                    }
                } else if join_keyword.eq_ignore_ascii_case("CROSS") {
                    debug!("parse_from_clause: found CROSS");
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(next)) = &self.current_token {
                        if next.eq_ignore_ascii_case("JOIN") {
                            debug!("parse_from_clause: found CROSS JOIN");
                            self.advance();
                            let table = self.parse_table_or_subquery()?;
                            join_clauses.push(JoinClause {
                                join_type: JoinType::Cross,
                                table_or_subquery: table,
                                constraint: JoinConstraint::On(Box::new(Expr::Literal(
                                    Literal::Numeric("1"),
                                ))),
                            });
                        } else {
                            debug!("parse_from_clause: Expected JOIN after CROSS, got {:?}", self.current_token);
                            return Err(format!(
                                "Expected JOIN after CROSS, got {:?}",
                                self.current_token
                            ));
                        }
                    }
                } else {
                    break;
                }
            } else if let TokenType::Valid(Token::LEFT_JOIN) = &self.current_token {
                debug!("parse_from_clause: found LEFT_JOIN token");
                self.advance();
                let table = self.parse_table_or_subquery()?;
                let constraint = self.parse_join_constraint()?;
                join_clauses.push(JoinClause {
                    join_type: JoinType::Left,
                    table_or_subquery: table,
                    constraint,
                });
            } else if let TokenType::Valid(Token::INNER_JOIN) = &self.current_token {
                debug!("parse_from_clause: found INNER_JOIN token");
                self.advance();
                let table = self.parse_table_or_subquery()?;
                let constraint = self.parse_join_constraint()?;
                join_clauses.push(JoinClause {
                    join_type: JoinType::Inner,
                    table_or_subquery: table,
                    constraint,
                });
            } else {
                break;
            }
        }
        let fc = FromClause {
            tables,
            join_clauses,
        };
        debug!("parse_from_clause: returning FromClause: {:#?}", fc);
        Ok(fc)
    }

    fn parse_table_or_subquery(&mut self) -> Result<TableOrSubquery<'static>, String> {
        debug!("parse_table_or_subquery() called, current_token: {:?}", self.current_token);
        match &self.current_token {
            TokenType::Valid(Token::IDENTIFIER(id)) => {
                let id_str = Box::leak(id.to_string().into_boxed_str());
                self.advance();
                let (schema_name, table_name) =
                    if let TokenType::Valid(Token::DOT) = &self.current_token {
                        debug!("parse_table_or_subquery: found schema qualifier");
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(table_id)) = &self.current_token {
                            let table_id_copy = table_id.to_string();
                            self.advance();
                            (Some(id_str), Box::leak(table_id_copy.into_boxed_str()))
                        } else {
                            debug!("parse_table_or_subquery: Expected table name after schema qualifier, found {:?}", self.current_token);
                            return Err(format!(
                                "Expected table name after schema qualifier, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        (None, id_str)
                    };
                let mut alias: Option<&'static str> = None;
                if let TokenType::Valid(Token::IDENTIFIER(as_id)) = &self.current_token {
                    const CLAUSE_KEYWORDS: [&str; 5] =
                        ["WHERE", "GROUP", "HAVING", "ORDER", "LIMIT"];
                    if !CLAUSE_KEYWORDS
                        .iter()
                        .any(|&kw| as_id.eq_ignore_ascii_case(kw))
                    {
                        if as_id.eq_ignore_ascii_case("AS") {
                            debug!("parse_table_or_subquery: found AS for alias");
                            self.advance();
                            if let TokenType::Valid(Token::IDENTIFIER(alias_id)) =
                                &self.current_token
                            {
                                alias = Some(Box::leak(alias_id.to_string().into_boxed_str()));
                                self.advance();
                            } else {
                                debug!("parse_table_or_subquery: Expected alias after AS, found {:?}", self.current_token);
                                return Err(format!(
                                    "Expected alias after AS, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            debug!("parse_table_or_subquery: found alias without AS");
                            alias = Some(Box::leak(as_id.to_string().into_boxed_str()));
                            self.advance();
                        }
                    }
                }
                let mut indexed_by: Option<&'static str> = None;
                let mut not_indexed = false;
                if let TokenType::Valid(Token::IDENTIFIER(indexed_keyword)) = &self.current_token {
                    if indexed_keyword.eq_ignore_ascii_case("INDEXED") {
                        debug!("parse_table_or_subquery: found INDEXED");
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(by_keyword)) = &self.current_token
                        {
                            if by_keyword.eq_ignore_ascii_case("BY") {
                                debug!("parse_table_or_subquery: found INDEXED BY");
                                self.advance();
                                if let TokenType::Valid(Token::IDENTIFIER(index_name)) =
                                    &self.current_token
                                {
                                    indexed_by =
                                        Some(Box::leak(index_name.to_string().into_boxed_str()));
                                    self.advance();
                                } else {
                                    debug!("parse_table_or_subquery: Expected index name after INDEXED BY, found {:?}", self.current_token);
                                    return Err(format!(
                                        "Expected index name after INDEXED BY, found {:?}",
                                        self.current_token
                                    ));
                                }
                            } else {
                                debug!("parse_table_or_subquery: Expected BY after INDEXED, found {:?}", self.current_token);
                                return Err(format!(
                                    "Expected BY after INDEXED, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            debug!("parse_table_or_subquery: Expected BY after INDEXED, found {:?}", self.current_token);
                            return Err(format!(
                                "Expected BY after INDEXED, found {:?}",
                                self.current_token
                            ));
                        }
                    } else if indexed_keyword.eq_ignore_ascii_case("NOT") {
                        debug!("parse_table_or_subquery: found NOT INDEXED");
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(indexed_keyword)) =
                            &self.current_token
                        {
                            if indexed_keyword.eq_ignore_ascii_case("INDEXED") {
                                not_indexed = true;
                                self.advance();
                            } else {
                                debug!("parse_table_or_subquery: Expected INDEXED after NOT, found {:?}", self.current_token);
                                return Err(format!(
                                    "Expected INDEXED after NOT, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            debug!("parse_table_or_subquery: Expected INDEXED after NOT, found {:?}", self.current_token);
                            return Err(format!(
                                "Expected INDEXED after NOT, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                let t = TableOrSubquery::Table {
                    schema_name: convert_option_str(schema_name.as_deref()),
                    table_name,
                    alias: convert_option_str(alias),
                    indexed_by: convert_option_str(indexed_by),
                    not_indexed,
                };
                debug!("parse_table_or_subquery: returning Table: {:#?}", t);
                Ok(t)
            }
            TokenType::Valid(Token::LEFT_PAREN) => {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if id.eq_ignore_ascii_case("SELECT") {
                        self.advance();
                        let stmt = self.parse_select(None)?;
                        let select_stmt = if let Stmt::Select(box_select) = stmt {
                            *box_select
                        } else {
                            return Err("Expected SELECT statement".to_string());
                        };
                        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                            self.advance();
                            let mut alias: Option<&'static str> = None;
                            if let TokenType::Valid(Token::IDENTIFIER(as_id)) = &self.current_token
                            {
                                if as_id.eq_ignore_ascii_case("AS") {
                                    self.advance();
                                    if let TokenType::Valid(Token::IDENTIFIER(alias_id)) =
                                        &self.current_token
                                    {
                                        alias =
                                            Some(Box::leak(alias_id.to_string().into_boxed_str()));
                                        self.advance();
                                    } else {
                                        return Err(format!(
                                            "Expected alias after AS, found {:?}",
                                            self.current_token
                                        ));
                                    }
                                } else {
                                    alias = Some(convert_str(as_id.to_string()));
                                    self.advance();
                                }
                            }
                            let sq = TableOrSubquery::Subquery {
                                select_stmt: Box::new(select_stmt),
                                alias: convert_option_str(alias),
                            };
                            debug!("parse_table_or_subquery: returning Subquery: {:#?}", sq);
                            Ok(sq)
                        } else {
                            return Err(format!(
                                "Expected ) after subquery, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        let table_or_subquery = self.parse_table_or_subquery()?;
                        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                            self.advance();
                            debug!("parse_table_or_subquery: returning nested table_or_subquery");
                            Ok(table_or_subquery)
                        } else {
                            return Err(format!(
                                "Expected ) after table expression, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                } else {
                    debug!("parse_table_or_subquery: Expected table name or SELECT after (, found {:?}", self.current_token);
                    return Err(format!(
                        "Expected table name or SELECT after (, found {:?}",
                        self.current_token
                    ));
                }
            }
            _ => {
                debug!("parse_table_or_subquery: Expected table name or subquery, found {:?}", self.current_token);
                return Err(format!(
                    "Expected table name or subquery, found {:?}",
                    self.current_token
                ));
            }
        }
    }

    fn parse_order_by_clause(&mut self) -> Result<Vec<crate::ast::OrderingTerm<'static>>, String> {
        debug!("parse_order_by_clause() called, current_token: {:?}", self.current_token);
        let mut terms = Vec::new();
        terms.push(self.parse_ordering_term()?);
        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            debug!("parse_order_by_clause: found COMMA");
            self.advance();
            terms.push(self.parse_ordering_term()?);
        }
        debug!("parse_order_by_clause: returning terms: {:?}", terms);
        Ok(terms)
    }

    fn parse_ordering_term(&mut self) -> Result<crate::ast::OrderingTerm<'static>, String> {
        debug!("parse_ordering_term() called, current_token: {:?}", self.current_token);
        let expr = self.parse_expr()?;
        let mut asc_desc = None;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("ASC") {
                debug!("parse_ordering_term: found ASC");
                asc_desc = Some(crate::ast::AscDesc::Asc);
                self.advance();
            } else if id.eq_ignore_ascii_case("DESC") {
                debug!("parse_ordering_term: found DESC");
                asc_desc = Some(crate::ast::AscDesc::Desc);
                self.advance();
            }
        }
        let mut nulls = None;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("NULLS") {
                debug!("parse_ordering_term: found NULLS");
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(pos)) = &self.current_token {
                    if pos.eq_ignore_ascii_case("FIRST") {
                        debug!("parse_ordering_term: found NULLS FIRST");
                        nulls = Some(crate::ast::NullsOrder::First);
                        self.advance();
                    } else if pos.eq_ignore_ascii_case("LAST") {
                        debug!("parse_ordering_term: found NULLS LAST");
                        nulls = Some(crate::ast::NullsOrder::Last);
                        self.advance();
                    } else {
                        debug!("parse_ordering_term: Expected FIRST or LAST after NULLS, found {:?}", self.current_token);
                        return Err(format!(
                            "Expected FIRST or LAST after NULLS, found {:?}",
                            self.current_token
                        ));
                    }
                } else {
                    debug!("parse_ordering_term: Expected FIRST or LAST after NULLS, found {:?}", self.current_token);
                    return Err(format!(
                        "Expected FIRST or LAST after NULLS, found {:?}",
                        self.current_token
                    ));
                }
            }
        }
        let ot = crate::ast::OrderingTerm {
            expr: Box::new(expr),
            asc_desc,
            nulls,
        };
        debug!("parse_ordering_term: returning OrderingTerm: {:#?}", ot);
        Ok(ot)
    }

    fn parse_limit_clause(&mut self) -> Result<LimitClause<'static>, String> {
        debug!("parse_limit_clause() called, current_token: {:?}", self.current_token);
        let expr = self.parse_expr()?;
        let mut offset = None;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("OFFSET") {
                debug!("parse_limit_clause: found OFFSET");
                self.advance();
                let offset_expr = self.parse_expr()?;
                offset = Some(Box::new(offset_expr));
            }
        } else if let TokenType::Valid(Token::COMMA) = &self.current_token {
            debug!("parse_limit_clause: found COMMA");
            self.advance();
            let limit_expr = expr;
            offset = Some(Box::new(limit_expr));
            let lc = LimitClause {
                limit: Box::new(self.parse_expr()?),
                offset,
            };
            debug!("parse_limit_clause: returning LimitClause (with comma): {:#?}", lc);
            return Ok(lc);
        }
        let lc = LimitClause {
            limit: Box::new(expr),
            offset,
        };
        debug!("parse_limit_clause: returning LimitClause: {:#?}", lc);
        Ok(lc)
    }
    
    fn parse_group_by_clause(&mut self) -> Result<crate::ast::GroupByClause<'static>, String> {
        debug!("parse_group_by_clause: entry, current_token: {:?}", self.current_token);
        let mut exprs = Vec::new();
        let expr = self.parse_expr()?;
        debug!("parse_group_by_clause: first expr: {:#?}", expr);
        exprs.push(expr);
        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            debug!("parse_group_by_clause: found COMMA, advancing");
            self.advance();
            let expr = self.parse_expr()?;
            debug!("parse_group_by_clause: next expr: {:#?}", expr);
            exprs.push(expr);
        }
        debug!("parse_group_by_clause: returning GroupByClause: {:#?}", exprs);
        Ok(crate::ast::GroupByClause { exprs })
    }

    fn parse_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_expr: entry, current_token: {:?}", self.current_token);
        let result = self.parse_or_expr();
        debug!("parse_expr: result: {:#?}, current_token: {:?}", result, self.current_token);
        result
    }

    fn parse_or_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_or_expr: entry, current_token: {:?}", self.current_token);
        let mut expr = self.parse_and_expr()?;
        debug!("parse_or_expr: after parse_and_expr, expr: {:#?}, current_token: {:?}", expr, self.current_token);
        loop {
            if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                if !id.eq_ignore_ascii_case("OR") {
                    break;
                }
                debug!("parse_or_expr: found OR, advancing");
                self.advance();
                let right = self.parse_and_expr()?;
                debug!("parse_or_expr: right expr: {:#?}", right);
                expr = crate::ast::Expr::BinaryOp {
                    left: Box::new(expr),
                    op: crate::ast::BinaryOperator::Or,
                    right: Box::new(right),
                };
            } else {
                break;
            }
        }
        debug!("parse_or_expr: returning expr: {:#?}", expr);
        Ok(expr)
    }

    fn parse_and_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_and_expr: entry, current_token: {:?}", self.current_token);
        let mut expr = self.parse_comparison_expr()?;
        debug!("parse_and_expr: after parse_comparison_expr, expr: {:#?}, current_token: {:?}", expr, self.current_token);
        while let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if !id.eq_ignore_ascii_case("AND") {
                break;
            }
            debug!("parse_and_expr: found AND, advancing");
            self.advance();
            let right = self.parse_comparison_expr()?;
            debug!("parse_and_expr: right expr: {:#?}", right);
            expr = crate::ast::Expr::BinaryOp {
                left: Box::new(expr),
                op: crate::ast::BinaryOperator::And,
                right: Box::new(right),
            };
        }
        debug!("parse_and_expr: returning expr: {:#?}", expr);
        Ok(expr)
    }

    fn parse_comparison_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_comparison_expr: entry, current_token: {:?}", self.current_token);
        let mut left = self.parse_additive_expr()?;
        debug!("parse_comparison_expr: after parse_additive_expr, left: {:#?}, current_token: {:?}", left, self.current_token);
        loop {
            // println!("[DEBUG] Top of comparison loop, current_token: {:?}", self.current_token);

            match &self.current_token {
                TokenType::Valid(Token::EQUALS) => {
                    // println!("[DEBUG] Found EQUALS, advancing");
                    self.advance();
                    // println!("[DEBUG] After advance (EQUALS), current_token: {:?}", self.current_token);
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::Equals,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::NOT_EQUAL) => {
                    self.advance();
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::NotEquals,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::LESS_THAN) => {
                    self.advance();
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::LessThan,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::LESS_THAN_EQUAL) => {
                    self.advance();
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::LessThanOrEqual,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::GREATER_THAN) => {
                    self.advance();
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::GreaterThan,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::GREATER_THAN_EQUAL) => {
                    self.advance();
                    let right = self.parse_additive_expr()?;
                    left = crate::ast::Expr::BinaryOp {
                        left: Box::new(left),
                        op: crate::ast::BinaryOperator::GreaterThanOrEqual,
                        right: Box::new(right),
                    };
                    continue;
                }
                TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("LIKE") => {
                    self.advance();
                    let pattern = self.parse_additive_expr()?;
                    left = crate::ast::Expr::Like {
                        expr: Box::new(left),
                        pattern: Box::new(pattern),
                        escape: None,
                    };
                    continue;
                }
                TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("IN") => {
                    // println!("[DEBUG] Found IN, advancing");
                    self.advance();
                    // println!("[DEBUG] After advance (IN), current_token: {:?}", self.current_token);
                    match &self.current_token {
                        TokenType::Valid(Token::LEFT_PAREN) => {
                            self.advance();
                            // println!("[DEBUG] After advance (LEFT_PAREN for IN), current_token: {:?}", self.current_token);
                            // Check for subquery: IN (SELECT ...)
                            if let TokenType::Valid(Token::IDENTIFIER(sel_kw)) = &self.current_token
                            {
                                if sel_kw.eq_ignore_ascii_case("SELECT") {
                                    self.advance();
                                    // println!("[DEBUG] After advance (SELECT in IN), current_token: {:?}", self.current_token);
                                    let subquery_stmt = self.parse_select(None)?;
                                    // println!("[DEBUG] After parse_select (IN subquery), current_token: {:?}", self.current_token);
                                    if let TokenType::Valid(Token::RIGHT_PAREN) =
                                        &self.current_token
                                    {
                                        self.advance();
                                        // println!("[DEBUG] After advance (RIGHT_PAREN after IN subquery), current_token: {:?}", self.current_token);
                                        left = crate::ast::Expr::InSelect {
                                            expr: Box::new(left),
                                            not: false,
                                            select: match subquery_stmt {
                                                crate::ast::Stmt::Select(box select_stmt) => {
                                                    Box::new(select_stmt)
                                                }
                                                _ => {
                                                    return Err(
                                                        "Expected SELECT statement in subquery"
                                                            .to_string(),
                                                    );
                                                }
                                            },
                                        };
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Expected ) after IN (SELECT ...) subquery, found {:?}",
                                            self.current_token
                                        ));
                                    }
                                }
                            }
                            // Otherwise, parse as value list
                            let mut list = Vec::new();
                            // println!("[DEBUG] IN value list, current_token: {:?}", self.current_token);
                            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                                self.advance();
                                // println!("[DEBUG] After advance (RIGHT_PAREN after IN value list), current_token: {:?}", self.current_token);
                                left = crate::ast::Expr::InList {
                                    expr: Box::new(left),
                                    not: false,
                                    list,
                                };
                                continue;
                            }
                            list.push(self.parse_expr()?);
                            // println!("[DEBUG] After first expr in IN value list, current_token: {:?}", self.current_token);
                            while let TokenType::Valid(Token::COMMA) = &self.current_token {
                                self.advance();
                                // println!("[DEBUG] After advance (COMMA in IN value list), current_token: {:?}", self.current_token);
                                list.push(self.parse_expr()?);
                            }
                            match &self.current_token {
                                TokenType::Valid(Token::RIGHT_PAREN) => {
                                    self.advance();
                                    // println!("[DEBUG] After advance (RIGHT_PAREN after full IN value list), current_token: {:?}", self.current_token);
                                    left = crate::ast::Expr::InList {
                                        expr: Box::new(left),
                                        not: false,
                                        list,
                                    };
                                    continue;
                                }
                                _ => {
                                    return Err(format!(
                                        "Expected ) after IN expression list, found {:?}",
                                        self.current_token
                                    ));
                                }
                            }
                        }
                        _ => {
                            return Err(format!(
                                "Expected ( after IN operator, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                _ => break,
            }
        }
        debug!("parse_comparison_expr: returning expr: {:#?}", left);
        Ok(left)
    }

    fn parse_additive_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_additive_expr: entry, current_token: {:?}", self.current_token);
        let mut expr = self.parse_multiplicative_expr()?;
        debug!("parse_additive_expr: after parse_multiplicative_expr, expr: {:#?}, current_token: {:?}", expr, self.current_token);
        loop {
            match &self.current_token {
                TokenType::Valid(Token::PLUS) => {
                    self.advance();
                    let right = self.parse_multiplicative_expr()?;
                    expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(expr),
                        op: crate::ast::BinaryOperator::Plus,
                        right: Box::new(right),
                    };
                }
                TokenType::Valid(Token::MINUS) => {
                    self.advance();
                    let right = self.parse_multiplicative_expr()?;
                    expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(expr),
                        op: crate::ast::BinaryOperator::Minus,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_multiplicative_expr: entry, current_token: {:?}", self.current_token);
        let mut expr = self.parse_unary_expr()?;
        debug!("parse_multiplicative_expr: after parse_unary_expr, expr: {:#?}, current_token: {:?}", expr, self.current_token);
        loop {
            match &self.current_token {
                TokenType::Valid(Token::ASTERISK) => {
                    self.advance();
                    let right = self.parse_unary_expr()?;
                    expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(expr),
                        op: crate::ast::BinaryOperator::Multiply,
                        right: Box::new(right),
                    };
                }
                TokenType::Valid(Token::SLASH) => {
                    self.advance();
                    let right = self.parse_unary_expr()?;
                    expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(expr),
                        op: crate::ast::BinaryOperator::Divide,
                        right: Box::new(right),
                    };
                }
                TokenType::Valid(Token::PERCENT) => {
                    self.advance();
                    let right = self.parse_unary_expr()?;
                    expr = crate::ast::Expr::BinaryOp {
                        left: Box::new(expr),
                        op: crate::ast::BinaryOperator::Modulo,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_unary_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_unary_expr: entry, current_token: {:?}", self.current_token);
        match &self.current_token {
            TokenType::Valid(Token::MINUS) => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(crate::ast::Expr::UnaryOp {
                    op: crate::ast::UnaryOperator::Negative,
                    expr: Box::new(expr),
                })
            }
            TokenType::Valid(Token::PLUS) => {
                self.advance();
                self.parse_unary_expr()
            }
            TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("NOT") => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(crate::ast::Expr::UnaryOp {
                    op: crate::ast::UnaryOperator::Not,
                    expr: Box::new(expr),
                })
            }
            _ => {
                debug!("parse_unary_expr: delegating to parse_primary_expr");
                self.parse_primary_expr()
            }
        }
    }

    fn parse_primary_expr(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_primary_expr: entry, current_token: {:?}", self.current_token);
        match &self.current_token {
            TokenType::Valid(Token::NUMBER(num)) => {
                let num_str = num.to_string();
                self.advance();
                Ok(crate::ast::Expr::Literal(crate::ast::Literal::Numeric(
                    Box::leak(num_str.into_boxed_str()),
                )))
            }
            TokenType::Valid(Token::STRING_LITERAL(s)) => {
                let string_value = s.to_string();
                self.advance();
                Ok(crate::ast::Expr::Literal(crate::ast::Literal::String(
                    Box::leak(string_value.into_boxed_str()),
                )))
            }
            TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("NULL") => {
                self.advance();
                Ok(crate::ast::Expr::Literal(crate::ast::Literal::Null))
            }
            TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("TRUE") => {
                self.advance();
                Ok(crate::ast::Expr::Literal(crate::ast::Literal::Keyword(
                    "TRUE",
                )))
            }
            TokenType::Valid(Token::IDENTIFIER(id)) if id.eq_ignore_ascii_case("FALSE") => {
                self.advance();
                Ok(crate::ast::Expr::Literal(crate::ast::Literal::Keyword(
                    "FALSE",
                )))
            }
            TokenType::Valid(Token::IDENTIFIER(id)) => {
                let id_str = id.to_string();
                // Lambda: single parameter => ...
                if let TokenType::Valid(Token::MINUS) = &self.current_token {
                    if let Some(Ok(Token::GREATER_THAN)) = self.lexer.clone().next() {
                        // Rewind to before MINUS and call parse_lambda
                        self.lexer = Token::lexer(&self.input[self.span.start..]);
                        self.current_token = TokenType::Valid(Token::IDENTIFIER(Box::leak(
                            id_str.clone().into_boxed_str(),
                        )));
                        return self.parse_lambda();
                    }
                }
                self.advance();
                if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                    // Lambda: (param, ...) => ...
                    let mut lookahead = self.lexer.clone();
                    let mut paren_count = 1;
                    let mut found_arrow = false;
                    while let Some(tok) = lookahead.next() {
                        match tok {
                            Ok(Token::LEFT_PAREN) => paren_count += 1,
                            Ok(Token::RIGHT_PAREN) => {
                                paren_count -= 1;
                                if paren_count == 0 {
                                    if let Some(Ok(Token::MINUS)) = lookahead.next() {
                                        if let Some(Ok(Token::GREATER_THAN)) = lookahead.next() {
                                            found_arrow = true;
                                        }
                                    }
                                }
                                break;
                            }
                            _ => {}
                        }
                    }
                    if found_arrow {
                        // Rewind and call parse_lambda
                        self.lexer = Token::lexer(&self.input[self.span.start..]);
                        self.current_token = TokenType::Valid(Token::IDENTIFIER(Box::leak(
                            id_str.clone().into_boxed_str(),
                        )));
                        return self.parse_lambda();
                    }
                    return self.parse_function_call(&id_str);
                }
                if let TokenType::Valid(Token::DOT) = &self.current_token {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(col)) = &self.current_token {
                        let col_str = col.to_string();
                        self.advance();
                        return Ok(crate::ast::Expr::Column {
                            schema_name: None,
                            table_name: Some(Box::leak(id_str.into_boxed_str())),
                            column_name: Box::leak(col_str.into_boxed_str()),
                        });
                    } else {
                        return Err(format!(
                            "Expected column name after ., found {:?}",
                            self.current_token
                        ));
                    }
                }
                Ok(crate::ast::Expr::Column {
                    schema_name: None,
                    table_name: None,
                    column_name: Box::leak(id_str.into_boxed_str()),
                })
            }
            TokenType::Valid(Token::FUNCTION((name, args))) => {
                let function_name = Box::leak(name.to_string().into_boxed_str());
                let args_copy = args.to_string();
                self.advance();
                let arg_expr = if args_copy.trim().is_empty() {
                    vec![]
                } else {
                    vec![crate::ast::Expr::Column {
                        schema_name: None,
                        table_name: None,
                        column_name: Box::leak(args_copy.into_boxed_str()),
                    }]
                };
                let mut over_clause = None;
                if let TokenType::Valid(Token::IDENTIFIER(over_id)) = &self.current_token {
                    if over_id.eq_ignore_ascii_case("OVER") {
                        self.advance();
                        let window_spec = self.parse_window_spec()?;
                        over_clause = Some(Box::new(window_spec));
                    }
                }
                Ok(crate::ast::Expr::FunctionCall {
                    name: function_name,
                    args: arg_expr,
                    filter_clause: None,
                    over_clause,
                })
            }
            TokenType::Valid(Token::LEFT_PAREN) => {
                debug!("parse_primary_expr: found LEFT_PAREN, advancing");
                self.advance();
                debug!("parse_primary_expr: after advance, current_token: {:?}", self.current_token);
                // Check for subquery: (SELECT ...)
                if let TokenType::Valid(Token::IDENTIFIER(kw)) = &self.current_token {
                    debug!("parse_primary_expr: after LEFT_PAREN, found IDENTIFIER: {}", kw);
                    if kw.eq_ignore_ascii_case("SELECT") || kw.eq_ignore_ascii_case("WITH") {
                        debug!("parse_primary_expr: Detected start of subquery, calling parse_select with current_token: {:?}", self.current_token);
                        // DO NOT advance past SELECT/WITH, let parse_select handle it
                        let select_stmt = match self.parse_select(None) {
                            Ok(crate::ast::Stmt::Select(stmt)) => stmt,
                            Ok(_) => return Err("Subquery did not produce a SELECT statement".to_string()),
                            Err(e) => return Err(format!("Error parsing subquery: {}", e)),
                        };
                        debug!("parse_primary_expr: after parse_select for subquery, current_token: {:?}", self.current_token);
                        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                            debug!("parse_primary_expr: found RIGHT_PAREN after subquery");
                            self.advance();
                            return Ok(crate::ast::Expr::Subquery(select_stmt));
                        } else {
                            debug!("parse_primary_expr: Expected ) after subquery, found {:?}", self.current_token);
                            return Err(format!(
                                "Expected ) after subquery, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                // Otherwise, parse as parenthesized expression
                let expr = self.parse_expr()?;
                match &self.current_token {
                    TokenType::Valid(Token::RIGHT_PAREN) => {
                        self.advance();
                        Ok(expr)
                    }
                    _ => Err(format!(
                        "Expected ) after expression, found {:?}",
                        self.current_token
                    )),
                }
            }
            TokenType::Valid(Token::ASTERISK) => {
                self.advance();
                Ok(crate::ast::Expr::FunctionCall {
                    name: "*",
                    args: vec![],
                    filter_clause: None,
                    over_clause: None,
                })
            }
            _ => {
                debug!("parse_primary_expr: unexpected token: {:?}", self.current_token);
                Err(format!(
                    "Unexpected token in expression: {:?}",
                    self.current_token
                ))
            },
        }
        // Optionally, log successful parse result here if desired
    }

    fn parse_join_constraint(&mut self) -> Result<JoinConstraint<'static>, String> {
        debug!("parse_join_constraint: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(on_keyword)) = &self.current_token {
            if on_keyword.eq_ignore_ascii_case("ON") {
                self.advance();
                let expr = self.parse_expr()?;
                return Ok(JoinConstraint::On(Box::new(expr)));
            } else if on_keyword.eq_ignore_ascii_case("USING") {
                self.advance();
                if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                    self.advance();
                    let mut cols = Vec::new();
                    if let TokenType::Valid(Token::IDENTIFIER(col)) = &self.current_token {
                        cols.push(Box::leak(col.to_string().into_boxed_str()));
                        self.advance();
                        while let TokenType::Valid(Token::COMMA) = &self.current_token {
                            self.advance();
                            if let TokenType::Valid(Token::IDENTIFIER(col)) = &self.current_token {
                                cols.push(Box::leak(col.to_string().into_boxed_str()));
                                self.advance();
                            } else {
                                return Err(format!(
                                    "Expected column name after comma in USING clause, found {:?}",
                                    self.current_token
                                ));
                            }
                        }
                    } else {
                        return Err(format!(
                            "Expected column name in USING clause, found {:?}",
                            self.current_token
                        ));
                    }
                    if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                        self.advance();
                        return Ok(JoinConstraint::Using(convert_mut_str_vec(cols)));
                    } else {
                        return Err(format!(
                            "Expected ) after USING columns, found {:?}",
                            self.current_token
                        ));
                    }
                }
            }
        }
        debug!("parse_join_constraint: error, expected ON or USING after join, found {:?}", self.current_token);
        Err(format!(
            "Expected ON or USING after join, found {:?}",
            self.current_token
        ))
    }
    
    fn parse_update(&mut self, with_clause: Option<crate::ast::WithClause<'static>>) -> Result<Stmt<'static>, String> {
        debug!("parse_update: entry, current_token: {:?}", self.current_token);
        let table_name = if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let name = id.to_string().into_static();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected table name after UPDATE, found {:?}",
                self.current_token
            ));
        };
        if let TokenType::Valid(Token::IDENTIFIER(set_keyword)) = &self.current_token {
            if !set_keyword.eq_ignore_ascii_case("SET") {
                return Err(format!(
                    "Expected SET after table name, found {:?}",
                    self.current_token
                ));
            }
            self.advance();
        } else {
            return Err(format!(
                "Expected SET after table name, found {:?}",
                self.current_token
            ));
        }
        let mut set_clauses = Vec::new();
        set_clauses.push(self.parse_set_clause()?);
        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            self.advance();
            set_clauses.push(self.parse_set_clause()?);
        }
        let mut where_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("WHERE") {
                self.advance();
                where_clause = Some(Box::new(self.parse_expr()?));
            }
        }
        let qualified_table_name = crate::ast::QualifiedTableName {
            schema_name: None,
            table_name,
            alias: None,
            indexed_by: None,
            not_indexed: false,
        };
        let stmt = Stmt::Update {
            with_clause: None,
            or_conflict: None,
            qualified_table_name,
            set_clauses,
            from_clause: None,
            where_clause,
            returning_clause: None,
            order_by_clause: None,
            limit_clause: None,
        };
        debug!("parse_update: returning stmt: {:#?}", stmt);
        Ok(stmt)
    }

    fn parse_set_clause(&mut self) -> Result<crate::ast::SetClause<'static>, String> {
        debug!("parse_set_clause: entry, current_token: {:?}", self.current_token);
        let column_name = if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let name = id.to_string().into_static();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected column name in SET clause, found {:?}",
                self.current_token
            ));
        };
        if let TokenType::Valid(Token::EQUALS) = &self.current_token {
            self.advance();
        } else {
            return Err(format!(
                "Expected = after column name in SET clause, found {:?}",
                self.current_token
            ));
        }
        let expr = self.parse_expr()?;
        let set_clause = crate::ast::SetClause {
            column_name,
            expr: Box::new(expr),
        };
        debug!("parse_set_clause: returning set_clause: {:#?}", set_clause);
        Ok(set_clause)
    }

    fn parse_delete(&mut self, with_clause: Option<crate::ast::WithClause<'static>>) -> Result<Stmt<'static>, String> {
        debug!("parse_delete: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if !keyword.eq_ignore_ascii_case("FROM") {
                return Err(format!(
                    "Expected FROM after DELETE, found {:?}",
                    self.current_token
                ));
            }
            self.advance();
        } else {
            return Err(format!(
                "Expected FROM after DELETE, found {:?}",
                self.current_token
            ));
        }
        let table_name = if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let name = id.to_string().into_static();
            self.advance();
            name
        } else {
            return Err(format!(
                "Expected table name after FROM, found {:?}",
                self.current_token
            ));
        };
        let mut where_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(keyword)) = &self.current_token {
            if keyword.eq_ignore_ascii_case("WHERE") {
                self.advance();
                where_clause = Some(Box::new(self.parse_expr()?));
            }
        }
        let qualified_table_name = crate::ast::QualifiedTableName {
            schema_name: None,
            table_name,
            alias: None,
            indexed_by: None,
            not_indexed: false,
        };
        let stmt = Stmt::Delete {
            with_clause: None,
            qualified_table_name,
            where_clause,
            returning_clause: None,
            order_by_clause: None,
            limit_clause: None,
        };
        debug!("parse_delete: returning stmt: {:#?}", stmt);
        Ok(stmt)
    }

    fn parse_window_spec(&mut self) -> Result<crate::ast::WindowSpec<'static>, String> {
        debug!("parse_window_spec: entry, current_token: {:?}", self.current_token);
        let mut window_name: Option<&'static str> = None;
        let mut partition_by = None;
        let mut order_by = None;
        let mut frame_spec = None;
        match &self.current_token {
            TokenType::Valid(Token::IDENTIFIER(id)) => {
                window_name = Some(Box::leak(id.to_string().into_boxed_str()));
                self.advance();
            }
            TokenType::Valid(Token::LEFT_PAREN) => {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if !id.eq_ignore_ascii_case("PARTITION") && !id.eq_ignore_ascii_case("ORDER") {
                        window_name = Some(Box::leak(id.to_string().into_boxed_str()));
                        self.advance();
                    }
                }
                if let TokenType::Valid(Token::IDENTIFIER(partition_keyword)) = &self.current_token
                {
                    if partition_keyword.eq_ignore_ascii_case("PARTITION") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(by_keyword)) = &self.current_token
                        {
                            if by_keyword.eq_ignore_ascii_case("BY") {
                                self.advance();
                                let mut partition_exprs = Vec::new();
                                partition_exprs.push(self.parse_expr()?);
                                while let TokenType::Valid(Token::COMMA) = &self.current_token {
                                    self.advance();
                                    partition_exprs.push(self.parse_expr()?);
                                }
                                partition_by = Some(partition_exprs);
                            } else {
                                return Err(format!(
                                    "Expected BY after PARTITION, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected BY after PARTITION, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }
                if let TokenType::Valid(Token::IDENTIFIER(order_keyword)) = &self.current_token {
                    if order_keyword.eq_ignore_ascii_case("ORDER") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(by_keyword)) = &self.current_token
                        {
                            if by_keyword.eq_ignore_ascii_case("BY") {
                                self.advance();
                                let terms = self.parse_order_by_clause()?;
                                order_by = Some(crate::ast::OrderByClause { terms });
                            } else {
                                return Err(format!(
                                    "Expected BY after ORDER, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected BY after ORDER, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                } else if let TokenType::Valid(Token::ORDER_BY) = &self.current_token {
                    self.advance();
                    let terms = self.parse_order_by_clause()?;
                    order_by = Some(crate::ast::OrderByClause { terms });
                }
                if let TokenType::Valid(Token::IDENTIFIER(frame_type_keyword)) = &self.current_token
                {
                    if frame_type_keyword.eq_ignore_ascii_case("ROWS")
                        || frame_type_keyword.eq_ignore_ascii_case("RANGE")
                        || frame_type_keyword.eq_ignore_ascii_case("GROUPS")
                    {
                        let frame_type = match frame_type_keyword.to_uppercase().as_str() {
                            "ROWS" => crate::ast::FrameType::Rows,
                            "RANGE" => crate::ast::FrameType::Range,
                            "GROUPS" => crate::ast::FrameType::Groups,
                            _ => unreachable!(),
                        };
                        self.advance();
                        let frame_start;
                        let mut frame_end = None;
                        let between_clause =
                            if let TokenType::Valid(Token::IDENTIFIER(between_keyword)) =
                                &self.current_token
                            {
                                if between_keyword.eq_ignore_ascii_case("BETWEEN") {
                                    self.advance();
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            };
                        frame_start = self.parse_frame_bound()?;
                        if between_clause {
                            if let TokenType::Valid(Token::IDENTIFIER(and_keyword)) =
                                &self.current_token
                            {
                                if and_keyword.eq_ignore_ascii_case("AND") {
                                    self.advance();
                                    frame_end = Some(self.parse_frame_bound()?);
                                } else {
                                    return Err(format!(
                                        "Expected AND after BETWEEN, found {:?}",
                                        self.current_token
                                    ));
                                }
                            } else {
                                return Err(format!(
                                    "Expected AND after BETWEEN, found {:?}",
                                    self.current_token
                                ));
                            }
                        }
                        let mut exclude = None;
                        if let TokenType::Valid(Token::IDENTIFIER(exclude_keyword)) =
                            &self.current_token
                        {
                            if exclude_keyword.eq_ignore_ascii_case("EXCLUDE") {
                                self.advance();
                                if let TokenType::Valid(Token::IDENTIFIER(exclude_type)) =
                                    &self.current_token
                                {
                                    exclude = Some(match exclude_type.to_uppercase().as_str() {
                                        "NO" => {
                                            self.advance();
                                            if let TokenType::Valid(Token::IDENTIFIER(others)) =
                                                &self.current_token
                                            {
                                                if others.eq_ignore_ascii_case("OTHERS") {
                                                    self.advance();
                                                    crate::ast::FrameExclude::NoOthers
                                                } else {
                                                    return Err(format!(
                                                        "Expected OTHERS after NO, found {:?}",
                                                        self.current_token
                                                    ));
                                                }
                                            } else {
                                                return Err(format!(
                                                    "Expected OTHERS after NO, found {:?}",
                                                    self.current_token
                                                ));
                                            }
                                        }
                                        "CURRENT" => {
                                            self.advance();
                                            if let TokenType::Valid(Token::IDENTIFIER(row)) =
                                                &self.current_token
                                            {
                                                if row.eq_ignore_ascii_case("ROW") {
                                                    self.advance();
                                                    crate::ast::FrameExclude::CurrentRow
                                                } else {
                                                    return Err(format!(
                                                        "Expected ROW after CURRENT, found {:?}",
                                                        self.current_token
                                                    ));
                                                }
                                            } else {
                                                return Err(format!(
                                                    "Expected ROW after CURRENT, found {:?}",
                                                    self.current_token
                                                ));
                                            }
                                        }
                                        "GROUP" => {
                                            self.advance();
                                            crate::ast::FrameExclude::Group
                                        }
                                        "TIES" => {
                                            self.advance();
                                            crate::ast::FrameExclude::Ties
                                        }
                                        _ => {
                                            return Err(format!(
                                                "Expected NO OTHERS, CURRENT ROW, GROUP, or TIES after EXCLUDE, found {:?}",
                                                self.current_token
                                            ));
                                        }
                                    });
                                } else {
                                    return Err(format!(
                                        "Expected EXCLUDE type, found {:?}",
                                        self.current_token
                                    ));
                                }
                            }
                        }
                        frame_spec = Some(crate::ast::FrameSpec {
                            frame_type,
                            frame_start,
                            frame_end,
                            exclude,
                        });
                    }
                }
                if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                    self.advance();
                } else {
                    return Err(format!(
                        "Expected ) to close window specification, found {:?}",
                        self.current_token
                    ));
                }
            }
            _ => {
                return Err(format!(
                    "Expected window name or window specification, found {:?}",
                    self.current_token
                ));
            }
        }
        let spec = crate::ast::WindowSpec {
            window_name: convert_option_str(window_name),
            partition_by,
            order_by,
            frame_spec,
        };
        debug!("parse_window_spec: returning WindowSpec: {:#?}", spec);
        Ok(spec)
    }

    fn parse_frame_bound(&mut self) -> Result<crate::ast::FrameBound<'static>, String> {
        debug!("parse_frame_bound: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(bound_type)) = &self.current_token {
            match bound_type.to_uppercase().as_str() {
                "UNBOUNDED" => {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(direction)) = &self.current_token {
                        match direction.to_uppercase().as_str() {
                            "PRECEDING" => {
                                self.advance();
                                debug!("parse_frame_bound: found UNBOUNDED PRECEDING");
                                Ok(crate::ast::FrameBound::UnboundedPreceding)
                            }
                            "FOLLOWING" => {
                                self.advance();
                                debug!("parse_frame_bound: found UNBOUNDED FOLLOWING");
                                Ok(crate::ast::FrameBound::UnboundedFollowing)
                            }
                            _ => Err(format!(
                                "Expected PRECEDING or FOLLOWING after UNBOUNDED, found {:?}",
                                self.current_token
                            )),
                        }
                    } else {
                        Err(format!(
                            "Expected PRECEDING or FOLLOWING after UNBOUNDED, found {:?}",
                            self.current_token
                        ))
                    }
                }
                "CURRENT" => {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(row)) = &self.current_token {
                        if row.eq_ignore_ascii_case("ROW") {
                            self.advance();
                            debug!("parse_frame_bound: found CURRENT ROW");
                            Ok(crate::ast::FrameBound::CurrentRow)
                        } else {
                            Err(format!(
                                "Expected ROW after CURRENT, found {:?}",
                                self.current_token
                            ))
                        }
                    } else {
                        Err(format!(
                            "Expected ROW after CURRENT, found {:?}",
                            self.current_token
                        ))
                    }
                }
                _ => {
                    let expr = self.parse_expr()?;
                    if let TokenType::Valid(Token::IDENTIFIER(direction)) = &self.current_token {
                        match direction.to_uppercase().as_str() {
                            "PRECEDING" => {
                                self.advance();
                                debug!("parse_frame_bound: found expr PRECEDING");
                                Ok(crate::ast::FrameBound::Preceding(Box::new(expr)))
                            }
                            "FOLLOWING" => {
                                self.advance();
                                debug!("parse_frame_bound: found expr FOLLOWING");
                                Ok(crate::ast::FrameBound::Following(Box::new(expr)))
                            }
                            _ => Err(format!(
                                "Expected PRECEDING or FOLLOWING after expression in frame bound, found {:?}",
                                self.current_token
                            )),
                        }
                    } else {
                        Err(format!(
                            "Expected PRECEDING or FOLLOWING after expression in frame bound, found {:?}",
                            self.current_token
                        ))
                    }
                }
            }
        } else {
            let expr = self.parse_expr()?;
            if let TokenType::Valid(Token::IDENTIFIER(direction)) = &self.current_token {
                match direction.to_uppercase().as_str() {
                    "PRECEDING" => {
                        self.advance();
                        debug!("parse_frame_bound: found expr PRECEDING (no identifier)");
                        Ok(crate::ast::FrameBound::Preceding(Box::new(expr)))
                    }
                    "FOLLOWING" => {
                        self.advance();
                        debug!("parse_frame_bound: found expr FOLLOWING (no identifier)");
                        Ok(crate::ast::FrameBound::Following(Box::new(expr)))
                    }
                    _ => Err(format!(
                        "Expected PRECEDING or FOLLOWING after expression in frame bound, found {:?}",
                        self.current_token
                    )),
                }
            } else {
                Err(format!(
                    "Expected PRECEDING or FOLLOWING after expression in frame bound, found {:?}",
                    self.current_token
                ))
            }
        }
    }

    fn parse_lambda(&mut self) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_lambda: entry, current_token: {:?}", self.current_token);
        let mut parameters = Vec::<&'static str>::new();
        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
            self.advance();
            if let TokenType::Valid(Token::IDENTIFIER(param)) = &self.current_token {
                parameters.push(Box::leak(param.to_string().into_boxed_str()));
                self.advance();
                while let TokenType::Valid(Token::COMMA) = &self.current_token {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(param)) = &self.current_token {
                        parameters.push(Box::leak(param.to_string().into_boxed_str()));
                        self.advance();
                    } else {
                        return Err(format!(
                            "Expected parameter name after comma in lambda parameters, found {:?}",
                            self.current_token
                        ));
                    }
                }
            }
            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                self.advance();
            } else {
                return Err(format!(
                    "Expected ) after lambda parameters, found {:?}",
                    self.current_token
                ));
            }
        } else {
            if let TokenType::Valid(Token::IDENTIFIER(param)) = &self.current_token {
                parameters.push(Box::leak(param.to_string().into_boxed_str()));
                self.advance();
            } else {
                return Err(format!(
                    "Expected parameter name or parameter list for lambda, found {:?}",
                    self.current_token
                ));
            }
        }
        if let TokenType::Valid(Token::MINUS) = &self.current_token {
            self.advance();
            if let TokenType::Valid(Token::GREATER_THAN) = &self.current_token {
                self.advance();
            } else {
                return Err(format!(
                    "Expected > to complete => arrow in lambda expression, found {:?}",
                    self.current_token
                ));
            }
        } else {
            return Err(format!(
                "Expected => arrow in lambda expression, found {:?}",
                self.current_token
            ));
        }
        let body = Box::new(self.parse_expr()?);
        let lambda = crate::ast::Expr::Lambda { parameters, body };
        debug!("parse_lambda: returning lambda: {:#?}", lambda);
        Ok(lambda)
    }

    fn parse_function_call(&mut self, name: &str) -> Result<crate::ast::Expr<'static>, String> {
        debug!("parse_function_call: entry, name: {}, current_token: {:?}", name, self.current_token);
        let function_name = Box::leak(name.to_string().into_boxed_str());
        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
            self.advance();
        } else {
            return Err(format!(
                "Expected ( after function name, found {:?}",
                self.current_token
            ));
        }
        let mut args = Vec::new();
        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
            self.advance();
        } else {
            args.push(self.parse_expr()?);
            while let TokenType::Valid(Token::COMMA) = &self.current_token {
                self.advance();
                args.push(self.parse_expr()?);
            }
            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                self.advance();
            } else {
                return Err(format!(
                    "Expected ) after function arguments, found {:?}",
                    self.current_token
                ));
            }
        }
        let mut filter_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(filter_id)) = &self.current_token {
            if filter_id.eq_ignore_ascii_case("FILTER") {
                self.advance();
                if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(where_id)) = &self.current_token {
                        if where_id.eq_ignore_ascii_case("WHERE") {
                            self.advance();
                            let filter_expr = self.parse_expr()?;
                            filter_clause = Some(Box::new(filter_expr));
                            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                                self.advance();
                            } else {
                                return Err(format!(
                                    "Expected ) after FILTER WHERE clause, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected WHERE after FILTER(, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Expected WHERE after FILTER(, found {:?}",
                            self.current_token
                        ));
                    }
                }
            }
        }
        let mut over_clause = None;
        if let TokenType::Valid(Token::IDENTIFIER(over_id)) = &self.current_token {
            if over_id.eq_ignore_ascii_case("OVER") {
                self.advance();
                let window_spec = self.parse_window_spec()?;
                over_clause = Some(Box::new(window_spec));
            }
        }
        let func_call = crate::ast::Expr::FunctionCall {
            name: function_name,
            args,
            filter_clause,
            over_clause,
        };
        debug!("parse_function_call: returning FunctionCall: {:#?}", func_call);
        Ok(func_call)
    }

    fn parse_alter(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_alter: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("TABLE") {
                self.advance();
                return self.parse_alter_table();
            }
            // In the future we can add other ALTER types here (e.g., ALTER INDEX)
            return Err(format!("Unsupported ALTER type: {}", id));
        }
        debug!("parse_alter: error, expected TABLE after ALTER, found {:?}", self.current_token);
        Err("Expected TABLE after ALTER".to_string())
    }

    fn parse_alter_table(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_alter_table: entry, current_token: {:?}", self.current_token);
        // Handle IF NOT EXISTS
        let mut if_not_exists = false;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("IF") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if id.eq_ignore_ascii_case("NOT") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                            if id.eq_ignore_ascii_case("EXISTS") {
                                if_not_exists = true;
                                self.advance();
                            } else {
                                return Err(format!(
                                    "Expected EXISTS after IF NOT, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected EXISTS after IF NOT, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Expected NOT after IF, found {:?}",
                            self.current_token
                        ));
                    }
                } else {
                    return Err(format!(
                        "Expected NOT after IF, found {:?}",
                        self.current_token
                    ));
                }
            }
        }
        // Parse table name (and possibly schema name)
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let id_static = id.into_static();
            self.advance();
            if let TokenType::Valid(Token::DOT) = &self.current_token {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id2)) = &self.current_token {
                    let table_name = id2.into_static();
                    self.advance(); // Now at the action token (ADD/RENAME/DROP)
                    return self.parse_alter_table_action(Some(id_static), table_name);
                } else {
                    return Err(format!(
                        "Expected table name after schema name, found {:?}",
                        self.current_token
                    ));
                }
            } else {
                let table_name = id_static;
                // Do NOT advance here, current_token is already the action (ADD/RENAME/DROP)
                return self.parse_alter_table_action(None, table_name);
            }
        }

        debug!("parse_alter_table: error, expected table name after ALTER TABLE, found {:?}", self.current_token);
        Err(format!(
            "Expected table name after ALTER TABLE, found {:?}",
            self.current_token
        ))
    }

    fn parse_alter_table_action(
        &mut self,
        schema_name: Option<&'static str>,
        table_name: &'static str,
    ) -> Result<Stmt<'static>, String> {
        debug!("parse_alter_table_action: entry, schema_name: {:?}, table_name: {:?}, current_token: {:?}", schema_name, table_name, self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(action)) = &self.current_token {
            if action.eq_ignore_ascii_case("RENAME") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(to)) = &self.current_token {
                    if to.eq_ignore_ascii_case("TO") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(new_name)) = &self.current_token {
                            let new_name_str = Box::leak(new_name.to_string().into_boxed_str());
                            self.advance();
                            return Ok(Stmt::AlterTable(AlterTableStmt {
                                schema_name,
                                table_name,
                                stmt: AlterTable::RenameTable(new_name_str),
                            }));
                        }
                    } else if to.eq_ignore_ascii_case("COLUMN") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(column_name)) =
                            &self.current_token
                        {
                            let column_name_str =
                                Box::leak(column_name.to_string().into_boxed_str());
                            self.advance();
                            if let TokenType::Valid(Token::IDENTIFIER(to)) = &self.current_token {
                                if to.eq_ignore_ascii_case("TO") {
                                    self.advance();
                                    if let TokenType::Valid(Token::IDENTIFIER(new_name)) =
                                        &self.current_token
                                    {
                                        let new_name_str =
                                            Box::leak(new_name.to_string().into_boxed_str());
                                        self.advance();
                                        return Ok(Stmt::AlterTable(AlterTableStmt {
                                            schema_name,
                                            table_name,
                                            stmt: AlterTable::RenameColumn {
                                                column_name: column_name_str,
                                                new_name: new_name_str,
                                            },
                                        }));
                                    }
                                }
                            }
                        }
                    }
                }
            } else if action.eq_ignore_ascii_case("ADD") {
                self.advance();

                // Check for "COLUMN" keyword which is optional
                if let TokenType::Valid(Token::IDENTIFIER(column_keyword)) = &self.current_token {
                    if column_keyword.eq_ignore_ascii_case("COLUMN") {
                        self.advance();
                    }
                }

                // Now parse the column definition
                if let TokenType::Valid(Token::IDENTIFIER(column_name)) = &self.current_token {
                    // Now parse the column definition using the full parser logic
                    let column_def = self.parse_column_def()?;
                    return Ok(Stmt::AlterTable(AlterTableStmt {
                        schema_name,
                        table_name,
                        stmt: AlterTable::Add(column_def),
                    }));
                }
            } else if action.eq_ignore_ascii_case("DROP") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if id.eq_ignore_ascii_case("COLUMN") {
                        self.advance();
                    }
                }
                if let TokenType::Valid(Token::IDENTIFIER(column_name)) = &self.current_token {
                    let column_name_str = convert_str(column_name.to_string());
                    self.advance();
                    return Ok(Stmt::AlterTable(AlterTableStmt {
                        schema_name,
                        table_name,
                        stmt: AlterTable::Drop(column_name_str),
                    }));
                }
            }
        }

        debug!("parse_alter_table_action: error, expected ALTER TABLE action (RENAME, ADD, DROP), found {:?}", self.current_token);
        Err(format!(
            "Expected ALTER TABLE action (RENAME, ADD, DROP), found {:?}",
            self.current_token
        ))
    }

    fn parse_create(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_create: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("TABLE") {
                self.advance();
                return self.parse_create_table();
            } else if id.eq_ignore_ascii_case("INDEX") {
                self.advance();
                return Err("CREATE INDEX not implemented yet".to_string());
            } else if id.eq_ignore_ascii_case("VIEW") {
                self.advance();
                return Err("CREATE VIEW not implemented yet".to_string());
            } else if id.eq_ignore_ascii_case("TRIGGER") {
                self.advance();
                return Err("CREATE TRIGGER not implemented yet".to_string());
            }
            // Add other CREATE types as needed
        }
        debug!("parse_create: error, expected TABLE, INDEX, VIEW, or TRIGGER after CREATE, found {:?}", self.current_token);
        Err(format!(
            "Expected TABLE, INDEX, VIEW, or TRIGGER after CREATE, found {:?}",
            self.current_token
        ))
    }

    fn parse_create_table(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_create_table: entry, current_token: {:?}", self.current_token);
        // Handle optional IF NOT EXISTS
        let mut if_not_exists = false;
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("IF") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if id.eq_ignore_ascii_case("NOT") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                            if id.eq_ignore_ascii_case("EXISTS") {
                                self.advance();
                                if_not_exists = true;
                            } else {
                                return Err(format!(
                                    "Expected EXISTS after IF NOT, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected EXISTS after IF NOT, found {:?}",
                                self.current_token
                            ));
                        }
                    } else {
                        return Err(format!(
                            "Expected NOT after IF, found {:?}",
                            self.current_token
                        ));
                    }
                } else {
                    return Err(format!(
                        "Expected NOT after IF, found {:?}",
                        self.current_token
                    ));
                }
            }
        }

        // Parse table name (and possibly schema name)
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let id_static = id.into_static();
            self.advance();
            if let TokenType::Valid(Token::DOT) = &self.current_token {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id2)) = &self.current_token {
                    let table_name = id2.into_static();
                    self.advance();
                    // current_token is now the action (ADD/RENAME/DROP)
                    return self.parse_alter_table_action(Some(id_static), table_name);
                } else {
                    return Err(format!(
                        "Expected table name after schema name, found {:?}",
                        self.current_token
                    ));
                }
            } else {
                let table_name = id_static;
                // After table, expect ( or AS for CREATE TABLE
                return self.parse_create_table_body(None, table_name, if_not_exists);
            }
        }

        debug!("parse_create_table: error, expected table name after CREATE TABLE, found {:?}", self.current_token);
        Err(format!(
            "Expected table name after CREATE TABLE, found {:?}",
            self.current_token
        ))
    }

    fn parse_create_table_body(
        &mut self,
        schema_name: Option<&'static str>,
        table_name: &'static str,
        if_not_exists: bool,
    ) -> Result<Stmt<'static>, String> {
        debug!("parse_create_table_body: entry, schema_name: {:?}, table_name: {:?}, if_not_exists: {}, current_token: {:?}", schema_name, table_name, if_not_exists, self.current_token);
        // Accept AS SELECT or column definitions
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("AS") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    if id.eq_ignore_ascii_case("SELECT") {
                        self.advance();
                        let select_stmt = self.parse_select(None)?;
                        if let Stmt::Select(select_box) = select_stmt {
                            return Ok(Stmt::CreateTable {
                                temp_temporary: None, // Assuming non-temporary
                                if_not_exists,
                                schema_name,
                                table_name,
                                body: Box::new(CreateTableBody::AsSelect(select_box)),
                            });
                        } else {
                            return Err("Expected SELECT statement".to_string());
                        }
                    }
                }
            }
            debug!("parse_create_table_body: error, expected SELECT after AS, found {:?}", self.current_token);
            return Err(format!("Expected SELECT after AS, found {:?}", self.current_token));
        }
        // Accept column definitions if next token is LEFT_PAREN
        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
            self.advance();
            let (columns, constraints) = self.parse_column_defs_and_constraints()?;
            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                self.advance();
                return Ok(Stmt::CreateTable {
                    temp_temporary: None, // Assuming non-temporary
                    if_not_exists,
                    schema_name,
                    table_name,
                    body: Box::new(CreateTableBody::ColumnsAndConstraints {
                        columns,
                        constraints,
                    }),
                });
            } else {
                return Err(format!(
                    "Expected ) to close CREATE TABLE definition, found {:?}",
                    self.current_token
                ));
            }
        }
        debug!("parse_create_table_body: error, expected AS or ( after table name, found {:?}", self.current_token);
        Err(format!(
            "Expected AS or ( after table name, found {:?}",
            self.current_token
        ))
    }

    fn parse_column_defs_and_constraints(
        &mut self,
    ) -> Result<(Vec<ColumnDef<'static>>, Vec<TableConstraint<'static>>), String> {
        debug!("parse_column_defs_and_constraints: entry, current_token: {:?}", self.current_token);
        let mut columns = Vec::new();
        let mut constraints = Vec::new();

        // Parse first column definition
        columns.push(self.parse_column_def()?);

        // Parse additional column definitions and table constraints
        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            self.advance();

            if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                if id.eq_ignore_ascii_case("PRIMARY")
                    || id.eq_ignore_ascii_case("UNIQUE")
                    || id.eq_ignore_ascii_case("CHECK")
                    || id.eq_ignore_ascii_case("FOREIGN")
                    || id.eq_ignore_ascii_case("CONSTRAINT")
                {
                    constraints.push(self.parse_table_constraint()?);
                } else {
                    // Regular column definition
                    columns.push(self.parse_column_def()?);
                }
            } else {
                return Err(format!(
                    "Expected column definition or constraint, found {:?}",
                    self.current_token
                ));
            }
        }

        debug!("parse_column_defs_and_constraints: returning columns: {:#?}, constraints: {:#?}", columns, constraints);
        Ok((columns, constraints))
    }

    fn parse_column_def(&mut self) -> Result<ColumnDef<'static>, String> {
        debug!("parse_column_def: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let column_name = convert_str(id.to_string());
            self.advance();

            // Parse type name (optional)
            let mut type_name = None;
            if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                // Check if it's a type
                if id.eq_ignore_ascii_case("INTEGER")
                    || id.eq_ignore_ascii_case("TEXT")
                    || id.eq_ignore_ascii_case("REAL")
                    || id.eq_ignore_ascii_case("BLOB")
                    || id.eq_ignore_ascii_case("NUMERIC")
                    || id.eq_ignore_ascii_case("INT")
                    || id.eq_ignore_ascii_case("VARCHAR")
                    || id.eq_ignore_ascii_case("BOOL")
                    || id.eq_ignore_ascii_case("BOOLEAN")
                    || id.eq_ignore_ascii_case("DATE")
                    || id.eq_ignore_ascii_case("DATETIME")
                {
                    type_name = Some(convert_str(id.to_string()));
                    self.advance();

                    // Check for type modifiers like (size)
                    if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                        self.advance();
                        // Skip type parameters
                        while let TokenType::Valid(token) = &self.current_token {
                            if let Token::RIGHT_PAREN = token {
                                self.advance();
                                break;
                            }
                            self.advance();
                        }
                    }
                }
            }

            // Parse column constraints
            let mut constraints = Vec::new();
            while let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                if id.eq_ignore_ascii_case("PRIMARY") {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(key)) = &self.current_token {
                        if key.eq_ignore_ascii_case("KEY") {
                            self.advance();
                            let mut order = None;
                            if let TokenType::Valid(Token::IDENTIFIER(asc_desc)) =
                                &self.current_token
                            {
                                if asc_desc.eq_ignore_ascii_case("ASC") {
                                    order = Some(ColumnOrder::Ascending);
                                    self.advance();
                                } else if asc_desc.eq_ignore_ascii_case("DESC") {
                                    order = Some(ColumnOrder::Descending);
                                    self.advance();
                                }
                            }

                            let mut autoincrement = false;
                            if let TokenType::Valid(Token::IDENTIFIER(auto)) = &self.current_token {
                                if auto.eq_ignore_ascii_case("AUTOINCREMENT") {
                                    autoincrement = true;
                                    self.advance();
                                }
                            }

                            constraints.push(ColumnConstraint::PrimaryKey {
                                order,
                                conflict_clause: None, // We could parse conflict clause here
                                autoincrement,
                            });
                        }
                    }
                } else if id.eq_ignore_ascii_case("NOT") {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(null)) = &self.current_token {
                        if null.eq_ignore_ascii_case("NULL") {
                            self.advance();
                            constraints.push(ColumnConstraint::NotNull(None)); // We could parse conflict clause here
                        }
                    }
                } else if id.eq_ignore_ascii_case("NULL") {
                    self.advance();
                    // Do nothing, NULL is the default
                } else if id.eq_ignore_ascii_case("UNIQUE") {
                    self.advance();
                    constraints.push(ColumnConstraint::Unique(None)); // We could parse conflict clause here
                } else if id.eq_ignore_ascii_case("DEFAULT") {
                    self.advance();
                    let default_expr = self.parse_expr()?;
                    constraints.push(ColumnConstraint::Default(crate::ast::DefaultValue::Expr(
                        Box::new(default_expr),
                    )));
                } else {
                    // Not a constraint, break the loop
                    break;
                }
            }

            let col_def = ColumnDef {
                name: column_name,
                type_name,
                constraints,
            };
            debug!("parse_column_def: returning col_def: {:#?}", col_def);
            return Ok(col_def);
        }

        debug!("parse_column_def: error, expected column name, found {:?}", self.current_token);
        Err(format!(
            "Expected column name, found {:?}",
            self.current_token
        ))
    }

    fn parse_table_constraint(&mut self) -> Result<TableConstraint<'static>, String> {
        debug!("parse_table_constraint: entry, current_token: {:?}", self.current_token);
        // For simplicity, we'll just implement a basic version that parses PRIMARY KEY constraints
        let mut constraint_name = None;

        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("CONSTRAINT") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(name)) = &self.current_token {
                    constraint_name = Some(convert_str(name.to_string()));
                    self.advance();
                }
            }
        }

        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("PRIMARY") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(key)) = &self.current_token {
                    if key.eq_ignore_ascii_case("KEY") {
                        self.advance();
                        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                            self.advance();
                            let mut columns = Vec::new();

                            // Parse first column
                            if let TokenType::Valid(Token::IDENTIFIER(col)) = &self.current_token {
                                let col_name = convert_str(col.to_string());
                                self.advance();

                                let mut order = None;
                                if let TokenType::Valid(Token::IDENTIFIER(asc_desc)) =
                                    &self.current_token
                                {
                                    if asc_desc.eq_ignore_ascii_case("ASC") {
                                        order = Some(ColumnOrder::Ascending);
                                        self.advance();
                                    } else if asc_desc.eq_ignore_ascii_case("DESC") {
                                        order = Some(ColumnOrder::Descending);
                                        self.advance();
                                    }
                                }

                                columns.push(crate::ast::IndexedColumn {
                                    column_name: col_name,
                                    expr: None,
                                    collation: None,
                                    order,
                                });

                                // Parse additional columns
                                while let TokenType::Valid(Token::COMMA) = &self.current_token {
                                    self.advance();
                                    if let TokenType::Valid(Token::IDENTIFIER(col)) =
                                        &self.current_token
                                    {
                                        let col_name = convert_str(col.to_string());
                                        self.advance();

                                        let mut order = None;
                                        if let TokenType::Valid(Token::IDENTIFIER(asc_desc)) =
                                            &self.current_token
                                        {
                                            if asc_desc.eq_ignore_ascii_case("ASC") {
                                                order = Some(ColumnOrder::Ascending);
                                                self.advance();
                                            } else if asc_desc.eq_ignore_ascii_case("DESC") {
                                                order = Some(ColumnOrder::Descending);
                                                self.advance();
                                            }
                                        }

                                        columns.push(crate::ast::IndexedColumn {
                                            column_name: col_name,
                                            expr: None,
                                            collation: None,
                                            order,
                                        });
                                    }
                                }

                                if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                                    self.advance();
                                    let pk = TableConstraint::PrimaryKey {
                                        name: constraint_name,
                                        columns,
                                        conflict_clause: None, // We could parse conflict clause here
                                    };
                                    debug!("parse_table_constraint: returning PrimaryKey: {:#?}", pk);
                                    return Ok(pk);
                                }
                            }
                        }
                    }
                }
            }
        }

        debug!("parse_table_constraint: error, expected PRIMARY KEY, UNIQUE, CHECK or FOREIGN KEY constraint, found {:?}", self.current_token);
        Err(format!(
            "Expected PRIMARY KEY, UNIQUE, CHECK or FOREIGN KEY constraint, found {:?}",
            self.current_token
        ))
    }

    fn parse_drop(&mut self) -> Result<Stmt<'static>, String> {
        debug!("parse_drop: entry, current_token: {:?}", self.current_token);
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            if id.eq_ignore_ascii_case("TABLE") {
                self.advance();

                // Parse IF EXISTS
                let mut if_exists = false;
                if let TokenType::Valid(Token::IDENTIFIER(if_keyword)) = &self.current_token {
                    if if_keyword.eq_ignore_ascii_case("IF") {
                        self.advance();
                        if let TokenType::Valid(Token::IDENTIFIER(exists)) = &self.current_token {
                            if exists.eq_ignore_ascii_case("EXISTS") {
                                if_exists = true;
                                self.advance();
                            } else {
                                return Err(format!(
                                    "Expected EXISTS after IF, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            return Err(format!(
                                "Expected EXISTS after IF, found {:?}",
                                self.current_token
                            ));
                        }
                    }
                }

                // Parse table name (and possibly schema name)
                let schema_name_opt =
                    if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                        let name = id.to_string();
                        self.advance();
                        if let TokenType::Valid(Token::DOT) = &self.current_token {
                            self.advance();
                            let schema = Some(convert_str(name));
                            if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                                let table_name = convert_str(id.to_string());
                                self.advance();
                                let drop_stmt = Stmt::DropTable {
                                    if_exists,
                                    schema_name: schema,
                                    table_name,
                                };
                                debug!("parse_drop: returning DropTable: {:#?}", drop_stmt);
                                return Ok(drop_stmt);
                            } else {
                                return Err(format!(
                                    "Expected table name after schema name, found {:?}",
                                    self.current_token
                                ));
                            }
                        } else {
                            let table_name = convert_str(name);
                            let drop_stmt = Stmt::DropTable {
                                if_exists,
                                schema_name: None,
                                table_name,
                            };
                            debug!("parse_drop: returning DropTable: {:#?}", drop_stmt);
                            return Ok(drop_stmt);
                        }
                    } else {
                        return Err(format!(
                            "Expected table name, found {:?}",
                            self.current_token
                        ));
                    };
            }
        }

        debug!("parse_drop: error, expected TABLE, INDEX, VIEW or TRIGGER after DROP, found {:?}", self.current_token);
        Err(format!(
            "Expected TABLE, INDEX, VIEW or TRIGGER after DROP, found {:?}",
            self.current_token
        ))
    }

    fn parse_insert(&mut self, with_clause: Option<crate::ast::WithClause<'static>>) -> Result<Stmt<'static>, String> {
        debug!("parse_insert: entry, current_token: {:?}", self.current_token);
        // Support for INSERT INTO syntax
        if let TokenType::Valid(Token::IDENTIFIER(into)) = &self.current_token {
            if !into.eq_ignore_ascii_case("INTO") {
                return Err(format!(
                    "Expected INTO after INSERT, found {:?}",
                    self.current_token
                ));
            }
            self.advance();
        }

        // Parse table name (and possibly schema name)
        let mut schema_name = None;
        let mut table_name = "";
        if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
            let name = id.to_string();
            self.advance();
            if let TokenType::Valid(Token::DOT) = &self.current_token {
                self.advance();
                schema_name = Some(convert_str(name));
                if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                    table_name = convert_str(id.to_string());
                    self.advance();
                } else {
                    return Err(format!(
                        "Expected table name after schema name, found {:?}",
                        self.current_token
                    ));
                }
            } else {
                table_name = convert_str(name);
            }
        } else {
            return Err(format!(
                "Expected table name, found {:?}",
                self.current_token
            ));
        }

        // Parse column names (optional)
        let mut column_names = None;
        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
            self.advance();
            let mut cols = Vec::new();

            if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                cols.push(convert_str(id.to_string()));
                self.advance();

                while let TokenType::Valid(Token::COMMA) = &self.current_token {
                    self.advance();
                    if let TokenType::Valid(Token::IDENTIFIER(id)) = &self.current_token {
                        cols.push(convert_str(id.to_string()));
                        self.advance();
                    } else {
                        return Err(format!(
                            "Expected column name, found {:?}",
                            self.current_token
                        ));
                    }
                }
            }

            if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
                self.advance();
                column_names = Some(cols);
            } else {
                return Err(format!("Expected ), found {:?}", self.current_token));
            }
        }

        // Parse VALUES or SELECT
        if let TokenType::Valid(Token::IDENTIFIER(values_or_select)) = &self.current_token {
            if values_or_select.eq_ignore_ascii_case("VALUES") {
                self.advance();
                let values = self.parse_insert_values()?;

                let insert_stmt = Stmt::Insert {
                    with_clause: None,
                    or_conflict: None,
                    schema_name,
                    table_name,
                    alias: None,
                    column_names,
                    data_source: crate::ast::InsertDataSource::Values(values),
                    returning_clause: None,
                };
                debug!("parse_insert: returning Insert (VALUES): {:#?}", insert_stmt);
                return Ok(insert_stmt);
            } else if values_or_select.eq_ignore_ascii_case("SELECT") {
                self.advance();
                let select_stmt = self.parse_select(None)?;
                if let Stmt::Select(select_box) = select_stmt {
                    let insert_stmt = Stmt::Insert {
                        with_clause: None,
                        or_conflict: None,
                        schema_name,
                        table_name,
                        alias: None,
                        column_names,
                        data_source: crate::ast::InsertDataSource::Select(select_box),
                        returning_clause: None,
                    };
                    debug!("parse_insert: returning Insert (SELECT): {:#?}", insert_stmt);
                    return Ok(insert_stmt);
                } else {
                    return Err("Expected SELECT statement".to_string());
                }
            } else if values_or_select.eq_ignore_ascii_case("DEFAULT") {
                self.advance();
                if let TokenType::Valid(Token::IDENTIFIER(values)) = &self.current_token {
                    if values.eq_ignore_ascii_case("VALUES") {
                        self.advance();
                        let insert_stmt = Stmt::Insert {
                            with_clause: None,
                            or_conflict: None,
                            schema_name,
                            table_name,
                            alias: None,
                            column_names,
                            data_source: crate::ast::InsertDataSource::DefaultValues,
                            returning_clause: None,
                        };
                        debug!("parse_insert: returning Insert (DEFAULT VALUES): {:#?}", insert_stmt);
                        return Ok(insert_stmt);
                    }
                }
            }
        }

        debug!("parse_insert: error, expected VALUES, SELECT or DEFAULT VALUES, found {:?}", self.current_token);
        Err(format!(
            "Expected VALUES, SELECT or DEFAULT VALUES, found {:?}",
            self.current_token
        ))
    }

    fn parse_insert_values(&mut self) -> Result<Vec<Vec<Expr<'static>>>, String> {
        debug!("parse_insert_values: entry, current_token: {:?}", self.current_token);
        let mut values_list = Vec::new();

        // Parse first row
        if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
            self.advance();
            let row = self.parse_insert_value_list()?;
            values_list.push(row);

            // Parse additional rows
            while let TokenType::Valid(Token::COMMA) = &self.current_token {
                self.advance();
                if let TokenType::Valid(Token::LEFT_PAREN) = &self.current_token {
                    self.advance();
                    let row = self.parse_insert_value_list()?;
                    values_list.push(row);
                } else {
                    return Err(format!("Expected (, found {:?}", self.current_token));
                }
            }

            debug!("parse_insert_values: returning values_list: {:#?}", values_list);
            return Ok(values_list);
        }

        debug!("parse_insert_values: error, expected (, found {:?}", self.current_token);
        Err(format!("Expected (, found {:?}", self.current_token))
    }
    
    fn parse_insert_value_list(&mut self) -> Result<Vec<Expr<'static>>, String> {
        debug!("parse_insert_value_list: entry, current_token: {:?}", self.current_token);
        let mut values = Vec::new();

        // Handle empty list case
        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
            self.advance();
            return Ok(values);
        }

        values.push(self.parse_expr()?);

        while let TokenType::Valid(Token::COMMA) = &self.current_token {
            self.advance();
            values.push(self.parse_expr()?);
        }

        if let TokenType::Valid(Token::RIGHT_PAREN) = &self.current_token {
            self.advance();
            debug!("parse_insert_value_list: returning values: {:#?}", values);
            return Ok(values);
        }

        debug!("parse_insert_value_list: error, expected ) to close value list, found {:?}", self.current_token);
        Err(format!(
            "Expected ) to close value list, found {:?}",
            self.current_token
        ))
    }
}

pub fn parse(input: &str) -> Result<Vec<Stmt<'static>>, String> {
    let mut parser = Parser::new(input);
    if let TokenType::EndOfInput = parser.current_token {
        return Err("Empty input".to_string());
    }
    let stmt = parser.parse_statement()?;
    Ok(vec![stmt])
}

pub fn parse_statement(input: &str) -> Result<Stmt<'static>, String> {
    let mut parser = Parser::new(input);
    parser.parse_statement()
}

pub trait IntoStatic<'a> {
    type Output;
    fn into_static(self) -> Self::Output;
}

impl<'a> IntoStatic<'a> for &'a str {
    type Output = &'static str;
    fn into_static(self) -> Self::Output {
        convert_str(self.to_string())
    }
}

impl<'a> IntoStatic<'a> for String {
    type Output = &'static str;
    fn into_static(self) -> Self::Output {
        convert_str(self)
    }
}

impl<'a> IntoStatic<'a> for Option<&'a str> {
    type Output = Option<&'static str>;
    fn into_static(self) -> Self::Output {
        self.map(|s| s.into_static())
    }
}

impl<'a> IntoStatic<'a> for Option<String> {
    type Output = Option<&'static str>;
    fn into_static(self) -> Self::Output {
        self.map(|s| s.into_static())
    }
}

impl<'a> IntoStatic<'a> for Vec<&'a str> {
    type Output = Vec<&'static str>;
    fn into_static(self) -> Self::Output {
        self.into_iter().map(|s| s.into_static()).collect()
    }
}

impl<'a> IntoStatic<'a> for Vec<String> {
    type Output = Vec<&'static str>;
    fn into_static(self) -> Self::Output {
        self.into_iter().map(|s| s.into_static()).collect()
    }
}

fn convert_mut_str_vec<'a>(vec: Vec<&'a mut str>) -> Vec<&'static str> {
    vec.into_iter()
        .map(|s| convert_str(s.to_string()))
        .collect()
}

fn convert_option_mut_str(opt: Option<&mut str>) -> Option<&'static str> {
    match opt {
        Some(s) => Some(convert_str(s.to_string())),
        None => None,
    }
}

fn convert_str(s: String) -> &'static str {
    // This is more efficient than Box::leak
    unsafe {
        let boxed = s.into_boxed_str();
        let static_str = mem::transmute::<&str, &'static str>(boxed.as_ref());
        let _ = Box::into_raw(boxed); // Prevent drop (intentional leak)
        static_str
    }
}

fn convert_option_str(opt: Option<&str>) -> Option<&'static str> {
    match opt {
        Some(s) => Some(convert_str(s.to_string())),
        None => None,
    }
}

pub fn print_ast(stmt: &crate::ast::Stmt) {
    println!("{:#?}", stmt);
}
