use crate::ast::{self, *};
use crate::lexer::{Token, Keywords};
use logos::{Logos, Span};
use std::borrow::Cow;

pub struct Parser<'a> {
    pub input: &'a str,
    pub current_token: Option<Result<Token<'a>, ()>>,
    lexer: logos::Lexer<'a, Token<'a>>,
    span: Span,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Token::lexer(input);
        let current_token = lexer.next();
        let span = lexer.span();
        
        Self {
            input,
            current_token,
            lexer,
            span,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.lexer.next();
        self.span = self.lexer.span();
    }

    fn peek(&self) -> Option<&Result<Token<'a>, ()>> {
        self.current_token.as_ref()
    }

    fn match_token(&self, token: &Token<'a>) -> bool {
        match &self.current_token {
            Some(Ok(current)) => current == token,
            _ => false,
        }
    }

    fn consume(&mut self, token: &Token<'a>) -> bool {
        if self.match_token(token) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, token: Token<'a>) -> Result<(), String> {
        if self.match_token(&token) {
            self.advance();
            Ok(())
        } else {
            Err(format!(
                "Expected token {:?}, found {:?}",
                token, self.current_token
            ))
        }
    }

    fn get_token_text(&self) -> &'a str {
        &self.input[self.span.clone()]
    }

    pub fn parse_statement(&mut self) -> Result<Stmt<'a>, String> {
        match &self.current_token {
            Some(Ok(Token::IDENTIFIER(id))) => {
                match *id {
                    "SELECT" => self.parse_select(),
                    "INSERT" => self.parse_insert(),
                    "UPDATE" => self.parse_update(),
                    "DELETE" => self.parse_delete(),
                    "CREATE" => self.parse_create(),
                    "ALTER" => self.parse_alter(),
                    "DROP" => self.parse_drop(),
                    "BEGIN" => self.parse_begin(),
                    "COMMIT" => self.parse_commit(),
                    "ROLLBACK" => self.parse_rollback(),
                    "PRAGMA" => self.parse_pragma(),
                    "ANALYZE" => self.parse_analyze(),
                    "ATTACH" => self.parse_attach(),
                    "DETACH" => self.parse_detach(),
                    "VACUUM" => self.parse_vacuum(),
                    "REINDEX" => self.parse_reindex(),
                    "SAVEPOINT" => self.parse_savepoint(),
                    "RELEASE" => self.parse_release(),
                    _ => Err(format!("Unexpected statement type: {}", id)),
                }
            }
            Some(Ok(Token::SELECT_DISTINCT)) => {
                self.parse_select_distinct()
            }
            _ => Err(format!(
                "Expected statement, found {:?}",
                self.current_token
            )),
        }
    }

    pub fn parse_statements(&mut self) -> Result<Vec<Stmt<'a>>, String> {
        let mut statements = Vec::new();
        
        while self.current_token.is_some() {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            
            if let Some(Ok(Token::SEMICOLON)) = self.current_token {
                self.advance();
            } else {
                if self.current_token.is_some() {
                    return Err(format!("Expected semicolon or end of input, found {:?}", self.current_token));
                }
            }
        }
        
        Ok(statements)
    }

    fn parse_select(&mut self) -> Result<Stmt<'a>, String> {
        self.advance();
        
        let mut select_stmt = SelectStmt {
            with_clause: self.parse_with_clause()?,
            compound_operator: None,
            select_core: self.parse_select_core(false)?,
            order_by_clause: None,
            limit_clause: None,
        };

        if let Some(Ok(Token::ORDER_BY)) = self.current_token {
            self.advance();
            select_stmt.order_by_clause = Some(self.parse_order_by_clause()?);
        } else if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "ORDER" {
                self.advance();
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "BY" {
                        self.advance();
                        select_stmt.order_by_clause = Some(self.parse_order_by_clause()?);
                    }
                }
            }
        }

        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "LIMIT" {
                self.advance();
                select_stmt.limit_clause = Some(self.parse_limit_clause()?);
            }
        }

        Ok(Stmt::Select(Box::new(select_stmt)))
    }

    fn parse_select_distinct(&mut self) -> Result<Stmt<'a>, String> {
        self.advance();
        
        let mut select_stmt = SelectStmt {
            with_clause: None,
            compound_operator: None,
            select_core: self.parse_select_core(true)?,
            order_by_clause: None,
            limit_clause: None,
        };

        Ok(Stmt::Select(Box::new(select_stmt)))
    }

    fn parse_with_clause(&mut self) -> Result<Option<WithClause<'a>>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id.eq("WITH") {
                self.advance();
                
                let recursive = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id.eq("RECURSIVE") {
                        self.advance();
                        true
                    } else {
                        false
                    }
                } else {
                    false
                };
                
                let mut cte_tables = Vec::new();
                
                cte_tables.push(self.parse_common_table_expression()?);
                
                while let Some(Ok(Token::COMMA)) = self.current_token {
                    self.advance();
                    cte_tables.push(self.parse_common_table_expression()?);
                }
                
                return Ok(Some(WithClause {
                    recursive,
                    cte_tables,
                }));
            }
        }
        
        Ok(None)
    }

    fn parse_common_table_expression(&mut self) -> Result<CommonTableExpression<'a>, String> {
        let table_name = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            let id = id.clone();
            self.advance();
            id
        } else {
            return Err("Expected identifier for CTE name".to_string());
        };
        
        let column_names = if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
            self.advance();
            
            let mut columns = Vec::new();
            
            if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                columns.push(id);
                self.advance();
                
                while let Some(Ok(Token::COMMA)) = self.current_token {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        columns.push(id);
                        self.advance();
                    } else {
                        return Err("Expected identifier for column name".to_string());
                    }
                }
                
                if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
                    self.advance();
                } else {
                    return Err("Expected closing parenthesis after column names".to_string());
                }
                
                Some(columns)
            } else {
                return Err("Expected at least one column name".to_string());
            }
        } else {
            None
        };
        
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "AS" {
                self.advance();
            } else {
                return Err("Expected AS keyword".to_string());
            }
        } else {
            return Err("Expected AS keyword".to_string());
        }
        
        let materialized = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "MATERIALIZED" {
                self.advance();
                Some(true)
            } else if id == "NOT" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "MATERIALIZED" {
                        self.advance();
                        Some(false)
                    } else {
                        return Err("Expected MATERIALIZED after NOT".to_string());
                    }
                } else {
                    return Err("Expected MATERIALIZED after NOT".to_string());
                }
            } else {
                None
            }
        } else {
            None
        };
        
        if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
            self.advance();
        } else {
            return Err("Expected opening parenthesis after AS".to_string());
        }
        
        let select_stmt = self.parse_select()?;
        
        let select_stmt = if let Stmt::Select(stmt) = select_stmt {
            stmt
        } else {
            return Err("Expected SELECT statement in CTE".to_string());
        };
        
        if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
            self.advance();
        } else {
            return Err("Expected closing parenthesis after SELECT".to_string());
        }
        
        Ok(CommonTableExpression {
            table_name,
            column_names,
            select_stmt,
            materialized,
        })
    }

    fn parse_select_core(&mut self, distinct: bool) -> Result<SelectCore<'a>, String> {
        let result_columns = self.parse_result_columns()?;
        
        let from_clause = self.parse_from_clause()?;
        
        let where_clause = self.parse_where_clause()?;
        
        let group_by_clause = self.parse_group_by_clause()?;
        
        let having_clause = self.parse_having_clause()?;
        
        let window_clause = self.parse_window_clause()?;
        
        Ok(SelectCore {
            distinct,
            result_columns,
            from_clause,
            where_clause,
            group_by_clause,
            having_clause,
            window_clause,
        })
    }
    
    fn parse_result_columns(&mut self) -> Result<Vec<ResultColumn<'a>>, String> {
        let mut result_columns = Vec::new();
        
        result_columns.push(self.parse_result_column()?);
        
        while let Some(Ok(Token::COMMA)) = self.current_token {
            self.advance();
            result_columns.push(self.parse_result_column()?);
        }
        
        Ok(result_columns)
    }
    
    fn parse_result_column(&mut self) -> Result<ResultColumn<'a>, String> {
        match &self.current_token {
            Some(Ok(Token::ASTERISK)) => {
                self.advance();
                Ok(ResultColumn::AllColumns)
            },
            
            Some(Ok(Token::IDENTIFIER(table_name))) => {
                let table_name = table_name.clone();
                self.advance();
                
                if let Some(Ok(Token::DOT)) = self.current_token {
                    self.advance();
                    
                    if let Some(Ok(Token::ASTERISK)) = self.current_token {
                        self.advance();
                        Ok(ResultColumn::TableAllColumns(table_name))
                    } else {
                        let column_name = if let Some(Ok(Token::IDENTIFIER(column_name))) = self.current_token {
                            self.advance();
                            column_name
                        } else {
                            return Err("Expected column name after dot".to_string());
                        };
                        
                        let expr = Expr::Column {
                            schema_name: None,
                            table_name: Some(table_name),
                            column_name,
                        };
                        
                        let alias = self.parse_optional_alias()?;
                        
                        Ok(ResultColumn::Expr {
                            expr: Box::new(expr),
                            alias,
                        })
                    }
                } else {
                    let expr = Expr::Column {
                        schema_name: None,
                        table_name: None,
                        column_name: table_name,
                    };
                    
                    let alias = self.parse_optional_alias()?;
                    
                    Ok(ResultColumn::Expr {
                        expr: Box::new(expr),
                        alias,
                    })
                }
            },
            
            _ => {
                let expr = self.parse_expr()?;
                
                let alias = self.parse_optional_alias()?;
                
                Ok(ResultColumn::Expr {
                    expr: Box::new(expr),
                    alias,
                })
            }
        }
    }
    
    fn parse_optional_alias(&mut self) -> Result<Option<&'a str>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "AS" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(alias))) = self.current_token {
                    self.advance();
                    Ok(Some(alias))
                } else {
                    Err("Expected alias name after AS".to_string())
                }
            } else {
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "FROM" || id == "WHERE" || id == "GROUP" || id == "ORDER" || id == "HAVING" || id == "LIMIT" {
                        Ok(None)
                    } else {
                        let alias = id;
                        self.advance();
                        Ok(Some(alias))
                    }
                } else if let Some(Ok(Token::COMMA)) = self.current_token {
                    Ok(None)
                } else {
                    Ok(None)
                }
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_from_clause(&mut self) -> Result<Option<FromClause<'a>>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "FROM" {
                self.advance();
                
                let mut tables = Vec::new();
                let mut join_clauses = Vec::new();
                
                tables.push(self.parse_table_or_subquery()?);
                
                loop {
                    let join_type = if let Some(Ok(Token::LEFT_JOIN)) = self.current_token {
                        self.advance();
                        JoinType::Left
                    } else if let Some(Ok(Token::INNER_JOIN)) = self.current_token {
                        self.advance();
                        JoinType::Inner
                    } else if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "JOIN" {
                            self.advance();
                            JoinType::Inner
                        } else if id == "LEFT" {
                            self.advance();
                            
                            if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                                if id == "OUTER" {
                                    self.advance();
                                    
                                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                                        if id == "JOIN" {
                                            self.advance();
                                            JoinType::LeftOuter
                                        } else {
                                            return Err("Expected JOIN after LEFT OUTER".to_string());
                                        }
                                    } else {
                                        return Err("Expected JOIN after LEFT OUTER".to_string());
                                    }
                                } else if id == "JOIN" {
                                    self.advance();
                                    JoinType::Left
                                } else {
                                    break;
                                }
                            } else {
                                break;
                            }
                        } else if id == "CROSS" {
                            self.advance();
                            
                            if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                                if id == "JOIN" {
                                    self.advance();
                                    JoinType::Cross
                                } else {
                                    return Err("Expected JOIN after CROSS".to_string());
                                }
                            } else {
                                return Err("Expected JOIN after CROSS".to_string());
                            }
                        } else {
                            break;
                        }
                    } else {
                        break;
                    };
                    
                    let table_or_subquery = self.parse_table_or_subquery()?;
                    
                    let constraint = self.parse_join_constraint()?;
                    
                    join_clauses.push(JoinClause {
                        join_type,
                        table_or_subquery,
                        constraint,
                    });
                }
                
                Ok(Some(FromClause {
                    tables,
                    join_clauses,
                }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_table_or_subquery(&mut self) -> Result<TableOrSubquery<'a>, String> {
        if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
            self.advance();
            
            if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "SELECT" {
                    let select_stmt = self.parse_select()?;
                    
                    let select_stmt = if let Stmt::Select(stmt) = select_stmt {
                        stmt
                    } else {
                        return Err("Expected SELECT statement in subquery".to_string());
                    };
                    
                    if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
                        self.advance();
                    } else {
                        return Err("Expected closing parenthesis after subquery".to_string());
                    }
                    
                    let alias = self.parse_optional_alias()?;
                    
                    Ok(TableOrSubquery::Subquery {
                        select_stmt,
                        alias,
                    })
                } else {
                    Err("Parenthesized table not yet supported".to_string())
                }
            } else {
                Err("Expected SELECT or table name in parentheses".to_string())
            }
        } else if let Some(Ok(Token::IDENTIFIER(table_name))) = self.current_token {
            self.advance();
            
            let (schema_name, table_name) = if let Some(Ok(Token::DOT)) = self.current_token {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(actual_table_name))) = self.current_token {
                    self.advance();
                    (Some(table_name), actual_table_name)
                } else {
                    return Err("Expected table name after schema name".to_string());
                }
            } else {
                (None, table_name)
            };
            
            let alias = self.parse_optional_alias()?;
            
            let (indexed_by, not_indexed) = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "INDEXED" {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "BY" {
                            self.advance();
                            
                            if let Some(Ok(Token::IDENTIFIER(index_name))) = self.current_token {
                                self.advance();
                                (Some(index_name), false)
                            } else {
                                return Err("Expected index name after INDEXED BY".to_string());
                            }
                        } else {
                            return Err("Expected BY after INDEXED".to_string());
                        }
                    } else {
                        return Err("Expected BY after INDEXED".to_string());
                    }
                } else if id == "NOT" {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "INDEXED" {
                            self.advance();
                            (None, true)
                        } else {
                            return Err("Expected INDEXED after NOT".to_string());
                        }
                    } else {
                        return Err("Expected INDEXED after NOT".to_string());
                    }
                } else {
                    (None, false)
                }
            } else {
                (None, false)
            };
            
            Ok(TableOrSubquery::Table {
                schema_name,
                table_name,
                alias,
                indexed_by,
                not_indexed,
            })
        } else {
            Err("Expected table name, subquery, or function".to_string())
        }
    }
    
    fn parse_join_constraint(&mut self) -> Result<JoinConstraint<'a>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "ON" {
                self.advance();
                
                let expr = self.parse_expr()?;
                
                Ok(JoinConstraint::On(Box::new(expr)))
            } else if id == "USING" {
                self.advance();
                
                if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
                    self.advance();
                } else {
                    return Err("Expected opening parenthesis after USING".to_string());
                }
                
                let mut columns = Vec::new();
                
                if let Some(Ok(Token::IDENTIFIER(column_name))) = self.current_token {
                    columns.push(column_name);
                    self.advance();
                } else {
                    return Err("Expected column name in USING clause".to_string());
                }
                
                while let Some(Ok(Token::COMMA)) = self.current_token {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(column_name))) = self.current_token {
                        columns.push(column_name);
                        self.advance();
                    } else {
                        return Err("Expected column name after comma in USING clause".to_string());
                    }
                }
                
                if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
                    self.advance();
                } else {
                    return Err("Expected closing parenthesis in USING clause".to_string());
                }
                
                Ok(JoinConstraint::Using(columns))
            } else {
                Err("Expected ON or USING after JOIN".to_string())
            }
        } else {
            Err("Expected ON or USING after JOIN".to_string())
        }
    }
    
    fn parse_where_clause(&mut self) -> Result<Option<Box<Expr<'a>>>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "WHERE" {
                self.advance();
                
                let expr = self.parse_expr()?;
                
                Ok(Some(Box::new(expr)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_group_by_clause(&mut self) -> Result<Option<GroupByClause<'a>>, String> {
        if let Some(Ok(Token::GROUP_BY)) = self.current_token {
            self.advance();
            
            let mut exprs = Vec::new();
            
            exprs.push(self.parse_expr()?);
            
            while let Some(Ok(Token::COMMA)) = self.current_token {
                self.advance();
                exprs.push(self.parse_expr()?);
            }
            
            Ok(Some(GroupByClause { exprs }))
        } else if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "GROUP" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "BY" {
                        self.advance();
                        
                        let mut exprs = Vec::new();
                        
                        exprs.push(self.parse_expr()?);
                        
                        while let Some(Ok(Token::COMMA)) = self.current_token {
                            self.advance();
                            exprs.push(self.parse_expr()?);
                        }
                        
                        Ok(Some(GroupByClause { exprs }))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_having_clause(&mut self) -> Result<Option<Box<Expr<'a>>>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "HAVING" {
                self.advance();
                
                let expr = self.parse_expr()?;
                
                Ok(Some(Box::new(expr)))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_window_clause(&mut self) -> Result<Option<WindowClause<'a>>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "WINDOW" {
                self.advance();
                
                let mut window_defs = Vec::new();
                
                let name = self.parse_window_name()?;
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "AS" {
                        self.advance();
                    } else {
                        return Err("Expected AS after window name".to_string());
                    }
                } else {
                    return Err("Expected AS after window name".to_string());
                }
                
                if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
                    self.advance();
                } else {
                    return Err("Expected opening parenthesis after AS".to_string());
                }
                
                let window_def = self.parse_window_def()?;
                
                if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
                    self.advance();
                } else {
                    return Err("Expected closing parenthesis after window definition".to_string());
                }
                
                window_defs.push((name, window_def));
                
                while let Some(Ok(Token::COMMA)) = self.current_token {
                    self.advance();
                    
                    let name = self.parse_window_name()?;
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "AS" {
                            self.advance();
                        } else {
                            return Err("Expected AS after window name".to_string());
                        }
                    } else {
                        return Err("Expected AS after window name".to_string());
                    }
                    
                    if let Some(Ok(Token::LEFT_PAREN)) = self.current_token {
                        self.advance();
                    } else {
                        return Err("Expected opening parenthesis after AS".to_string());
                    }
                    
                    let window_def = self.parse_window_def()?;
                    
                    if let Some(Ok(Token::RIGHT_PAREN)) = self.current_token {
                        self.advance();
                    } else {
                        return Err("Expected closing parenthesis after window definition".to_string());
                    }
                    
                    window_defs.push((name, window_def));
                }
                
                Ok(Some(WindowClause { window_defs }))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
    
    fn parse_window_name(&mut self) -> Result<WindowName<'a>, String> {
        if let Some(Ok(Token::IDENTIFIER(name))) = self.current_token {
            self.advance();
            Ok(WindowName::Name(name))
        } else {
            Err("Expected window name".to_string())
        }
    }
    
    fn parse_window_def(&mut self) -> Result<WindowDef<'a>, String> {
        let base_window_name = if let Some(Ok(Token::IDENTIFIER(name))) = self.current_token {
            self.advance();
            Some(name)
        } else {
            None
        };
        
        let mut partition_by = Vec::new();
        
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "PARTITION" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "BY" {
                        self.advance();
                        
                        partition_by.push(self.parse_expr()?);
                        
                        while let Some(Ok(Token::COMMA)) = self.current_token {
                            self.advance();
                            partition_by.push(self.parse_expr()?);
                        }
                    }
                }
            }
        }
        
        let order_by = if let Some(Ok(Token::ORDER_BY)) = self.current_token {
            self.advance();
            Some(self.parse_order_by_clause()?)
        } else if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "ORDER" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    if id == "BY" {
                        self.advance();
                        Some(self.parse_order_by_clause()?)
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        };
        
        let frame_spec = self.parse_frame_spec()?;
        
        Ok(WindowDef {
            base_window_name,
            partition_by,
            order_by,
            frame_spec,
        })
    }
    
    fn parse_frame_spec(&mut self) -> Result<Option<FrameSpec>, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            let type_ = match id {
                "RANGE" => {
                    self.advance();
                    FrameType::Range
                },
                "ROWS" => {
                    self.advance();
                    FrameType::Rows
                },
                "GROUPS" => {
                    self.advance();
                    FrameType::Groups
                },
                _ => {
                    return Ok(None);
                }
            };
            
            let bounds = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "BETWEEN" {
                    self.advance();
                    
                    let start = self.parse_frame_bound()?;
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "AND" {
                            self.advance();
                            
                            let end = self.parse_frame_bound()?;
                            
                            FrameBounds::Between(start, end)
                        } else {
                            return Err("Expected AND in BETWEEN clause".to_string());
                        }
                    } else {
                        return Err("Expected AND in BETWEEN clause".to_string());
                    }
                } else {
                    let start = self.parse_frame_bound()?;
                    FrameBounds::Start(start)
                }
            } else {
                return Err("Expected frame bound".to_string());
            };
            
            let exclude = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "EXCLUDE" {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        self.advance();
                        
                        match id {
                            "NO" => {
                                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                                    if id == "OTHERS" {
                                        self.advance();
                                        Some(FrameExclude::NoOthers)
                                    } else {
                                        return Err("Expected OTHERS after NO".to_string());
                                    }
                                } else {
                                    return Err("Expected OTHERS after NO".to_string());
                                }
                            },
                            "CURRENT" => {
                                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                                    if id == "ROW" {
                                        self.advance();
                                        Some(FrameExclude::CurrentRow)
                                    } else {
                                        return Err("Expected ROW after CURRENT".to_string());
                                    }
                                } else {
                                    return Err("Expected ROW after CURRENT".to_string());
                                }
                            },
                            "GROUP" => Some(FrameExclude::Group),
                            "TIES" => Some(FrameExclude::Ties),
                            _ => return Err(format!("Unexpected EXCLUDE type: {}", id)),
                        }
                    } else {
                        return Err("Expected EXCLUDE type".to_string());
                    }
                } else {
                    None
                }
            } else {
                None
            };
            
            Ok(Some(FrameSpec {
                type_,
                bounds,
                exclude,
            }))
        } else {
            Ok(None)
        }
    }
    
    fn parse_frame_bound(&mut self) -> Result<FrameBound, String> {
        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            match id {
                "UNBOUNDED" => {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "PRECEDING" || id == "FOLLOWING" {
                            self.advance();
                            Ok(FrameBound::Unbounded)
                        } else {
                            Err("Expected PRECEDING or FOLLOWING after UNBOUNDED".to_string())
                        }
                    } else {
                        Err("Expected PRECEDING or FOLLOWING after UNBOUNDED".to_string())
                    }
                },
                "CURRENT" => {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                        if id == "ROW" {
                            self.advance();
                            Ok(FrameBound::Current)
                        } else {
                            Err("Expected ROW after CURRENT".to_string())
                        }
                    } else {
                        Err("Expected ROW after CURRENT".to_string())
                    }
                },
                _ => {
                    if let Some(Ok(Token::NUMBER(offset_str))) = self.current_token {
                        self.advance();
                        
                        let offset = offset_str.parse::<i64>().map_err(|_| "Invalid offset value".to_string())?;
                        
                        if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                            if id == "PRECEDING" || id == "FOLLOWING" {
                                self.advance();
                                Ok(FrameBound::Offset(offset))
                            } else {
                                Err("Expected PRECEDING or FOLLOWING after offset".to_string())
                            }
                        } else {
                            Err("Expected PRECEDING or FOLLOWING after offset".to_string())
                        }
                    } else {
                        Err("Expected frame bound specification".to_string())
                    }
                }
            }
        } else if let Some(Ok(Token::NUMBER(offset_str))) = self.current_token {
            self.advance();
            
            let offset = offset_str.parse::<i64>().map_err(|_| "Invalid offset value".to_string())?;
            
            if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "PRECEDING" || id == "FOLLOWING" {
                    self.advance();
                    Ok(FrameBound::Offset(offset))
                } else {
                    Err("Expected PRECEDING or FOLLOWING after offset".to_string())
                }
            } else {
                Err("Expected PRECEDING or FOLLOWING after offset".to_string())
            }
        } else {
            Err("Expected frame bound specification".to_string())
        }
    }
    
    fn parse_expr(&mut self) -> Result<Expr<'a>, String> {
        self.parse_simple_expr()
    }
    
    fn parse_simple_expr(&mut self) -> Result<Expr<'a>, String> {
        let left = self.parse_term()?;
        
        if let Some(Ok(token)) = &self.current_token {
            match token {
                Token::EQUALS => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::Equals,
                        right: Box::new(right),
                    });
                },
                Token::NOT_EQUAL => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::NotEquals,
                        right: Box::new(right),
                    });
                },
                Token::LESS_THAN => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::LessThan,
                        right: Box::new(right),
                    });
                },
                Token::GREATER_THAN => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::GreaterThan,
                        right: Box::new(right),
                    });
                },
                Token::LESS_THAN_EQUAL => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::LessThanOrEqual,
                        right: Box::new(right),
                    });
                },
                Token::GREATER_THAN_EQUAL => {
                    self.advance();
                    let right = self.parse_term()?;
                    return Ok(Expr::BinaryOp {
                        left: Box::new(left),
                        op: BinaryOperator::GreaterThanOrEqual,
                        right: Box::new(right),
                    });
                },
                _ => {}
            }
        }
        
        Ok(left)
    }
    
    fn parse_term(&mut self) -> Result<Expr<'a>, String> {
        match &self.current_token {
            Some(Ok(Token::IDENTIFIER(id))) => {
                let id = id.clone();
                self.advance();
                
                if let Some(Ok(Token::DOT)) = self.current_token {
                    self.advance();
                    
                    if let Some(Ok(Token::IDENTIFIER(column_name))) = self.current_token {
                        let column_name = column_name.clone();
                        self.advance();
                        Ok(Expr::Column {
                            schema_name: None,
                            table_name: Some(id),
                            column_name,
                        })
                    } else if let Some(Ok(Token::ASTERISK)) = self.current_token {
                        Err("table.* is not supported in an expression context".to_string())
                    } else {
                        Err("Expected column name or * after dot".to_string())
                    }
                } else {
                    Ok(Expr::Column {
                        schema_name: None,
                        table_name: None,
                        column_name: id,
                    })
                }
            },
            Some(Ok(Token::NUMBER(value))) => {
                let value = value.clone();
                self.advance();
                Ok(Expr::Literal(Literal::Numeric(value)))
            },
            Some(Ok(Token::STRING_LITERAL(value))) => {
                let value = value.clone();
                self.advance();
                Ok(Expr::Literal(Literal::String(value)))
            },
            Some(Ok(Token::ASTERISK)) => {
                self.advance();
                Err("* is not a valid expression in this context".to_string())
            },
            _ => {
                Err(format!("Unexpected token in expression: {:?}", self.current_token))
            }
        }
    }

    fn parse_order_by_clause(&mut self) -> Result<OrderByClause<'a>, String> {
        let mut terms = Vec::new();
        
        terms.push(self.parse_ordering_term()?);
        
        while let Some(Ok(Token::COMMA)) = self.current_token {
            self.advance();
            terms.push(self.parse_ordering_term()?);
        }
        
        Ok(OrderByClause { terms })
    }
    
    fn parse_ordering_term(&mut self) -> Result<OrderingTerm<'a>, String> {
        let expr = Box::new(self.parse_expr()?);
        
        let asc_desc = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            match id {
                "ASC" => {
                    self.advance();
                    Some(AscDesc::Asc)
                },
                "DESC" => {
                    self.advance();
                    Some(AscDesc::Desc)
                },
                _ => None,
            }
        } else {
            None
        };
        
        let nulls = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
            if id == "NULLS" {
                self.advance();
                
                if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                    self.advance();
                    
                    match id {
                        "FIRST" => Some(NullsOrder::First),
                        "LAST" => Some(NullsOrder::Last),
                        _ => return Err(format!("Expected FIRST or LAST after NULLS, got {}", id)),
                    }
                } else {
                    return Err("Expected FIRST or LAST after NULLS".to_string());
                }
            } else {
                None
            }
        } else {
            None
        };
        
        Ok(OrderingTerm {
            expr,
            asc_desc,
            nulls,
        })
    }

    fn parse_limit_clause(&mut self) -> Result<LimitClause<'a>, String> {
        if let Some(Ok(Token::NUMBER(limit_str))) = self.current_token {
            self.advance();
            
            let limit_expr = Expr::Literal(Literal::Numeric(limit_str));
            
            let offset = if let Some(Ok(Token::IDENTIFIER(id))) = self.current_token {
                if id == "OFFSET" {
                    self.advance();
                    
                    if let Some(Ok(Token::NUMBER(offset_str))) = self.current_token {
                        self.advance();
                        
                        let offset_expr = Expr::Literal(Literal::Numeric(offset_str));
                        Some(Box::new(offset_expr))
                    } else {
                        return Err("Expected numeric value for OFFSET".to_string());
                    }
                } else {
                    None
                }
            } else {
                None
            };
            
            Ok(LimitClause {
                limit: Box::new(limit_expr),
                offset,
            })
        } else {
            Err("Expected numeric value for LIMIT".to_string())
        }
    }

    fn parse_insert(&mut self) -> Result<Stmt<'a>, String> {
        Err("INSERT parsing not yet implemented".to_string())
    }
    
    fn parse_update(&mut self) -> Result<Stmt<'a>, String> {
        Err("UPDATE parsing not yet implemented".to_string())
    }
    
    fn parse_delete(&mut self) -> Result<Stmt<'a>, String> {
        Err("DELETE parsing not yet implemented".to_string())
    }
    
    fn parse_create(&mut self) -> Result<Stmt<'a>, String> {
        Err("CREATE parsing not yet implemented".to_string())
    }
    
    fn parse_alter(&mut self) -> Result<Stmt<'a>, String> {
        Err("ALTER parsing not yet implemented".to_string())
    }
    
    fn parse_drop(&mut self) -> Result<Stmt<'a>, String> {
        Err("DROP parsing not yet implemented".to_string())
    }
    
    fn parse_begin(&mut self) -> Result<Stmt<'a>, String> {
        Err("BEGIN parsing not yet implemented".to_string())
    }
    
    fn parse_commit(&mut self) -> Result<Stmt<'a>, String> {
        Err("COMMIT parsing not yet implemented".to_string())
    }
    
    fn parse_rollback(&mut self) -> Result<Stmt<'a>, String> {
        Err("ROLLBACK parsing not yet implemented".to_string())
    }
    
    fn parse_pragma(&mut self) -> Result<Stmt<'a>, String> {
        Err("PRAGMA parsing not yet implemented".to_string())
    }
    
    fn parse_analyze(&mut self) -> Result<Stmt<'a>, String> {
        Err("ANALYZE parsing not yet implemented".to_string())
    }
    
    fn parse_attach(&mut self) -> Result<Stmt<'a>, String> {
        Err("ATTACH parsing not yet implemented".to_string())
    }
    
    fn parse_detach(&mut self) -> Result<Stmt<'a>, String> {
        Err("DETACH parsing not yet implemented".to_string())
    }
    
    fn parse_vacuum(&mut self) -> Result<Stmt<'a>, String> {
        Err("VACUUM parsing not yet implemented".to_string())
    }
    
    fn parse_reindex(&mut self) -> Result<Stmt<'a>, String> {
        Err("REINDEX parsing not yet implemented".to_string())
    }
    
    fn parse_savepoint(&mut self) -> Result<Stmt<'a>, String> {
        Err("SAVEPOINT parsing not yet implemented".to_string())
    }
    
    fn parse_release(&mut self) -> Result<Stmt<'a>, String> {
        Err("RELEASE parsing not yet implemented".to_string())
    }
}

pub fn parse(input: &str) -> Result<Vec<Stmt>, String> {
    let mut parser = Parser::new(input);
    parser.parse_statements()
}

pub fn parse_statement(input: &str) -> Result<Stmt, String> {
    let mut parser = Parser::new(input);
    parser.parse_statement()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn print_ast(stmt: &Stmt) {
        match stmt {
            Stmt::Select(select_stmt) => {
                println!("SELECT Statement:");
                println!("  Distinct: {}", select_stmt.select_core.distinct);
                
                println!("  Result Columns:");
                for (i, column) in select_stmt.select_core.result_columns.iter().enumerate() {
                    match column {
                        ResultColumn::AllColumns => {
                            println!("    [{}] All Columns (*)", i);
                        },
                        ResultColumn::TableAllColumns(table_name) => {
                            println!("    [{}] Table All Columns: {}.* ", i, table_name);
                        },
                        ResultColumn::Expr { expr, alias } => {
                            print!("    [{}] Expression", i);
                            if let Some(alias_name) = alias {
                                print!(" AS {}", alias_name);
                            }
                            println!();
                            
                            match expr.as_ref() {
                                Expr::Column { schema_name, table_name, column_name } => {
                                    print!("      Column: ");
                                    if let Some(schema) = schema_name {
                                        print!("{}..", schema);
                                    }
                                    if let Some(table) = table_name {
                                        print!("{}.", table);
                                    }
                                    println!("{}", column_name);
                                },
                                Expr::Literal(lit) => {
                                    match lit {
                                        Literal::Numeric(val) => println!("      Literal: Number({})", val),
                                        Literal::String(val) => println!("      Literal: String(\"{}\")", val),
                                        _ => println!("      Literal: {:?}", lit),
                                    }
                                },
                                _ => println!("      Other expression type: {:?}", expr),
                            }
                        },
                    }
                }
                
                if let Some(from_clause) = &select_stmt.select_core.from_clause {
                    println!("  FROM Clause:");
                    for (i, table) in from_clause.tables.iter().enumerate() {
                        print_table_or_subquery(i, table);
                    }
                    
                    if !from_clause.join_clauses.is_empty() {
                        println!("    JOIN Clauses:");
                        for (i, join) in from_clause.join_clauses.iter().enumerate() {
                            println!("      [{}] Join Type: {:?}", i, join.join_type);
                            
                            print!("        Joined Table: ");
                            print_table_or_subquery_inline(&join.table_or_subquery);
                            println!();
                            
                            match &join.constraint {
                                JoinConstraint::On(expr) => {
                                    println!("        ON Condition: ");
                                    print_expr("          ", expr);
                                },
                                JoinConstraint::Using(columns) => {
                                    println!("        USING Columns: {}", columns.join(", "));
                                },
                            }
                        }
                    }
                }
                
                if let Some(where_clause) = &select_stmt.select_core.where_clause {
                    println!("  WHERE Clause:");
                    print_expr("    ", where_clause);
                }
                
                if let Some(group_by) = &select_stmt.select_core.group_by_clause {
                    println!("  GROUP BY Clause: {} expressions", group_by.exprs.len());
                    for (i, expr) in group_by.exprs.iter().enumerate() {
                        println!("    [{}] ", i);
                        print_expr("      ", expr);
                    }
                }
                
                if let Some(having) = &select_stmt.select_core.having_clause {
                    println!("  HAVING Clause:");
                    print_expr("    ", having);
                }
                
                if let Some(order_by) = &select_stmt.order_by_clause {
                    println!("  ORDER BY Clause: {} terms", order_by.terms.len());
                    for (i, term) in order_by.terms.iter().enumerate() {
                        print!("    [{}] ", i);
                        print_expr("      ", &term.expr);
                        
                        if let Some(asc_desc) = &term.asc_desc {
                            match asc_desc {
                                AscDesc::Asc => print!(" ASC"),
                                AscDesc::Desc => print!(" DESC"),
                            }
                        }
                        
                        if let Some(nulls) = &term.nulls {
                            match nulls {
                                NullsOrder::First => print!(" NULLS FIRST"),
                                NullsOrder::Last => print!(" NULLS LAST"),
                            }
                        }
                        println!();
                    }
                }
                
                if let Some(limit) = &select_stmt.limit_clause {
                    println!("  LIMIT Clause:");
                    print!("    Limit: ");
                    print_expr("", &limit.limit);
                    
                    if let Some(offset) = &limit.offset {
                        print!("    Offset: ");
                        print_expr("", offset);
                    }
                }
            },
            _ => println!("Non-SELECT statement: {:?}", stmt),
        }
    }
    
    fn print_table_or_subquery(index: usize, table: &TableOrSubquery) {
        match table {
            TableOrSubquery::Table { schema_name, table_name, alias, indexed_by, not_indexed } => {
                print!("    [{}] Table: ", index);
                if let Some(schema) = schema_name {
                    print!("{}.", schema);
                }
                print!("{}", table_name);
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
                if *not_indexed {
                    print!(" NOT INDEXED");
                } else if let Some(index) = indexed_by {
                    print!(" INDEXED BY {}", index);
                }
                println!();
            },
            TableOrSubquery::Subquery { select_stmt, alias } => {
                print!("    [{}] Subquery", index);
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
                println!();
            },
            TableOrSubquery::TableFunction { name, args, alias } => {
                print!("    [{}] Table Function: {}(", index, name);
                if !args.is_empty() {
                    print!("...)");
                } else {
                    print!(")");
                }
                
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
                println!();
            },
        }
    }
    
    fn print_table_or_subquery_inline(table: &TableOrSubquery) {
        match table {
            TableOrSubquery::Table { schema_name, table_name, alias, indexed_by, not_indexed } => {
                if let Some(schema) = schema_name {
                    print!("{}.", schema);
                }
                print!("{}", table_name);
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
                if *not_indexed {
                    print!(" NOT INDEXED");
                } else if let Some(index) = indexed_by {
                    print!(" INDEXED BY {}", index);
                }
            },
            TableOrSubquery::Subquery { alias, .. } => {
                print!("Subquery");
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
            },
            TableOrSubquery::TableFunction { name, alias, .. } => {
                print!("Table Function: {}", name);
                if let Some(alias_name) = alias {
                    print!(" AS {}", alias_name);
                }
            },
        }
    }
    
    fn print_expr(indent: &str, expr: &Expr) {
        match expr {
            Expr::Column { schema_name, table_name, column_name } => {
                print!("{}Column: ", indent);
                if let Some(schema) = schema_name {
                    print!("{}..", schema);
                }
                if let Some(table) = table_name {
                    print!("{}.", table);
                }
                println!("{}", column_name);
            },
            Expr::Literal(lit) => {
                match lit {
                    Literal::Numeric(val) => println!("{}Literal: Number({})", indent, val),
                    Literal::String(val) => println!("{}Literal: String(\"{}\")", indent, val),
                    _ => println!("{}Literal: {:?}", indent, lit),
                }
            },
            Expr::BinaryOp { left, op, right } => {
                println!("{}Binary Operation: {:?}", indent, op);
                print_expr(&format!("{}  ", indent), left);
                print_expr(&format!("{}  ", indent), right);
            },
            _ => println!("{}Other expression type: {:?}", indent, expr),
        }
    }

    #[test]
    fn test_parser_setup() {
        let input = "SELECT * FROM users;";
        let parser = Parser::new(input);
        assert_eq!(parser.input, input);
    }

    #[test]
    fn test_print_select_ast() {
        let queries = [
            "SELECT * FROM users;",
            "SELECT id, name FROM users;",
            "SELECT id AS user_id FROM users;",
            "SELECT users.* FROM users;",
            "SELECT * FROM users WHERE id = 1;",
            "SELECT * FROM users ORDER BY name;",
            "SELECT * FROM users LIMIT 10;",
            "SELECT u.id, u.name FROM users u JOIN orders o ON u.id = o.user_id;"
        ];
        
        println!("\n=== PRINTING AST STRUCTURES ===\n");
        
        for (i, query) in queries.iter().enumerate() {
            println!("\nQuery {}: {}", i+1, query);
            match parse_statement(query) {
                Ok(stmt) => {
                    print_ast(&stmt);
                },
                Err(err) => {
                    println!("Error parsing: {}", err);
                }
            }
            println!("{}", "-".repeat(50));
        }
    }

    #[test]
    fn test_simple_select() {
        let input = "SELECT * FROM users;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            assert!(!select_core.distinct);
            
            if let Some(from_clause) = &select_core.from_clause {
                assert_eq!(from_clause.tables.len(), 1);
                if let TableOrSubquery::Table { schema_name, table_name, .. } = &from_clause.tables[0] {
                    assert_eq!(schema_name, &None);
                    assert_eq!(table_name, &"users");
                } else {
                    panic!("Expected Table variant");
                }
            } else {
                panic!("Expected FROM clause");
            }
            
            assert_eq!(select_core.result_columns.len(), 1);
            match &select_core.result_columns[0] {
                ResultColumn::AllColumns => {},
                _ => panic!("Expected AllColumns variant"),
            }
        } else {
            panic!("Expected Select statement");
        }
    }
    
    #[test]
    fn test_select_with_columns() {
        let input = "SELECT id, name FROM users;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            
            assert_eq!(select_core.result_columns.len(), 2);
            
            if let ResultColumn::Expr { expr, alias } = &select_core.result_columns[0] {
                if let Expr::Column { column_name, .. } = &**expr {
                    assert_eq!(column_name, &"id");
                    assert_eq!(alias, &None);
                } else {
                    panic!("Expected Column variant");
                }
            } else {
                panic!("Expected Expr variant");
            }
            
            if let ResultColumn::Expr { expr, alias } = &select_core.result_columns[1] {
                if let Expr::Column { column_name, .. } = &**expr {
                    assert_eq!(column_name, &"name");
                    assert_eq!(alias, &None);
                } else {
                    panic!("Expected Column variant");
                }
            } else {
                panic!("Expected Expr variant");
            }
        } else {
            panic!("Expected Select statement");
        }
    }
    
    #[test]
    fn test_select_with_alias() {
        let input = "SELECT id AS user_id FROM users;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            
            if let ResultColumn::Expr { expr, alias } = &select_core.result_columns[0] {
                if let Expr::Column { column_name, .. } = &**expr {
                    assert_eq!(column_name, &"id");
                    assert_eq!(alias, &Some("user_id"));
                } else {
                    panic!("Expected Column variant");
                }
            } else {
                panic!("Expected Expr variant");
            }
        } else {
            panic!("Expected Select statement");
        }
    }
    
    #[test]
    fn test_select_with_where() {
        let input = "SELECT * FROM users WHERE id = 1;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            
            assert!(select_core.where_clause.is_some());
        } else {
            panic!("Expected Select statement");
        }
    }
    
    #[test]
    fn test_select_with_table_dot_star() {
        let input = "SELECT users.* FROM users;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            
            assert_eq!(select_core.result_columns.len(), 1);
            match &select_core.result_columns[0] {
                ResultColumn::TableAllColumns(table_name) => {
                    assert_eq!(table_name, &"users");
                },
                _ => panic!("Expected TableAllColumns variant"),
            }
        } else {
            panic!("Expected Select statement");
        }
    }

    #[test]
    fn test_join_with_on_clause() {
        let input = "SELECT u.id, u.name FROM users u JOIN orders o ON u.id = o.user_id;";
        let result = parse_statement(input);
        assert!(result.is_ok());
        
        if let Ok(Stmt::Select(select_stmt)) = result {
            let select_core = &select_stmt.select_core;
            
            let from_clause = select_core.from_clause.as_ref().expect("Expected FROM clause");
            
            assert_eq!(from_clause.tables.len(), 1);
            if let TableOrSubquery::Table { table_name, alias, .. } = &from_clause.tables[0] {
                assert_eq!(table_name, &"users");
                assert_eq!(alias, &Some("u"));
            } else {
                panic!("Expected Table variant");
            }
            
            assert_eq!(from_clause.join_clauses.len(), 1);
            let join = &from_clause.join_clauses[0];
            assert_eq!(join.join_type, JoinType::Inner);
            
            if let TableOrSubquery::Table { table_name, alias, .. } = &join.table_or_subquery {
                assert_eq!(table_name, &"orders");
                assert_eq!(alias, &Some("o"));
            } else {
                panic!("Expected Table variant for joined table");
            }
            
            if let JoinConstraint::On(expr) = &join.constraint {
                if let Expr::BinaryOp { left, op, right } = expr.as_ref() {
                    assert_eq!(*op, BinaryOperator::Equals);
                    
                    if let Expr::Column { table_name, column_name, .. } = left.as_ref() {
                        assert_eq!(table_name, &Some("u"));
                        assert_eq!(column_name, &"id");
                    } else {
                        panic!("Expected Column on left side of join condition");
                    }
                    
                    if let Expr::Column { table_name, column_name, .. } = right.as_ref() {
                        assert_eq!(table_name, &Some("o"));
                        assert_eq!(column_name, &"user_id");
                    } else {
                        panic!("Expected Column on right side of join condition");
                    }
                } else {
                    panic!("Expected BinaryOp in join condition");
                }
            } else {
                panic!("Expected ON constraint in join");
            }
            
            println!("Successfully validated JOIN with ON clause");
        } else {
            panic!("Expected Select statement");
        }
    }
}
