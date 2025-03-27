use logos::Logos;
use std::borrow::Cow;
#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'a> {
    #[token("UNSIGNED BIG INT", ignore(case))]
    UINT,
    #[regex(r"(?i:CHARACTER)\(\d+\)", extract_size)]
    CHARACTER(&'a str),
    #[regex(r"(?i:VARCHAR)\(\d+\)", extract_size)]
    VARCHAR(&'a str),
    #[regex(r"(?i:VARYING\s+CHARACTER)\(\d+\)", extract_size)]
    VARYING_CHARACTER(&'a str),
    #[regex(r"(?i:NCHAR)\(\d+\)", extract_size)]
    NCHAR(&'a str),
    #[regex(r"(?i:NATIVE\s+CHARACTER)\(\d+\)", extract_size)]
    NATIVE_CHARACTER(&'a str),
    #[regex(r"(?i:NVARCHAR)\(\d+\)", extract_size)]
    NVARCHAR(&'a str),
    #[token("DOUBLE PRECISION", ignore(case))]
    DOUBLE_PRECISION,
    #[regex(r"(?i:DECIMAL)\(\d+,\d+\)", extract_decimal)]
    Decimal((usize, usize)),
    #[token("SELECT DISTINCT")]
    SELECT_DISTINCT,
    #[token("GROUP BY")]
    GROUP_BY,
    #[token("LEFT JOIN")]
    LEFT_JOIN,
    #[token("INNER JOIN")]
    INNER_JOIN,
    #[token("ORDER BY")]
    ORDER_BY,
    #[regex(
        r"(?i:[A-Za-z_][A-Za-z0-9_]*\((?:[^()]+|\((?:[^()]+|\([^()]*\))*\))*\))",
        extract_function
    )]
    FUNCTION((&'a str, Cow<'a, str>)),
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice())]
    IDENTIFIER(&'a str),
    #[regex(r#"["']([^"'\\]|\\.)*["']"#, |lex| {
        let slice = lex.slice();
        if slice.starts_with('"') {
            slice.strip_prefix('"').unwrap().strip_suffix('"').unwrap()
        } else {
            slice.strip_prefix('\'').unwrap().strip_suffix('\'').unwrap()
        }
    })]
    STRING_LITERAL(&'a str),
    #[token(",")]
    COMMA,
    #[token("=")]
    EQUALS,
    #[token("<")]
    LESS_THAN,
    #[token(">")]
    GREATER_THAN,
    #[token("<=")]
    LESS_THAN_EQUAL,
    #[token(">=")]
    GREATER_THAN_EQUAL,
    #[token("<>")]
    #[token("!=")]
    NOT_EQUAL,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("/")]
    SLASH,
    #[token("%")]
    PERCENT,
    #[token(";")]
    SEMICOLON,
    #[token("*")]
    ASTERISK,
    #[token(".")]
    DOT,
    #[token("(")]
    LEFT_PAREN,
    #[token(")")]
    RIGHT_PAREN,
    #[regex(r"[+-]?\d+(\.\d+)?")]
    NUMBER(&'a str),
}
#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Keywords {
    #[token("TEXT", ignore(case))]
    TEXT,
    #[token("CLOB", ignore(case))]
    CLOB,
    #[token("BLOB", ignore(case))]
    BLOB,
    #[token("REAL", ignore(case))]
    REAL,
    #[token("DOUBLE", ignore(case))]
    DOUBLE,
    #[token("FLOAT", ignore(case))]
    FLOAT,
    #[token("NUMERIC", ignore(case))]
    NUMERIC,
    #[token("BOOLEAN", ignore(case))]
    BOOLEAN,
    #[token("DATE", ignore(case))]
    DATE,
    #[token("DATETIME", ignore(case))]
    DATE_TIME,
    #[token("SELECT")]
    SELECT,
    #[token("CREATE")]
    CREATE,
    #[token("UPDATE")]
    UPDATE,
    #[token("DELETE")]
    DELETE,
    #[token("INSERT")]
    INSERT,
    #[token("SET")]
    SET,
    #[token("WITH")]
    WITH,
    #[token("JOIN")]
    JOIN,
    #[token("ON")]
    ON,
    #[token("NULL")]
    NULL,
    #[token("LIKE")]
    LIKE,
    #[token("LIMIT")]
    LIMIT,
    #[token("OR")]
    OR,
    #[token("ASC")]
    ASCENDING,
    #[token("DESC")]
    DESCENDING,
    #[token("FROM")]
    FROM,
    #[token("WHERE")]
    WHERE,
    #[token("AS")]
    AS,
    #[token("CASE")]
    CASE,
    #[token("WHEN")]
    WHEN,
    #[token("THEN")]
    THEN,
    #[token("END")]
    END,
    #[token("INT", ignore(case))]
    #[token("INTEGER", ignore(case))]
    INT,
    #[token("TINYINT", ignore(case))]
    TINYINT,
    #[token("SMALLINT", ignore(case))]
    SMALLINT,
    #[token("MEDIUMINT", ignore(case))]
    MEDIUMINT,
    #[token("BIGINT", ignore(case))]
    BIGINT,
    #[token("INT2", ignore(case))]
    INT2,
    #[token("INT8", ignore(case))]
    INT8,
    #[token("VALUES")]
    VALUES,
}
fn collapse_whitespace(s: &str) -> Cow<str> {
    let mut iter = s.split_whitespace();
    if let Some(first) = iter.next() {
        let mut result = first.to_string();
        for word in iter {
            result.push(' ');
            result.push_str(word);
        }
        if result == s {
            Cow::Borrowed(s)
        } else {
            Cow::Owned(result)
        }
    } else {
        Cow::Borrowed(s)
    }
}
fn extract_function<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> (&'a str, Cow<'a, str>) {
    let slice = lex.slice();
    let start = slice.find('(').unwrap();
    let end = slice.rfind(')').unwrap();
    let name = slice[..start].trim();
    let inner = slice[start + 1..end].trim();
    let inner_clean = collapse_whitespace(inner);
    (name, inner_clean)
}
fn extract_decimal<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> (usize, usize) {
    let slice = lex.slice();
    let start = slice.find('(').expect("Expected '(' in DECIMAL");
    let comma = slice.find(',').expect("Expected ',' in DECIMAL");
    let end = slice.find(')').expect("Expected ')' in DECIMAL");
    let precision_str = &slice[start + 1..comma];
    let scale_str = &slice[comma + 1..end];
    let precision = precision_str
        .parse::<usize>()
        .expect("Failed to parse precision");
    let scale = scale_str.parse::<usize>().expect("Failed to parse scale");
    (precision, scale)
}
fn extract_size<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> &'a str {
    let slice = lex.slice();
    let start = slice.find('(').expect("Expected '('");
    let end = slice.find(')').expect("Expected ')'");
    &slice[start + 1..end]
}
#[cfg(test)]
mod tests {
    use super::*;
    use logos::Logos;
    #[test]
    fn test_select_query() {
        let query = "SELECT CustomerName, City FROM Customers WHERE Country = 'Mexico';";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("SELECT"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("CustomerName"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("City"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("FROM"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("WHERE"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Country"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(lexer.next(), Some(Ok(Token::STRING_LITERAL("Mexico"))));
        assert_eq!(lexer.next(), Some(Ok(Token::SEMICOLON)));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_insert_query() {
        let query = "INSERT INTO Customers (CustomerName, City, Country) VALUES ('Cardinal', 'Stavanger', 'Norway');";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("INSERT"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("INTO"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::LEFT_PAREN)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("CustomerName"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("City"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Country"))));
        assert_eq!(lexer.next(), Some(Ok(Token::RIGHT_PAREN)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("VALUES"))));
        assert_eq!(lexer.next(), Some(Ok(Token::LEFT_PAREN)));
        assert_eq!(lexer.next(), Some(Ok(Token::STRING_LITERAL("Cardinal"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::STRING_LITERAL("Stavanger"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::STRING_LITERAL("Norway"))));
        assert_eq!(lexer.next(), Some(Ok(Token::RIGHT_PAREN)));
        assert_eq!(lexer.next(), Some(Ok(Token::SEMICOLON)));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_update_query() {
        let query = "UPDATE Customers SET ContactName = 'Alfred Schmidt', City = 'Hamburg' WHERE CustomerID = 1;";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("UPDATE"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("SET"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("ContactName"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::STRING_LITERAL("Alfred Schmidt")))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("City"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(lexer.next(), Some(Ok(Token::STRING_LITERAL("Hamburg"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("WHERE"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("CustomerID"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(lexer.next(), Some(Ok(Token::NUMBER("1"))));
        assert_eq!(lexer.next(), Some(Ok(Token::SEMICOLON)));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_delete_query() {
        let query = "DELETE FROM Customers WHERE CustomerName = 'Alfreds Futterkiste';";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("DELETE"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("FROM"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("Customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("WHERE"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("CustomerName"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::STRING_LITERAL("Alfreds Futterkiste")))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::SEMICOLON)));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_function_call() {
        let query = "SELECT ABS(-5) AS AbsoluteValue;";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("SELECT"))));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::FUNCTION(("ABS", Cow::Borrowed("-5")))))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("AS"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("AbsoluteValue"))));
        assert_eq!(lexer.next(), Some(Ok(Token::SEMICOLON)));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_select_distinct() {
        let query = "SELECT DISTINCT name FROM users";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::SELECT_DISTINCT)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("name"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("FROM"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("users"))));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_left_join() {
        let query = "SELECT * FROM orders LEFT JOIN customers ON orders.customer_id = customers.id";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("SELECT"))));
        assert_eq!(lexer.next(), Some(Ok(Token::ASTERISK)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("FROM"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("orders"))));
        assert_eq!(lexer.next(), Some(Ok(Token::LEFT_JOIN)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("ON"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("orders"))));
        assert_eq!(lexer.next(), Some(Ok(Token::DOT)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("customer_id"))));
        assert_eq!(lexer.next(), Some(Ok(Token::EQUALS)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("customers"))));
        assert_eq!(lexer.next(), Some(Ok(Token::DOT)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("id"))));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_group_by_order_by() {
        let query =
            "SELECT category, COUNT(*) FROM products GROUP BY category ORDER BY COUNT(*) DESC";
        let mut lexer = Token::lexer(query);
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("SELECT"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("category"))));
        assert_eq!(lexer.next(), Some(Ok(Token::COMMA)));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::FUNCTION(("COUNT", Cow::Borrowed("*")))))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("FROM"))));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("products"))));
        assert_eq!(lexer.next(), Some(Ok(Token::GROUP_BY)));
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("category"))));
        assert_eq!(lexer.next(), Some(Ok(Token::ORDER_BY)));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::FUNCTION(("COUNT", Cow::Borrowed("*")))))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("DESC"))));
        assert_eq!(lexer.next(), None);
    }
    #[test]
    fn test_complex_sql_query() {
        let query = r"
        WITH DepartmentSummary AS (
    SELECT 
        d.department_id,
        d.department_name,
        COUNT(e.employee_id) AS employee_count,
        AVG(s.salary) AS avg_salary,
        SUM(CASE WHEN e.hire_date > DATEADD(year, -5, GETDATE()) 
                 THEN 1 ELSE 0 END) AS recent_hires,
        RANK() OVER (ORDER BY SUM(s.salary) DESC) AS dept_salary_rank
    FROM departments d
    LEFT JOIN employees e ON d.department_id = e.department_id
    INNER JOIN salaries s ON e.employee_id = s.employee_id
    WHERE s.effective_date = (
        SELECT MAX(s2.effective_date)
        FROM salaries s2
        WHERE s2.employee_id = s.employee_id
    )
    GROUP BY d.department_id, d.department_name
    HAVING AVG(s.salary) > (
        SELECT AVG(s3.salary) * 0.8
        FROM salaries s3
        WHERE s3.effective_date > DATEADD(year, -2, GETDATE())
    )
),
ProjectBudgetAnalysis AS (
    SELECT 
        p.project_id,
        p.project_name,
        p.budget,
        p.department_id,
        (SELECT COUNT(*) 
         FROM project_assignments pa 
         WHERE pa.project_id = p.project_id) AS team_size,
        COALESCE(SUM(pa.hours_worked * e.hourly_rate), 0) AS estimated_cost,
        p.budget - COALESCE(SUM(pa.hours_worked * e.hourly_rate), 0) AS budget_variance
    FROM projects p
    LEFT JOIN project_assignments pa ON p.project_id = pa.project_id
    LEFT JOIN employees e ON pa.employee_id = e.employee_id
    GROUP BY p.project_id, p.project_name, p.budget, p.department_id
)
SELECT 
    ds.department_name,
    ds.avg_salary,
    ds.dept_salary_rank,
    pba.project_name,
    pba.budget_variance,
    ROW_NUMBER() OVER (
        PARTITION BY ds.department_id 
        ORDER BY pba.budget_variance DESC
    ) AS project_rank,
    (SELECT STRING_AGG(e.first_name + ' ' + e.last_name, ', ')
     FROM employees e
     WHERE e.department_id = ds.department_id
     AND e.employee_id IN (
         SELECT TOP 3 pa.employee_id
         FROM project_assignments pa
         WHERE pa.project_id = pba.project_id
         ORDER BY pa.hours_worked DESC
     )) AS top_performers,
    CASE 
        WHEN pba.budget_variance < 0 THEN 'Over Budget'
        WHEN pba.budget_variance BETWEEN 0 AND 10000 THEN 'On Track'
        ELSE 'Under Budget'
    END AS budget_status
FROM DepartmentSummary ds
INNER JOIN ProjectBudgetAnalysis pba ON ds.department_id = pba.department_id
WHERE ds.department_id IN (
    SELECT department_id
    FROM employees
    GROUP BY department_id
    HAVING COUNT(DISTINCT job_id) > 5
)
AND EXISTS (
    SELECT 1
    FROM employee_skills es
    WHERE es.employee_id IN (
        SELECT employee_id
        FROM project_assignments pa
        WHERE pa.project_id = pba.project_id
    )
    AND es.skill_id IN (123, 456, 789)
)
ORDER BY 
    ds.dept_salary_rank,
    project_rank,
    CASE budget_status
        WHEN 'Over Budget' THEN 1
        WHEN 'On Track' THEN 2
        ELSE 3
    END;
        ";
        let mut lexer = Token::lexer(query);
        let pattern = [
            Token::IDENTIFIER("WITH"),
            Token::IDENTIFIER("DepartmentSummary"),
            Token::IDENTIFIER("AS"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("d"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::COMMA,
            Token::IDENTIFIER("d"),
            Token::DOT,
            Token::IDENTIFIER("department_name"),
            Token::COMMA,
            Token::FUNCTION(("COUNT", Cow::Borrowed("e.employee_id"))),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("employee_count"),
            Token::COMMA,
            Token::FUNCTION(("AVG", Cow::Borrowed("s.salary"))),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("avg_salary"),
            Token::COMMA,
            Token::FUNCTION((
                "SUM",
                Cow::Borrowed(
                    "CASE WHEN e.hire_date > DATEADD(year, -5, GETDATE()) THEN 1 ELSE 0 END",
                ),
            )),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("recent_hires"),
            Token::COMMA,
            Token::FUNCTION(("RANK", Cow::Borrowed(""))),
            Token::IDENTIFIER("OVER"),
            Token::LEFT_PAREN,
            Token::ORDER_BY,
            Token::FUNCTION(("SUM", Cow::Borrowed("s.salary"))),
            Token::IDENTIFIER("DESC"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("dept_salary_rank"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("departments"),
            Token::IDENTIFIER("d"),
            Token::LEFT_JOIN,
            Token::IDENTIFIER("employees"),
            Token::IDENTIFIER("e"),
            Token::IDENTIFIER("ON"),
            Token::IDENTIFIER("d"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::EQUALS,
            Token::IDENTIFIER("e"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::INNER_JOIN,
            Token::IDENTIFIER("salaries"),
            Token::IDENTIFIER("s"),
            Token::IDENTIFIER("ON"),
            Token::IDENTIFIER("e"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::EQUALS,
            Token::IDENTIFIER("s"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("s"),
            Token::DOT,
            Token::IDENTIFIER("effective_date"),
            Token::EQUALS,
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::FUNCTION(("MAX", Cow::Borrowed("s2.effective_date"))),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("salaries"),
            Token::IDENTIFIER("s2"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("s2"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::EQUALS,
            Token::IDENTIFIER("s"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::RIGHT_PAREN,
            Token::GROUP_BY,
            Token::IDENTIFIER("d"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::COMMA,
            Token::IDENTIFIER("d"),
            Token::DOT,
            Token::IDENTIFIER("department_name"),
            Token::IDENTIFIER("HAVING"),
            Token::FUNCTION(("AVG", Cow::Borrowed("s.salary"))),
            Token::GREATER_THAN,
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::FUNCTION(("AVG", Cow::Borrowed("s3.salary"))),
            Token::ASTERISK,
            Token::NUMBER("0.8"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("salaries"),
            Token::IDENTIFIER("s3"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("s3"),
            Token::DOT,
            Token::IDENTIFIER("effective_date"),
            Token::GREATER_THAN,
            Token::FUNCTION(("DATEADD", Cow::Borrowed("year, -2, GETDATE()"))),
            Token::RIGHT_PAREN,
            Token::RIGHT_PAREN,
            Token::COMMA,
            Token::IDENTIFIER("ProjectBudgetAnalysis"),
            Token::IDENTIFIER("AS"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_name"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("budget"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::COMMA,
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::FUNCTION(("COUNT", Cow::Borrowed("*"))),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("project_assignments"),
            Token::IDENTIFIER("pa"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::EQUALS,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("team_size"),
            Token::COMMA,
            Token::FUNCTION((
                "COALESCE",
                Cow::Borrowed("SUM(pa.hours_worked * e.hourly_rate), 0"),
            )),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("estimated_cost"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("budget"),
            Token::MINUS,
            Token::FUNCTION((
                "COALESCE",
                Cow::Borrowed("SUM(pa.hours_worked * e.hourly_rate), 0"),
            )),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("budget_variance"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("projects"),
            Token::IDENTIFIER("p"),
            Token::LEFT_JOIN,
            Token::IDENTIFIER("project_assignments"),
            Token::IDENTIFIER("pa"),
            Token::IDENTIFIER("ON"),
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::EQUALS,
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::LEFT_JOIN,
            Token::IDENTIFIER("employees"),
            Token::IDENTIFIER("e"),
            Token::IDENTIFIER("ON"),
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::EQUALS,
            Token::IDENTIFIER("e"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::GROUP_BY,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("project_name"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("budget"),
            Token::COMMA,
            Token::IDENTIFIER("p"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("department_name"),
            Token::COMMA,
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("avg_salary"),
            Token::COMMA,
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("dept_salary_rank"),
            Token::COMMA,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("project_name"),
            Token::COMMA,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("budget_variance"),
            Token::COMMA,
            Token::FUNCTION(("ROW_NUMBER", Cow::Borrowed(""))),
            Token::IDENTIFIER("OVER"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("PARTITION"),
            Token::IDENTIFIER("BY"),
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::ORDER_BY,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("budget_variance"),
            Token::IDENTIFIER("DESC"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("project_rank"),
            Token::COMMA,
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::FUNCTION((
                "STRING_AGG",
                Cow::Borrowed("e.first_name + ' ' + e.last_name, ', '"),
            )),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("employees"),
            Token::IDENTIFIER("e"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("e"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::EQUALS,
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::IDENTIFIER("AND"),
            Token::IDENTIFIER("e"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::IDENTIFIER("IN"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("TOP"),
            Token::NUMBER("3"),
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("project_assignments"),
            Token::IDENTIFIER("pa"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::EQUALS,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::ORDER_BY,
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("hours_worked"),
            Token::IDENTIFIER("DESC"),
            Token::RIGHT_PAREN,
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("top_performers"),
            Token::COMMA,
            Token::IDENTIFIER("CASE"),
            Token::IDENTIFIER("WHEN"),
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("budget_variance"),
            Token::LESS_THAN,
            Token::NUMBER("0"),
            Token::IDENTIFIER("THEN"),
            Token::STRING_LITERAL("Over Budget"),
            Token::IDENTIFIER("WHEN"),
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("budget_variance"),
            Token::IDENTIFIER("BETWEEN"),
            Token::NUMBER("0"),
            Token::IDENTIFIER("AND"),
            Token::NUMBER("10000"),
            Token::IDENTIFIER("THEN"),
            Token::STRING_LITERAL("On Track"),
            Token::IDENTIFIER("ELSE"),
            Token::STRING_LITERAL("Under Budget"),
            Token::IDENTIFIER("END"),
            Token::IDENTIFIER("AS"),
            Token::IDENTIFIER("budget_status"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("DepartmentSummary"),
            Token::IDENTIFIER("ds"),
            Token::INNER_JOIN,
            Token::IDENTIFIER("ProjectBudgetAnalysis"),
            Token::IDENTIFIER("pba"),
            Token::IDENTIFIER("ON"),
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::EQUALS,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("department_id"),
            Token::IDENTIFIER("IN"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("department_id"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("employees"),
            Token::GROUP_BY,
            Token::IDENTIFIER("department_id"),
            Token::IDENTIFIER("HAVING"),
            Token::FUNCTION(("COUNT", Cow::Borrowed("DISTINCT job_id"))),
            Token::GREATER_THAN,
            Token::NUMBER("5"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AND"),
            Token::IDENTIFIER("EXISTS"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::NUMBER("1"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("employee_skills"),
            Token::IDENTIFIER("es"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("es"),
            Token::DOT,
            Token::IDENTIFIER("employee_id"),
            Token::IDENTIFIER("IN"),
            Token::LEFT_PAREN,
            Token::IDENTIFIER("SELECT"),
            Token::IDENTIFIER("employee_id"),
            Token::IDENTIFIER("FROM"),
            Token::IDENTIFIER("project_assignments"),
            Token::IDENTIFIER("pa"),
            Token::IDENTIFIER("WHERE"),
            Token::IDENTIFIER("pa"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::EQUALS,
            Token::IDENTIFIER("pba"),
            Token::DOT,
            Token::IDENTIFIER("project_id"),
            Token::RIGHT_PAREN,
            Token::IDENTIFIER("AND"),
            Token::IDENTIFIER("es"),
            Token::DOT,
            Token::IDENTIFIER("skill_id"),
            Token::IDENTIFIER("IN"),
            Token::LEFT_PAREN,
            Token::NUMBER("123"),
            Token::COMMA,
            Token::NUMBER("456"),
            Token::COMMA,
            Token::NUMBER("789"),
            Token::RIGHT_PAREN,
            Token::RIGHT_PAREN,
            Token::ORDER_BY,
            Token::IDENTIFIER("ds"),
            Token::DOT,
            Token::IDENTIFIER("dept_salary_rank"),
            Token::COMMA,
            Token::IDENTIFIER("project_rank"),
            Token::COMMA,
            Token::IDENTIFIER("CASE"),
            Token::IDENTIFIER("budget_status"),
            Token::IDENTIFIER("WHEN"),
            Token::STRING_LITERAL("Over Budget"),
            Token::IDENTIFIER("THEN"),
            Token::NUMBER("1"),
            Token::IDENTIFIER("WHEN"),
            Token::STRING_LITERAL("On Track"),
            Token::IDENTIFIER("THEN"),
            Token::NUMBER("2"),
            Token::IDENTIFIER("ELSE"),
            Token::NUMBER("3"),
            Token::IDENTIFIER("END"),
            Token::SEMICOLON,
        ];
        for token_match in pattern {
            assert_eq!(lexer.next(), Some(Ok(token_match)));
        }
    }
}
