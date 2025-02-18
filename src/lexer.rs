use logos::{Logos, Span};

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\r\n\f]+")]
pub enum Token<'a> {
    // Types
    //
    // INTEGERS
    #[token("UNSIGNED BIG INT", ignore(case))]
    UINT,

    // TEXT
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

    // REAL
    #[token("DOUBLE PRECISION", ignore(case))]
    DOUBLE_PRECISION,

    #[regex(r"(?i:DECIMAL)\(\d+,\d+\)", extract_decimal)]
    Decimal((usize, usize)),

    // Commands
    #[token("SELECT DISTINCT")]
    SELECT_DISTINCT,

    #[token("GROUP BY")]
    GROUP_BY,

    #[token("LEFT JOIN")]
    LEFT_JOIN,

    #[token("ORDER BY")]
    ORDER_BY,

    // FUNCTIONS
    #[regex(r"(?i:abs)\([^)]*\)", extract_args_vec)]
    ABS(Vec<&'a str>),

    #[regex(r"(?i:changes)\([^)]*\)", extract_args_vec)]
    CHANGES(Vec<&'a str>),

    #[regex(r"(?i:char)\([^)]*\)", extract_args_vec)]
    CHAR(Vec<&'a str>),

    #[regex(r"(?i:coalesce)\([^)]*\)", extract_args_vec)]
    COALESCE(Vec<&'a str>),

    #[regex(r"(?i:concat)\([^)]*\)", extract_args_vec)]
    CONCAT(Vec<&'a str>),

    #[regex(r"(?i:concat_ws)\([^)]*\)", extract_args_vec)]
    CONCAT_WS(Vec<&'a str>),

    #[regex(r"(?i:format)\([^)]*\)", extract_args_vec)]
    FORMAT(Vec<&'a str>),

    #[regex(r"(?i:glob)\([^)]*\)", extract_args_vec)]
    GLOB(Vec<&'a str>),

    #[regex(r"(?i:hex)\([^)]*\)", extract_args_vec)]
    HEX(Vec<&'a str>),

    #[regex(r"(?i:if)\([^)]*\)", extract_args_vec)]
    IF(Vec<&'a str>),

    #[regex(r"(?i:ifnull)\([^)]*\)", extract_args_vec)]
    IFNULL(Vec<&'a str>),

    #[regex(r"(?i:iif)\([^)]*\)", extract_args_vec)]
    IIF(Vec<&'a str>),

    #[regex(r"(?i:instr)\([^)]*\)", extract_args_vec)]
    INSTR(Vec<&'a str>),

    #[regex(r"(?i:last_insert_rowid)\([^)]*\)", extract_args_vec)]
    LAST_INSERT_ROWID(Vec<&'a str>),

    #[regex(r"(?i:length)\([^)]*\)", extract_args_vec)]
    LENGTH(Vec<&'a str>),

    #[regex(r"(?i:like)\([^)]*\)", extract_args_vec)]
    LIKE_FUNCTION(Vec<&'a str>),

    #[regex(r"(?i:likelihood)\([^)]*\)", extract_args_vec)]
    LIKELIHOOD(Vec<&'a str>),

    #[regex(r"(?i:likely)\([^)]*\)", extract_args_vec)]
    LIKELY(Vec<&'a str>),

    #[regex(r"(?i:load_extension)\([^)]*\)", extract_args_vec)]
    LOAD_EXTENSION(Vec<&'a str>),

    #[regex(r"(?i:lower)\([^)]*\)", extract_args_vec)]
    LOWER(Vec<&'a str>),

    #[regex(r"(?i:ltrim)\([^)]*\)", extract_args_vec)]
    LTRIM(Vec<&'a str>),

    #[regex(r"(?i:max)\([^)]*\)", extract_args_vec)]
    MAX(Vec<&'a str>),

    #[regex(r"(?i:min)\([^)]*\)", extract_args_vec)]
    MIN(Vec<&'a str>),

    #[regex(r"(?i:nullif)\([^)]*\)", extract_args_vec)]
    NULLIF(Vec<&'a str>),

    #[regex(r"(?i:octet_length)\([^)]*\)", extract_args_vec)]
    OCTET_LENGTH(Vec<&'a str>),

    #[regex(r"(?i:printf)\([^)]*\)", extract_args_vec)]
    PRINTF(Vec<&'a str>),

    #[regex(r"(?i:quote)\([^)]*\)", extract_args_vec)]
    QUOTE(Vec<&'a str>),

    #[regex(r"(?i:random)\([^)]*\)", extract_args_vec)]
    RANDOM(Vec<&'a str>),

    #[regex(r"(?i:randomblob)\([^)]*\)", extract_args_vec)]
    RANDOMBLOB(Vec<&'a str>),

    #[regex(r"(?i:replace)\([^)]*\)", extract_args_vec)]
    REPLACE(Vec<&'a str>),

    #[regex(r"(?i:round)\([^)]*\)", extract_args_vec)]
    ROUND(Vec<&'a str>),

    #[regex(r"(?i:rtrim)\([^)]*\)", extract_args_vec)]
    RTRIM(Vec<&'a str>),

    #[regex(r"(?i:sign)\([^)]*\)", extract_args_vec)]
    SIGN(Vec<&'a str>),

    #[regex(r"(?i:soundex)\([^)]*\)", extract_args_vec)]
    SOUNDEX(Vec<&'a str>),

    #[regex(r"(?i:sqlite_compileoption_get)\([^)]*\)", extract_args_vec)]
    SQLITE_COMPILEOPTION_GET(Vec<&'a str>),

    #[regex(r"(?i:sqlite_compileoption_used)\([^)]*\)", extract_args_vec)]
    SQLITE_COMPILEOPTION_USED(Vec<&'a str>),

    #[regex(r"(?i:sqlite_offset)\([^)]*\)", extract_args_vec)]
    SQLITE_OFFSET(Vec<&'a str>),

    #[regex(r"(?i:sqlite_source_id)\([^)]*\)", extract_args_vec)]
    SQLITE_SOURCE_ID(Vec<&'a str>),

    #[regex(r"(?i:sqlite_version)\([^)]*\)", extract_args_vec)]
    SQLITE_VERSION(Vec<&'a str>),

    #[regex(r"(?i:substr)\([^)]*\)", extract_args_vec)]
    SUBSTR(Vec<&'a str>),

    #[regex(r"(?i:substring)\([^)]*\)", extract_args_vec)]
    SUBSTRING(Vec<&'a str>),

    #[regex(r"(?i:total_changes)\([^)]*\)", extract_args_vec)]
    TOTAL_CHANGES(Vec<&'a str>),

    #[regex(r"(?i:trim)\([^)]*\)", extract_args_vec)]
    TRIM(Vec<&'a str>),

    #[regex(r"(?i:typeof)\([^)]*\)", extract_args_vec)]
    TYPEOF(Vec<&'a str>),

    #[regex(r"(?i:unhex)\([^)]*\)", extract_args_vec)]
    UNHEX(Vec<&'a str>),

    #[regex(r"(?i:unicode)\([^)]*\)", extract_args_vec)]
    UNICODE(Vec<&'a str>),

    #[regex(r"(?i:unlikely)\([^)]*\)", extract_args_vec)]
    UNLIKELY(Vec<&'a str>),

    #[regex(r"(?i:upper)\([^)]*\)", extract_args_vec)]
    UPPER(Vec<&'a str>),

    #[regex(r"(?i:zeroblob)\([^)]*\)", extract_args_vec)]
    ZEROBLOB(Vec<&'a str>),

    // VALUES
    #[regex(r"\([^)]*\)", extract_values)]
    VALUE_VECTOR(Vec<&'a str>),

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

    // SEPERATORS
    #[token(",")]
    COMMA,

    #[token("=")]
    EQUALS,

    #[token(";")]
    SEMICOLON,

    #[regex(r"[+-]?\d+(\.\d+)?")]
    NUMBER(&'a str),
}

#[allow(non_camel_case_types)]
#[derive(Logos, Debug, PartialEq)]
pub enum Keywords {
    #[token("TEXT", ignore(case))]
    TEXT,

    #[token("CLOB", ignore(case))]
    CLOB,

    #[token("BLOB", ignore(case))]
    BLOB,

    // REAL
    #[token("REAL", ignore(case))]
    REAL,

    #[token("DOUBLE", ignore(case))]
    DOUBLE,

    #[token("FLOAT", ignore(case))]
    FLOAT,

    // NUMERIC
    #[token("NUMERIC", ignore(case))]
    NUMERIC,

    #[token("BOOLEAN", ignore(case))]
    BOOLEAN,

    #[token("DATE", ignore(case))]
    DATE,

    #[token("DATETIME", ignore(case))]
    DATE_TIME,

    // Commands
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

    // CASE COMMANDS
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

fn extract_args_vec<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Vec<&'a str> {
    let slice = lex.slice();
    let start = slice.find('(').unwrap() + 1;
    let end = slice.rfind(')').unwrap();
    let inner = &slice[start..end];
    if inner.trim().is_empty() {
        Vec::new()
    } else {
        inner.split(',').map(|s| s.trim()).collect()
    }
}

fn extract_values<'a>(lex: &mut logos::Lexer<'a, Token<'a>>) -> Vec<&'a str> {
    let slice = lex.slice();

    let inner = &slice[1..slice.len() - 1];
    inner
        .split(',')
        .map(|s| {
            let s = s.trim();
            if s.starts_with('"') {
                s.strip_prefix('"').unwrap().strip_suffix('"').unwrap()
            } else if s.starts_with("\'") {
                s.strip_prefix('\'').unwrap().strip_suffix('\'').unwrap()
            } else {
                s
            }
        })
        .collect()
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
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::VALUE_VECTOR(vec![
                "CustomerName",
                "City",
                "Country"
            ])))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::IDENTIFIER("VALUES"))));
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::VALUE_VECTOR(vec![
                "Cardinal",
                "Stavanger",
                "Norway"
            ])))
        );
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
}
