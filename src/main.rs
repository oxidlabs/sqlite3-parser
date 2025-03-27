use std::env;
use sqlite3_parser_logos::parser::{parse, print_ast};
fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: cargo run \"SELECT * FROM table\"");
        return;
    }
    let sql = &args[1];
    println!("Parsing SQL: {}\n", sql);
    match parse(sql) {
        Ok(statements) => {
            println!("Successfully parsed {} statement(s)!\n", statements.len());
            for (i, stmt) in statements.iter().enumerate() {
                println!("Statement {}:", i + 1);
                print_ast(stmt);
                println!();
            }
        },
        Err(error) => {
            println!("Error parsing SQL: {}", error);
        }
    }
} 