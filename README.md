# SQLite3 Parser with Logos

A high-performance SQLite3 parser implemented in Rust using the Logos lexer.

## Overview

This project implements a parser for SQLite3 SQL statements, focusing on high performance and memory efficiency. It uses the Logos lexer for tokenization and implements a recursive descent parser to produce an abstract syntax tree (AST).

## Features

- Fast lexical analysis with Logos
- Comprehensive SQLite3 syntax support
- Memory-efficient parsing
- Clear and well-structured AST

## Usage

```rust
use sqlite3_parser_logos::parse_statement;

fn main() {
    let sql = "SELECT id, name FROM users WHERE age > 18;";
    
    match parse_statement(sql) {
        Ok(stmt) => println!("Successfully parsed: {:?}", stmt),
        Err(err) => println!("Parse error: {}", err),
    }
}
```

## Benchmarks

The project includes benchmarks to compare performance against other SQL parsers.

### Running Benchmarks

```bash
# Run all benchmarks
cargo bench

# Analyze and display results in a table
cargo run --bin analyze_results
```

### Benchmark Types

1. **Query Complexity Benchmarks**: Tests parsing performance on queries of increasing complexity
2. **Batch Processing Benchmarks**: Measures throughput when parsing multiple queries
3. **Repeated Parsing Benchmarks**: Tests performance when parsing the same query repeatedly

### Comparing with Other Parsers

The benchmarks compare this parser with [sqlparser-rs](https://github.com/sqlparser-rs/sqlparser-rs), another popular SQL parser in Rust.

## License

This project is licensed under the MIT License - see the LICENSE file for details. 