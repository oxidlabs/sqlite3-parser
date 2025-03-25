use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use sqlite3_parser_logos::parse_statement;
use sqlparser::dialect::SQLiteDialect;
use sqlparser::parser::Parser as SqlparserParser;

fn bench_parsers(c: &mut Criterion) {
    let mut group = c.benchmark_group("SQLite Parsers");
    
    // Define test SQL queries of increasing complexity
    let test_queries = [
        // Simple queries
        "SELECT * FROM users;",
        "SELECT id, name, email FROM users WHERE age > 18;",
        
        // Medium complexity with JOIN and ORDER BY
        "SELECT u.id, u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE o.amount > 100.0 ORDER BY o.created_at DESC LIMIT 10;",
        
        // Complex query with CTE
        "WITH recent_orders AS (SELECT * FROM orders WHERE created_at > '2023-01-01') 
         SELECT u.name, COUNT(o.id) as order_count 
         FROM users u 
         LEFT JOIN recent_orders o ON u.id = o.user_id 
         GROUP BY u.id 
         HAVING order_count > 0 
         ORDER BY order_count DESC;",
        
        // Query with multiple JOINs
        "SELECT u.name, p.title, c.text 
         FROM users u 
         JOIN posts p ON u.id = p.user_id 
         JOIN comments c ON p.id = c.post_id 
         WHERE u.active = 1 
         AND p.published_at > '2023-01-01' 
         ORDER BY p.published_at DESC;",
        
        // Query with subquery in WHERE clause
        "SELECT id, name 
         FROM products 
         WHERE category_id IN (SELECT id FROM categories WHERE active = 1) 
         AND price > 100.0 
         ORDER BY name;",
        
        // Complex query with multiple subqueries, JOINs and aggregations
        "WITH top_selling AS (
           SELECT product_id, SUM(quantity) as total_sold 
           FROM order_items 
           GROUP BY product_id 
           ORDER BY total_sold DESC 
           LIMIT 100
         )
         SELECT 
           c.name as category_name, 
           p.name as product_name, 
           ts.total_sold,
           (SELECT AVG(rating) FROM reviews r WHERE r.product_id = p.id) as avg_rating
         FROM top_selling ts
         JOIN products p ON ts.product_id = p.id  
         JOIN categories c ON p.category_id = c.id
         WHERE p.active = 1
         AND c.active = 1
         ORDER BY ts.total_sold DESC, avg_rating DESC;"
    ];
    
    // Create SQLite dialect for sqlparser-rs
    let dialect = SQLiteDialect {};
    
    // Benchmark each query with both parsers
    for (i, query) in test_queries.iter().enumerate() {
        let query_name = format!("query_{}", i + 1);
        
        // Set throughput based on query length (characters)
        group.throughput(Throughput::Bytes(query.len() as u64));
        
        // Benchmark our parser
        group.bench_with_input(
            BenchmarkId::new("logos_parser", &query_name), 
            query, 
            |b, q| {
                b.iter(|| {
                    let _ = black_box(parse_statement(black_box(*q)));
                })
            }
        );
        
        // Benchmark sqlparser-rs
        group.bench_with_input(
            BenchmarkId::new("sqlparser_rs", &query_name), 
            query, 
            |b, q| {
                b.iter(|| {
                    let _ = black_box(SqlparserParser::parse_sql(&dialect, black_box(*q)));
                })
            }
        );
    }
    
    // Also add a batch processing benchmark to measure throughput on multiple queries
    let all_queries = test_queries.join("\n");
    group.throughput(Throughput::Bytes(all_queries.len() as u64));
    
    // Benchmark our parser on batch processing
    group.bench_function("logos_parser/batch_all_queries", |b| {
        b.iter(|| {
            for query in test_queries.iter() {
                let _ = black_box(parse_statement(black_box(*query)));
            }
        })
    });
    
    // Benchmark sqlparser-rs on batch processing
    group.bench_function("sqlparser_rs/batch_all_queries", |b| {
        b.iter(|| {
            for query in test_queries.iter() {
                let _ = black_box(SqlparserParser::parse_sql(&dialect, black_box(*query)));
            }
        })
    });
    
    group.finish();
}

// Add a separate benchmark for stress testing with repeated parsing of the same query
fn bench_repeated_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("Repeated Parsing");
    
    // A typical query that might be used repeatedly with different parameters
    let query = "SELECT id, name, email, created_at FROM users WHERE status = 1 AND age > ? AND region_id = ? ORDER BY created_at DESC LIMIT 20;";
    
    // Set throughput measurement
    group.throughput(Throughput::Elements(1000));
    
    // Create SQLite dialect for sqlparser-rs
    let dialect = SQLiteDialect {};
    
    // Benchmark our parser with 1000 iterations
    group.bench_function("logos_parser/1000_iterations", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let _ = black_box(parse_statement(black_box(query)));
            }
        })
    });
    
    // Benchmark sqlparser-rs with 1000 iterations
    group.bench_function("sqlparser_rs/1000_iterations", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let _ = black_box(SqlparserParser::parse_sql(&dialect, black_box(query)));
            }
        })
    });
    
    group.finish();
}

criterion_group!(benches, bench_parsers, bench_repeated_parsing);
criterion_main!(benches); 