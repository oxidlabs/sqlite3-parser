use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, Throughput};
use sqlite3_parser_logos::parser::parse_statement;
use sqlparser::dialect::SQLiteDialect;
use sqlparser::parser::Parser as SqlparserParser;
fn bench_parsers(c: &mut Criterion) {
    let mut group = c.benchmark_group("SQLite Parsers");
    let test_queries = [
        "SELECT * FROM users;",
        "SELECT id, name, email FROM users WHERE age > 18;",
        "SELECT u.id, u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE o.amount > 100.0 ORDER BY o.created_at DESC LIMIT 10;",
        "WITH recent_orders AS (SELECT * FROM orders WHERE created_at > '2023-01-01') 
         SELECT u.name, COUNT(o.id) as order_count 
         FROM users u 
         LEFT JOIN recent_orders o ON u.id = o.user_id 
         GROUP BY u.id 
         HAVING order_count > 0 
         ORDER BY order_count DESC;",
        "SELECT u.name, p.title, c.text 
         FROM users u 
         JOIN posts p ON u.id = p.user_id 
         JOIN comments c ON p.id = c.post_id 
         WHERE u.active = 1 
         AND p.published_at > '2023-01-01' 
         ORDER BY p.published_at DESC;",
        "SELECT id, name 
         FROM products 
         WHERE category_id IN (SELECT id FROM categories WHERE active = 1) 
         AND price > 100.0 
         ORDER BY name;",
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
    let dialect = SQLiteDialect {};
    for (i, query) in test_queries.iter().enumerate() {
        let query_name = format!("query_{}", i + 1);
        group.throughput(Throughput::Bytes(query.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("logos_parser", &query_name), 
            query, 
            |b, q| {
                b.iter(|| {
                    let _ = black_box(parse_statement(black_box(*q)));
                })
            }
        );
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
    let all_queries = test_queries.join("\n");
    group.throughput(Throughput::Bytes(all_queries.len() as u64));
    group.bench_function("logos_parser/batch_all_queries", |b| {
        b.iter(|| {
            for query in test_queries.iter() {
                let _ = black_box(parse_statement(black_box(*query)));
            }
        })
    });
    group.bench_function("sqlparser_rs/batch_all_queries", |b| {
        b.iter(|| {
            for query in test_queries.iter() {
                let _ = black_box(SqlparserParser::parse_sql(&dialect, black_box(*query)));
            }
        })
    });
    group.finish();
}
fn bench_window_functions(c: &mut Criterion) {
    let mut group = c.benchmark_group("Window Functions and Closures");
    let window_queries = [
        "SELECT name, rank() OVER (PARTITION BY department) FROM employees;",
        "SELECT name, AVG(salary) OVER (PARTITION BY department ORDER BY hire_date) FROM employees;",
        "SELECT name, SUM(sales) OVER (PARTITION BY department ORDER BY hire_date ROWS UNBOUNDED PRECEDING) FROM employees;",
        "SELECT name, SUM(sales) OVER (PARTITION BY department ORDER BY hire_date ROWS BETWEEN 3 PRECEDING AND CURRENT ROW) FROM employees;",
        "SELECT 
           name, 
           AVG(salary) OVER (PARTITION BY department) as dept_avg,
           MAX(salary) OVER (PARTITION BY department) as dept_max,
           MIN(salary) OVER (PARTITION BY department) as dept_min,
           RANK() OVER (PARTITION BY department ORDER BY salary DESC) as salary_rank
         FROM employees;",
        "SELECT name, SUM(sales) FILTER (WHERE region = 'North') OVER (PARTITION BY department) FROM employees;",
        "SELECT 
           e.name, 
           d.name as department_name,
           e.salary,
           AVG(e.salary) OVER (PARTITION BY e.department_id) as dept_avg_salary,
           e.salary - AVG(e.salary) OVER (PARTITION BY e.department_id) as salary_diff,
           RANK() OVER (PARTITION BY e.department_id ORDER BY e.salary DESC) as dept_salary_rank
         FROM employees e
         JOIN departments d ON e.department_id = d.id
         WHERE e.active = 1
         ORDER BY d.name, dept_salary_rank;"
    ];
    let dialect = SQLiteDialect {};
    for (i, query) in window_queries.iter().enumerate() {
        let query_name = format!("window_query_{}", i + 1);
        group.throughput(Throughput::Bytes(query.len() as u64));
        group.bench_with_input(
            BenchmarkId::new("logos_parser", &query_name), 
            query, 
            |b, q| {
                b.iter(|| {
                    let _ = black_box(parse_statement(black_box(*q)));
                })
            }
        );
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
    group.finish();
}
fn bench_repeated_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("Repeated Parsing");
    let query = "SELECT id, name, email, created_at FROM users WHERE status = 1 AND age > ? AND region_id = ? ORDER BY created_at DESC LIMIT 20;";
    group.throughput(Throughput::Elements(1000));
    let dialect = SQLiteDialect {};
    group.bench_function("logos_parser/1000_iterations", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let _ = black_box(parse_statement(black_box(query)));
            }
        })
    });
    group.bench_function("sqlparser_rs/1000_iterations", |b| {
        b.iter(|| {
            for _ in 0..1000 {
                let _ = black_box(SqlparserParser::parse_sql(&dialect, black_box(query)));
            }
        })
    });
    group.finish();
}
criterion_group!(benches, bench_parsers, bench_window_functions, bench_repeated_parsing);
criterion_main!(benches); 