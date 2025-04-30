use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::Path;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("SQLite Parser Benchmark Results Analysis");
    println!("========================================\n");
    let target_dir = Path::new("target/criterion");
    if !target_dir.exists() {
        return Err("Benchmark results not found. Please run 'cargo bench' first.".into());
    }
    let mut results: BTreeMap<String, BTreeMap<String, f64>> = BTreeMap::new();
    for group_dir in fs::read_dir(target_dir)? {
        let group_dir = group_dir?;
        let group_path = group_dir.path();
        if group_path.is_dir()
            && group_path
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_lowercase()
                .contains("sqlite parsers")
        {
            println!(
                "Found benchmark group: {}",
                group_path.file_name().unwrap().to_string_lossy()
            );
            for bench_dir in fs::read_dir(group_path)? {
                let bench_dir = bench_dir?;
                let bench_path = bench_dir.path();
                if bench_path.is_dir() {
                    let name = bench_path
                        .file_name()
                        .unwrap()
                        .to_string_lossy()
                        .to_string();
                    println!("Found benchmark: {}", name);
                    let mut parts = name.split('/');
                    let parser = parts.next().unwrap_or("unknown").to_string();
                    let query = parts.next().unwrap_or("unknown").to_string();
                    let estimates_path = bench_path.join("estimates.json");
                    if estimates_path.exists() {
                        println!("Found estimates.json at: {}", estimates_path.display());
                        let file = File::open(estimates_path)?;
                        let reader = BufReader::new(file);
                        let mut mean_time = 0.0;
                        for line in reader.lines() {
                            let line = line?;
                            if line.contains("\"mean\":") {
                                let start = line.find(":").unwrap() + 1;
                                let end = line.find(",").unwrap_or(line.len());
                                let value = line[start..end].trim();
                                mean_time = value.parse::<f64>().unwrap_or(0.0);
                                println!("Extracted mean time: {}", mean_time);
                                break;
                            }
                        }
                        results
                            .entry(query)
                            .or_insert_with(BTreeMap::new)
                            .insert(parser, mean_time);
                    } else {
                        println!("No estimates.json found at: {}", estimates_path.display());
                    }
                }
            }
            break;
        }
    }
    println!("\nBenchmark Results Table:");
    println!("| Query | Logos Parser (ns) | sqlparser-rs (ns) | Difference (%) |");
    println!("|-------|-------------------|-------------------|----------------|");
    for (query, parsers) in &results {
        let logos_time = parsers.get("logos_parser").copied().unwrap_or(0.0);
        let sqlparser_time = parsers.get("sqlparser_rs").copied().unwrap_or(0.0);
        let diff_pct = if sqlparser_time > 0.0 {
            ((logos_time / sqlparser_time) - 1.0) * 100.0
        } else {
            0.0
        };
        println!(
            "| {} | {:.2} | {:.2} | {:.2}% |",
            query, logos_time, sqlparser_time, diff_pct
        );
    }
    if results.is_empty() {
        println!("\nNo results found. Make sure your benchmarks have run successfully.");
    } else {
        println!("\nNote: Negative difference (%) means Logos parser is faster");
    }
    Ok(())
}
