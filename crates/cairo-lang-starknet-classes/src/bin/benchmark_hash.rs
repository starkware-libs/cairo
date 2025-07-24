use std::time::{Duration, Instant};
use std::{env, fs};

use cairo_lang_starknet_classes::casm_contract_class::CasmContractClass;

fn main() {
    let args: Vec<String> = env::args().collect();

    // Parse command line arguments
    let iterations = if args.len() > 1 { args[1].parse().unwrap_or(1000) } else { 1000 };

    let contract_path = if args.len() > 2 {
        args[2].clone()
    } else {
        "/home/aviv/workspace/sequencer/crates/blockifier_test_utils/resources/feature_contracts/\
         cairo1/compiled/test_contract.casm.json"
            .to_string()
    };

    println!("=== Compiled Class Hash Benchmark ===\n");
    println!("Parameters:");
    println!("  Iterations: {}", iterations);
    println!("  Contract: {}\n", contract_path);

    // Load the test contract from the JSON file
    println!("Loading contract from: {}", contract_path);
    let contract_json = fs::read_to_string(&contract_path).expect("Failed to read contract file");

    println!("Deserializing contract...");
    let start_deserialize = Instant::now();
    let casm_contract: CasmContractClass =
        serde_json::from_str(&contract_json).expect("Failed to deserialize contract");
    let deserialize_time = start_deserialize.elapsed();
    println!("Deserialization took: {:?}", deserialize_time);

    // Print some basic info about the contract
    println!("\nContract info:");
    println!("  Compiler version: {}", casm_contract.compiler_version);
    println!("  Bytecode length: {}", casm_contract.bytecode.len());
    println!("  External entry points: {}", casm_contract.entry_points_by_type.external.len());
    println!("  L1 handler entry points: {}", casm_contract.entry_points_by_type.l1_handler.len());
    println!(
        "  Constructor entry points: {}",
        casm_contract.entry_points_by_type.constructor.len()
    );

    if let Some(ref segment_lengths) = casm_contract.bytecode_segment_lengths {
        println!("  Bytecode segments: present");
    } else {
        println!("  Bytecode segments: not present");
    }
    println!();

    // Warm up run
    println!("Performing warm-up run...");
    let _ = casm_contract.compiled_class_hash();

    // Single run benchmark
    println!("Measuring single run...");
    let start_single = Instant::now();
    let hash = casm_contract.compiled_class_hash();
    let single_time = start_single.elapsed();
    println!("Single run took: {:?}", single_time);
    println!("Computed hash: {:x}", hash.to_biguint());
    println!();

    if iterations == 1 {
        println!("Single run benchmark completed.");
        return;
    }

    // Multiple runs benchmark
    println!("Measuring {} iterations...", iterations);

    let mut times = Vec::with_capacity(iterations);
    let overall_start = Instant::now();

    let progress_interval = if iterations >= 100 { iterations / 10 } else { 1 };

    for i in 0..iterations {
        if i % progress_interval == 0 && i > 0 {
            print!(".");
            use std::io::{self, Write};
            io::stdout().flush().unwrap();
        }

        let start = Instant::now();
        let _ = casm_contract.compiled_class_hash();
        times.push(start.elapsed());
    }
    println!(); // New line after progress dots

    let overall_time = overall_start.elapsed();

    // Calculate statistics
    let total_nanos: u128 = times.iter().map(|d| d.as_nanos()).sum();
    let avg_time = Duration::from_nanos((total_nanos / iterations as u128) as u64);

    let mut sorted_times = times.clone();
    sorted_times.sort();
    let median_time = sorted_times[iterations / 2];
    let min_time = *sorted_times.first().unwrap();
    let max_time = *sorted_times.last().unwrap();

    println!("=== Benchmark Results ({} iterations) ===", iterations);
    println!("  Total time: {:?}", overall_time);
    println!("  Average time: {:?}", avg_time);
    println!("  Median time: {:?}", median_time);
    println!("  Min time: {:?}", min_time);
    println!("  Max time: {:?}", max_time);
    println!("  Throughput: {:.2} hashes/sec", iterations as f64 / overall_time.as_secs_f64());

    if iterations >= 10 {
        // Percentiles
        let p95_time = sorted_times[(iterations as f64 * 0.95) as usize];
        let p99_time = sorted_times[(iterations as f64 * 0.99) as usize];
        println!("  95th percentile: {:?}", p95_time);
        println!("  99th percentile: {:?}", p99_time);
    }

    // Performance analysis
    println!("\n=== Performance Analysis ===");
    if avg_time.as_micros() < 100 {
        println!("  Performance: EXCELLENT (< 100μs average)");
    } else if avg_time.as_micros() < 500 {
        println!("  Performance: GOOD (< 500μs average)");
    } else if avg_time.as_millis() < 1 {
        println!("  Performance: FAIR (< 1ms average)");
    } else {
        println!("  Performance: SLOW (> 1ms average)");
    }

    if iterations > 1 {
        let std_dev_nanos = {
            let avg_nanos = avg_time.as_nanos() as f64;
            let variance = times
                .iter()
                .map(|t| {
                    let diff = t.as_nanos() as f64 - avg_nanos;
                    diff * diff
                })
                .sum::<f64>()
                / iterations as f64;
            variance.sqrt()
        };
        let std_dev = Duration::from_nanos(std_dev_nanos as u64);
        println!("  Standard deviation: {:?}", std_dev);
        println!(
            "  Coefficient of variation: {:.2}%",
            (std_dev_nanos / avg_time.as_nanos() as f64) * 100.0
        );
    }

    println!("\n=== Usage Examples ===");
    println!("  Single run:     cargo run --bin benchmark_hash 1");
    println!("  Quick test:     cargo run --bin benchmark_hash 10");
    println!("  Default test:   cargo run --bin benchmark_hash 1000");
    println!("  Long test:      cargo run --bin benchmark_hash 10000");
    println!("  Custom file:    cargo run --bin benchmark_hash 100 /path/to/contract.json");
}
