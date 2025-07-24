use std::fs;
use std::time::{Duration, Instant};

use crate::casm_contract_class::CasmContractClass;

pub fn benchmark_compiled_class_hash() {
    // Load the test contract from the JSON file
    let contract_path = "/home/aviv/workspace/sequencer/crates/blockifier_test_utils/resources/feature_contracts/cairo1/compiled/test_contract.casm.json";

    println!("Loading contract from: {}", contract_path);
    let contract_json = fs::read_to_string(contract_path)
        .expect("Failed to read contract file");

    println!("Deserializing contract...");
    let start_deserialize = Instant::now();
    let casm_contract: CasmContractClass = serde_json::from_str(&contract_json)
        .expect("Failed to deserialize contract");

    // Warm up run
    println!("Performing warm-up run...");
    let _ = casm_contract.compiled_class_hash();

    // Single run benchmark
    println!("Measuring single run...");
    let start_single = Instant::now();
    let hash = casm_contract.compiled_class_hash();
    let single_time = start_single.elapsed();
    println!("Single run took: {:?}", single_time);
    println!();

    // Multiple runs benchmark
    const NUM_ITERATIONS: usize = 1000;
    println!("Measuring {} iterations...", NUM_ITERATIONS);

    let mut times = Vec::with_capacity(NUM_ITERATIONS);
    let overall_start = Instant::now();

    for _ in 0..NUM_ITERATIONS {
        let start = Instant::now();
        let _ = casm_contract.compiled_class_hash();
        times.push(start.elapsed());
    }

    let overall_time = overall_start.elapsed();

    // Calculate statistics
    let total_nanos: u128 = times.iter().map(|d| d.as_nanos()).sum();
    let avg_time = Duration::from_nanos((total_nanos / NUM_ITERATIONS as u128) as u64);

    let mut sorted_times = times.clone();
    sorted_times.sort();
    let median_time = sorted_times[NUM_ITERATIONS / 2];
    let min_time = *sorted_times.first().unwrap();
    let max_time = *sorted_times.last().unwrap();

    println!("Benchmark Results ({} iterations):", NUM_ITERATIONS);
    println!("  Total time: {:?}", overall_time);
    println!("  Average time: {:?}", avg_time);
    println!("  Median time: {:?}", median_time);
    println!("  Min time: {:?}", min_time);
    println!("  Max time: {:?}", max_time);
    println!("  Throughput: {:.2} hashes/sec", NUM_ITERATIONS as f64 / overall_time.as_secs_f64());

    // Percentiles
    let p95_time = sorted_times[(NUM_ITERATIONS as f64 * 0.95) as usize];
    let p99_time = sorted_times[(NUM_ITERATIONS as f64 * 0.99) as usize];
    println!("  95th percentile: {:?}", p95_time);
    println!("  99th percentile: {:?}", p99_time);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // Use `cargo test benchmark_test -- --ignored` to run
    fn benchmark_test() {
        benchmark_compiled_class_hash();
    }
}