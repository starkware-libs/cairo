use criterion::{Criterion, criterion_group, criterion_main};

mod common;

use common::{
    bench_configs, generate_cache, prepare_big_array, run_cache_to_sierra, run_cache_to_testing,
    run_cairo_to_cache, run_cairo_to_diagnostics, run_cairo_to_sierra, run_cairo_to_testing,
    target_dir,
};

/// Phase: source → Sierra (full IR generation).
fn bench_cairo_to_sierra(c: &mut Criterion) {
    let mut group = c.benchmark_group("cairo-to-sierra");
    group.sample_size(10);
    for config in bench_configs().into_iter().filter(|c| c.sierra_phases) {
        group.bench_function(config.name, |b| b.iter(|| run_cairo_to_sierra(&config)));
    }
    // Compile-time canary for large array literals (generated as an un-measured setup step):
    // catches compilation costs that scale super-linearly with the number of elements, e.g.
    // per-append state in lowering analyses.
    let big_array = prepare_big_array(&target_dir().join("bench-inputs"));
    group.bench_function(big_array.name, |b| b.iter(|| run_cairo_to_sierra(&big_array)));
    group.finish();
}

/// Phase: source → diagnostics (no Sierra generation).
fn bench_cairo_to_diagnostics(c: &mut Criterion) {
    let mut group = c.benchmark_group("cairo-to-diagnostics");
    group.sample_size(10);
    for config in bench_configs() {
        group.bench_function(config.name, |b| b.iter(|| run_cairo_to_diagnostics(&config)));
    }
    group.finish();
}

/// Phase: source → test results (compile + execute all #[test] functions).
fn bench_cairo_to_testing(c: &mut Criterion) {
    let mut group = c.benchmark_group("cairo-to-testing");
    group.sample_size(10);
    for config in bench_configs() {
        group.bench_function(config.name, |b| b.iter(|| run_cairo_to_testing(&config)));
    }
    group.finish();
}

/// Phase: source → cache (compile + serialize lowering to bytes, no Sierra generation).
fn bench_cairo_to_cache(c: &mut Criterion) {
    let mut group = c.benchmark_group("cairo-to-cache");
    group.sample_size(10);
    for config in bench_configs() {
        group.bench_function(config.name, |b| b.iter(|| run_cairo_to_cache(&config)));
    }
    group.finish();
}

/// Phase: cached lowering → Sierra (Sierra generation with pre-compiled lowering cache).
fn bench_cache_to_sierra(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache-to-sierra");
    group.sample_size(10);
    for config in bench_configs().into_iter().filter(|c| c.sierra_phases) {
        let cache_bytes = generate_cache(&config, false);
        group
            .bench_function(config.name, |b| b.iter(|| run_cache_to_sierra(&config, &cache_bytes)));
    }
    group.finish();
}

/// Phase: cached lowering → test results (compile tests + execute with pre-compiled lowering).
fn bench_cache_to_testing(c: &mut Criterion) {
    let mut group = c.benchmark_group("cache-to-testing");
    group.sample_size(10);
    for config in bench_configs() {
        // Cache must be generated with the same DB configuration as the test compiler uses
        // (test plugin + cfg(test)), so that test functions are included in the cache.
        let cache_bytes = generate_cache(&config, true);
        group.bench_function(config.name, |b| {
            b.iter(|| run_cache_to_testing(&config, &cache_bytes))
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_cairo_to_sierra,
    bench_cairo_to_diagnostics,
    bench_cairo_to_testing,
    bench_cairo_to_cache,
    bench_cache_to_sierra,
    bench_cache_to_testing,
);
criterion_main!(benches);
