use std::path::{Path, PathBuf};

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::{CompilerConfig, compile_prepared_db_program, ensure_diagnostics};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::db::{files_group_input, set_crate_configs_input};
use cairo_lang_filesystem::ids::{BlobLongId, CrateInput};
use cairo_lang_lowering::cache::generate_crate_cache;
use cairo_lang_lowering::optimizations::config::Optimizations;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_plugin::{TestsCompilationConfig, compile_test_prepared_db, test_plugin_suite};
use cairo_lang_test_runner::{TestRunConfig, TestRunner, run_tests};
use criterion::{Criterion, criterion_group, criterion_main};

/// Builds a database for benchmarking with optimizations and corelib detection.
/// `testing` enables the test plugin and `cfg(test)` for phases that compile test code.
fn build_db(config: &BenchConfig<'_>, testing: bool) -> RootDatabase {
    let mut builder = RootDatabase::builder();
    builder
        .with_optimizations(Optimizations::enabled_with_default_movable_functions(
            InliningStrategy::Default,
        ))
        .detect_corelib();
    if testing {
        builder
            .with_cfg(CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")]))
            .with_default_plugin_suite(test_plugin_suite());
    }
    if config.starknet {
        builder.with_default_plugin_suite(starknet_plugin_suite());
    }
    builder.build().unwrap()
}

/// Configuration for a benchmark group.
struct BenchConfig<'a> {
    path: &'a Path,
    group_name: &'a str,
    /// Whether to include the Starknet plugin (required for contract code).
    starknet: bool,
}

/// Benchmarks all compilation phases for a given project.
fn bench_all_phases(c: &mut Criterion, config: BenchConfig<'_>) {
    let mut group = c.benchmark_group(config.group_name);
    group.sample_size(10);

    // Phase: source → Sierra (full IR generation).
    group.bench_function("cairo-to-sierra", |b| {
        b.iter(|| {
            let mut db = build_db(&config, false);
            let inputs = setup_project(&mut db, config.path).unwrap();
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            compile_prepared_db_program(&db, crate_ids, CompilerConfig::default()).unwrap()
        });
    });

    // Phase: source → diagnostics (no Sierra generation).
    group.bench_function("cairo-to-diagnostics", |b| {
        b.iter(|| {
            let mut db = build_db(&config, true);
            setup_project(&mut db, config.path).unwrap();
            ensure_diagnostics(&db, &mut DiagnosticsReporter::ignoring()).unwrap()
        })
    });

    // Phase: source → test results (compile + execute all #[test] functions).
    group.bench_function("cairo-to-testing", |b| {
        b.iter(|| {
            TestRunner::new(
                config.path,
                config.starknet,
                false,
                TestRunConfig {
                    filter: String::new(),
                    include_ignored: false,
                    ignored: false,
                    profiler_config: None,
                    gas_enabled: true,
                    print_resource_usage: false,
                },
            )
            .unwrap()
            .run()
            .unwrap()
        })
    });

    // Phase: source → cache (compile + serialize lowering to bytes, no Sierra generation).
    group.bench_function("cairo-to-cache", |b| {
        b.iter(|| {
            let mut db = build_db(&config, true);
            let inputs = setup_project(&mut db, config.path).unwrap();
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            generate_crate_cache(&db, crate_ids[0]).unwrap()
        })
    });

    // Phase: cached lowering → Sierra (Sierra generation with pre-compiled lowering cache).
    group.bench_function("cache-to-sierra", |b| {
        let cache_bytes = {
            let mut db = build_db(&config, false);
            let inputs = setup_project(&mut db, config.path).unwrap();
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            generate_crate_cache(&db, crate_ids[0]).unwrap()
        };
        b.iter(|| {
            let mut db = build_db(&config, false);
            let inputs = setup_project(&mut db, config.path).unwrap();
            let mut crate_configs = files_group_input(&db).crate_configs(&db).clone().unwrap();
            crate_configs.get_mut(&inputs[0]).unwrap().cache_file =
                Some(BlobLongId::Virtual(cache_bytes.clone()));
            set_crate_configs_input(&mut db, Some(crate_configs));
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            compile_prepared_db_program(&db, crate_ids, CompilerConfig::default()).unwrap()
        })
    });

    // Phase: cached lowering → test results (compile tests + execute with pre-compiled lowering).
    group.bench_function("cache-to-testing", |b| {
        // Cache must be generated with the same DB configuration as the test compiler uses
        // (test plugin + cfg(test)), so that test functions are included in the cache.
        let cache_bytes = {
            let mut db = build_db(&config, true);
            let inputs = setup_project(&mut db, config.path).unwrap();
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            generate_crate_cache(&db, crate_ids[0]).unwrap()
        };
        let run_config = TestRunConfig {
            filter: String::new(),
            include_ignored: false,
            ignored: false,
            profiler_config: None,
            gas_enabled: true,
            print_resource_usage: false,
        };
        b.iter(|| {
            let mut db = build_db(&config, true);
            let main_crate_inputs = setup_project(&mut db, config.path).unwrap();
            let mut crate_configs = files_group_input(&db).crate_configs(&db).clone().unwrap();
            crate_configs.get_mut(&main_crate_inputs[0]).unwrap().cache_file =
                Some(BlobLongId::Virtual(cache_bytes.clone()));
            set_crate_configs_input(&mut db, Some(crate_configs));
            let compiled = compile_test_prepared_db(
                &db,
                TestsCompilationConfig {
                    starknet: config.starknet,
                    contract_declarations: None,
                    contract_crate_ids: None,
                    executable_crate_ids: None,
                    add_statements_functions: false,
                    add_statements_code_locations: false,
                    add_functions_debug_info: false,
                    replace_ids: false,
                },
                main_crate_inputs.clone(),
                DiagnosticsReporter::stderr().with_crates(&main_crate_inputs),
            )
            .unwrap();
            run_tests(None, compiled, &run_config, None).unwrap()
        })
    });

    group.finish();
}

/// Actually run all the benchmarks.
fn bench_compile(c: &mut Criterion) {
    let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = tests.parent().unwrap().to_path_buf();
    let examples = tests.join("benches").join("examples");
    bench_all_phases(
        c,
        BenchConfig { group_name: "fib", path: &examples.join("fib.cairo"), starknet: false },
    );
    bench_all_phases(
        c,
        BenchConfig { group_name: "corelib", path: &root.join("corelib"), starknet: false },
    );
    bench_all_phases(
        c,
        BenchConfig { group_name: "bug_samples", path: &tests.join("bug_samples"), starknet: true },
    );
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
