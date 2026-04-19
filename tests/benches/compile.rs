use std::path::PathBuf;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::{
    CompilerConfig, compile_cairo_project_at_path, compile_prepared_db_program, ensure_diagnostics,
};
use cairo_lang_filesystem::db::{files_group_input, set_crate_configs_input};
use cairo_lang_filesystem::ids::{BlobLongId, CrateInput};
use cairo_lang_lowering::cache::generate_crate_cache;
use cairo_lang_lowering::optimizations::config::Optimizations;
use cairo_lang_lowering::utils::InliningStrategy;
use cairo_lang_test_runner::{TestRunConfig, TestRunner};
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_compile(c: &mut Criterion) {
    let examples = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("benches").join("examples");

    let mut group = c.benchmark_group("compile");
    group.sample_size(10);

    // Phase: source → Sierra (full IR generation).
    group.bench_function("fib: cairo-to-sierra", |b| {
        let path = examples.join("fib.cairo");
        b.iter(|| {
            compile_cairo_project_at_path(
                &path,
                CompilerConfig::default(),
                InliningStrategy::Default,
            )
            .unwrap()
        });
    });

    // Phase: source → diagnostics (no Sierra generation).
    group.bench_function("fib: cairo-to-diagnostics", |b| {
        let path = examples.join("fib.cairo");
        b.iter(|| {
            let mut db = RootDatabase::builder()
                .with_optimizations(Optimizations::enabled_with_default_movable_functions(
                    InliningStrategy::Default,
                ))
                .detect_corelib()
                .build()
                .unwrap();
            setup_project(&mut db, &path).unwrap();
            ensure_diagnostics(&db, &mut DiagnosticsReporter::ignoring()).unwrap()
        })
    });

    // Phase: source → test results (compile + execute all #[test] functions).
    group.bench_function("fib: cairo-to-testing", |b| {
        let path = examples.join("fib.cairo");
        b.iter(|| {
            TestRunner::new(
                &path,
                false,
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
    group.bench_function("fib: cairo-to-cache", |b| {
        let path = examples.join("fib.cairo");
        b.iter(|| {
            let mut db = RootDatabase::builder()
                .with_optimizations(Optimizations::enabled_with_default_movable_functions(
                    InliningStrategy::Default,
                ))
                .detect_corelib()
                .build()
                .unwrap();
            let inputs = setup_project(&mut db, &path).unwrap();
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            generate_crate_cache(&db, crate_ids[0]).unwrap()
        })
    });

    // Phase: cached lowering → Sierra (Sierra generation with pre-compiled lowering cache).
    group.bench_function("fib: cache-to-sierra", |b| {
        let path = examples.join("fib.cairo");
        let mut db = RootDatabase::builder()
            .with_optimizations(Optimizations::enabled_with_default_movable_functions(
                InliningStrategy::Default,
            ))
            .detect_corelib()
            .build()
            .unwrap();
        let inputs = setup_project(&mut db, &path).unwrap();
        let crate_ids = CrateInput::into_crate_ids(&db, inputs);
        let cache_bytes = generate_crate_cache(&db, crate_ids[0]).unwrap();
        b.iter(|| {
            let mut db = RootDatabase::builder()
                .with_optimizations(Optimizations::enabled_with_default_movable_functions(
                    InliningStrategy::Default,
                ))
                .detect_corelib()
                .build()
                .unwrap();
            let inputs = setup_project(&mut db, &path).unwrap();
            let mut crate_configs = files_group_input(&db).crate_configs(&db).clone().unwrap();
            crate_configs.get_mut(&inputs[0]).unwrap().cache_file =
                Some(BlobLongId::Virtual(cache_bytes.clone()));
            set_crate_configs_input(&mut db, Some(crate_configs));
            let crate_ids = CrateInput::into_crate_ids(&db, inputs);
            compile_prepared_db_program(&db, crate_ids, CompilerConfig::default()).unwrap()
        })
    });

    group.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
