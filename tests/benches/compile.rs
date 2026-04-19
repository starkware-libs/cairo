use std::path::PathBuf;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::{CompilerConfig, compile_cairo_project_at_path, ensure_diagnostics};
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

    group.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
