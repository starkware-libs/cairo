use std::path::PathBuf;

use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_compiler::{CompilerConfig, compile_cairo_project_at_path, ensure_diagnostics};
use cairo_lang_lowering::optimizations::config::Optimizations;
use cairo_lang_lowering::utils::InliningStrategy;
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_compile(c: &mut Criterion) {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();

    let mut group = c.benchmark_group("compile");
    group.sample_size(10);

    // Phase: source → Sierra (full IR generation).
    group.bench_function("fib", |b| {
        let path = workspace_root.join("examples").join("fib.cairo");
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
    group.bench_function("fib_diagnostics", |b| {
        let path = workspace_root.join("examples").join("fib.cairo");
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

    group.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
