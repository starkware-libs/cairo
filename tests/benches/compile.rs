use std::path::PathBuf;

use cairo_lang_compiler::{CompilerConfig, compile_cairo_project_at_path};
use cairo_lang_lowering::utils::InliningStrategy;
use criterion::{Criterion, criterion_group, criterion_main};

fn bench_compile(c: &mut Criterion) {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).parent().unwrap().to_owned();

    let mut group = c.benchmark_group("compile");
    group.sample_size(10);

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

    group.finish();
}

criterion_group!(benches, bench_compile);
criterion_main!(benches);
