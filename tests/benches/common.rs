//! Shared scenarios for compile benchmarks.
//!
//! Each phase is exposed as a one-shot `run_*` function so it can be driven by either the criterion
//! timing harness (in `compile.rs`) or the dhat heap profiler (in `dhat_compile.rs`). Because each
//! bench includes this module independently, items used by only one bench look "dead" to the
//! other — silence those false positives module-wide.
#![allow(dead_code)]

use std::path::PathBuf;

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
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_to_casm::compiler::SierraToCasmConfig;
use cairo_lang_sierra_to_casm::metadata::calc_metadata;
use cairo_lang_sierra_type_size::ProgramRegistryInfo;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_plugin::{TestsCompilationConfig, compile_test_prepared_db, test_plugin_suite};
use cairo_lang_test_runner::{TestRunConfig, TestRunner, run_tests};

/// Configuration for a benchmarked project.
pub struct BenchConfig {
    pub name: &'static str,
    pub path: PathBuf,
    /// Whether to include the Starknet plugin (required for contract code).
    pub starknet: bool,
    /// Whether to run cairo-to-sierra and cache-to-sierra phases.
    /// Set to false when source files use bare `#[test]` attributes (not guarded by
    /// `#[cfg(test)]`), which are unrecognized without the test plugin, or when the project
    /// contains only contract code with no program entry point.
    pub sierra_phases: bool,
}

/// Returns the list of projects to benchmark across all phases.
pub fn bench_configs() -> Vec<BenchConfig> {
    let tests = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = tests.parent().unwrap().to_path_buf();
    let examples = tests.join("benches").join("examples");
    vec![
        BenchConfig {
            name: "fib",
            path: examples.join("fib.cairo"),
            starknet: false,
            sierra_phases: true,
        },
        BenchConfig {
            name: "corelib",
            path: root.join("corelib"),
            starknet: false,
            sierra_phases: true,
        },
        BenchConfig {
            name: "bug_samples",
            path: tests.join("bug_samples"),
            starknet: true,
            sierra_phases: false,
        },
        BenchConfig {
            name: "cairo_level_tests",
            path: root.join("crates").join("cairo-lang-starknet").join("cairo_level_tests"),
            starknet: true,
            sierra_phases: true,
        },
    ]
}

/// Builds a database for benchmarking with optimizations and corelib detection.
/// `testing` enables the test plugin and `cfg(test)` for phases that compile test code.
pub fn build_db(config: &BenchConfig, testing: bool) -> RootDatabase {
    let mut builder = RootDatabase::builder();
    builder
        .with_optimizations(Optimizations::enabled_with_default_movable_functions(
            InliningStrategy::Default,
        ))
        .detect_corelib();
    if config.starknet {
        builder.with_default_plugin_suite(starknet_plugin_suite());
    }
    if testing {
        builder
            .with_cfg(CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")]))
            .with_default_plugin_suite(test_plugin_suite());
    }
    builder.build().unwrap()
}

fn test_run_config() -> TestRunConfig {
    TestRunConfig {
        filter: String::new(),
        include_ignored: false,
        ignored: false,
        profiler_config: None,
        gas_enabled: true,
        print_resource_usage: false,
    }
}

/// Phase: source → Sierra (full IR generation). Returns the program so downstream phases (e.g.
/// sierra-to-casm) can be set up outside a profiled section.
pub fn run_cairo_to_sierra(config: &BenchConfig) -> Program {
    let mut db = build_db(config, false);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    let diagnostics_reporter = DiagnosticsReporter::ignoring().with_crates(&inputs);
    let crate_ids = CrateInput::into_crate_ids(&db, inputs);
    compile_prepared_db_program(
        &db,
        crate_ids,
        CompilerConfig { diagnostics_reporter, ..Default::default() },
    )
    .unwrap()
}

/// Phase: Sierra → CASM. Only valid for non-Starknet programs (Starknet contracts go through
/// `cairo_lang_starknet_classes::casm_contract_class` instead).
pub fn run_sierra_to_casm(program: &Program) {
    let program_info = ProgramRegistryInfo::new(program).unwrap();
    let metadata = calc_metadata(program, &program_info, Default::default()).unwrap();
    cairo_lang_sierra_to_casm::compiler::compile(
        program,
        &program_info,
        &metadata,
        SierraToCasmConfig { gas_usage_check: true, max_bytecode_size: usize::MAX },
    )
    .unwrap();
}

/// Phase: source → diagnostics (no Sierra generation).
pub fn run_cairo_to_diagnostics(config: &BenchConfig) {
    let mut db = build_db(config, true);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    ensure_diagnostics(&db, &mut DiagnosticsReporter::ignoring().with_crates(&inputs)).unwrap();
}

/// Phase: source → test results (compile + execute all #[test] functions).
pub fn run_cairo_to_testing(config: &BenchConfig) {
    TestRunner::new(&config.path, config.starknet, false, test_run_config())
        .unwrap()
        .run()
        .unwrap();
}

/// Phase: source → cache (compile + serialize lowering to bytes, no Sierra generation).
pub fn run_cairo_to_cache(config: &BenchConfig) {
    generate_cache(config, true);
}

/// Generates a lowering cache for use as input to cache-* phases.
/// `testing` must match the database configuration of the consumer phase.
pub fn generate_cache(config: &BenchConfig, testing: bool) -> Vec<u8> {
    let mut db = build_db(config, testing);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    let crate_ids = CrateInput::into_crate_ids(&db, inputs);
    generate_crate_cache(&db, crate_ids[0]).unwrap()
}

/// Phase: cached lowering → Sierra (Sierra generation with pre-compiled lowering cache).
pub fn run_cache_to_sierra(config: &BenchConfig, cache_bytes: &[u8]) {
    let mut db = build_db(config, false);
    let inputs = setup_project(&mut db, &config.path).unwrap();
    let diagnostics_reporter = DiagnosticsReporter::ignoring().with_crates(&inputs);
    let mut crate_configs = files_group_input(&db).crate_configs(&db).clone().unwrap();
    crate_configs.get_mut(&inputs[0]).unwrap().cache_file =
        Some(BlobLongId::Virtual(cache_bytes.to_vec()));
    set_crate_configs_input(&mut db, Some(crate_configs));
    let crate_ids = CrateInput::into_crate_ids(&db, inputs);
    compile_prepared_db_program(
        &db,
        crate_ids,
        CompilerConfig { diagnostics_reporter, ..Default::default() },
    )
    .unwrap();
}

/// Phase: cached lowering → test results (compile tests + execute with pre-compiled lowering).
pub fn run_cache_to_testing(config: &BenchConfig, cache_bytes: &[u8]) {
    let mut db = build_db(config, true);
    let main_crate_inputs = setup_project(&mut db, &config.path).unwrap();
    let mut crate_configs = files_group_input(&db).crate_configs(&db).clone().unwrap();
    crate_configs.get_mut(&main_crate_inputs[0]).unwrap().cache_file =
        Some(BlobLongId::Virtual(cache_bytes.to_vec()));
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
            add_type_names: false,
            replace_ids: false,
        },
        main_crate_inputs.clone(),
        DiagnosticsReporter::stderr().with_crates(&main_crate_inputs),
    )
    .unwrap();
    run_tests(None, compiled, &test_run_config(), None).unwrap();
}
