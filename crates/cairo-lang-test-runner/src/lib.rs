use std::path::Path;
use std::sync::Mutex;

use anyhow::{Context, Result, bail};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_runner::casm_run::format_for_panic;
use cairo_lang_runner::profiling::{
    ProfilingInfo, ProfilingInfoProcessor, ProfilingInfoProcessorParams,
};
use cairo_lang_runner::{
    ProfilingInfoCollectionConfig, RunResultValue, SierraCasmRunner, StarknetExecutionResources,
};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_to_casm::metadata::MetadataComputationConfig;
use cairo_lang_starknet::contract::ContractInfo;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_plugin::test_config::{PanicExpectation, TestExpectation};
use cairo_lang_test_plugin::{
    TestCompilation, TestCompilationMetadata, TestConfig, TestsCompilationConfig,
    compile_test_prepared_db, test_plugin_suite,
};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use colored::Colorize;
use itertools::Itertools;
use num_traits::ToPrimitive;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use starknet_types_core::felt::Felt as Felt252;

#[cfg(test)]
mod test;

/// Compile and run tests.
pub struct TestRunner {
    compiler: TestCompiler,
    config: TestRunConfig,
}

impl TestRunner {
    /// Configure a new test runner
    ///
    /// # Arguments
    ///
    /// * `path` - The path to compile and run its tests
    /// * `filter` - Run only tests containing the filter string
    /// * `include_ignored` - Include ignored tests as well
    /// * `ignored` - Run ignored tests only
    /// * `starknet` - Add the starknet plugin to run the tests
    pub fn new(
        path: &Path,
        starknet: bool,
        allow_warnings: bool,
        config: TestRunConfig,
    ) -> Result<Self> {
        let compiler = TestCompiler::try_new(
            path,
            allow_warnings,
            config.gas_enabled,
            TestsCompilationConfig {
                starknet,
                add_statements_functions: config.run_profiler == RunProfilerConfig::Cairo,
                add_statements_code_locations: false,
                contract_declarations: None,
                contract_crate_ids: None,
                executable_crate_ids: None,
            },
        )?;
        Ok(Self { compiler, config })
    }

    /// Runs the tests and process the results for a summary.
    pub fn run(&self) -> Result<Option<TestsSummary>> {
        let runner = CompiledTestRunner::new(self.compiler.build()?, self.config.clone());
        runner.run(Some(&self.compiler.db))
    }
}

pub struct CompiledTestRunner {
    pub compiled: TestCompilation,
    pub config: TestRunConfig,
}

impl CompiledTestRunner {
    /// Configure a new compiled test runner
    ///
    /// # Arguments
    ///
    /// * `compiled` - The compiled tests to run
    /// * `config` - Test run configuration
    pub fn new(compiled: TestCompilation, config: TestRunConfig) -> Self {
        Self { compiled, config }
    }

    /// Execute preconfigured test execution.
    pub fn run(self, db: Option<&RootDatabase>) -> Result<Option<TestsSummary>> {
        let (compiled, filtered_out) = filter_test_cases(
            self.compiled,
            self.config.include_ignored,
            self.config.ignored,
            &self.config.filter,
        );

        let TestsSummary { passed, failed, ignored, failed_run_results } = run_tests(
            if self.config.run_profiler == RunProfilerConfig::Cairo {
                let db = db.expect("db must be passed when profiling.");
                let statements_locations = compiled
                    .metadata
                    .statements_locations
                    .expect("statements locations must be present when profiling.");
                Some(PorfilingAuxData {
                    db,
                    statements_functions: statements_locations
                        .get_statements_functions_map_for_tests(db),
                })
            } else {
                None
            },
            compiled.metadata.named_tests,
            compiled.sierra_program.program,
            compiled.metadata.function_set_costs,
            compiled.metadata.contracts_info,
            &self.config,
        )?;

        if failed.is_empty() {
            println!(
                "test result: {}. {} passed; {} failed; {} ignored; {filtered_out} filtered out;",
                "ok".bright_green(),
                passed.len(),
                failed.len(),
                ignored.len()
            );
            Ok(None)
        } else {
            println!("failures:");
            for (failure, run_result) in failed.iter().zip_eq(failed_run_results) {
                print!("   {failure} - ");
                match run_result {
                    RunResultValue::Success(_) => {
                        println!("expected panic but finished successfully.");
                    }
                    RunResultValue::Panic(values) => {
                        println!("{}", format_for_panic(values.into_iter()));
                    }
                }
            }
            println!();
            bail!(
                "test result: {}. {} passed; {} failed; {} ignored",
                "FAILED".bright_red(),
                passed.len(),
                failed.len(),
                ignored.len()
            );
        }
    }
}

/// Whether to run the profiler, and what results to produce.
///
/// With `None`, don't run the profiler.
/// With `Sierra`, run the profiler and produce sierra profiling information.
/// With `Cairo`, run the profiler and additionally produce cairo profiling information (e.g.
///     filtering out generated functions).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RunProfilerConfig {
    None,
    Cairo,
    Sierra,
}

/// Configuration of compiled tests runner.
#[derive(Clone, Debug)]
pub struct TestRunConfig {
    pub filter: String,
    pub include_ignored: bool,
    pub ignored: bool,
    /// Whether to run the profiler and how.
    pub run_profiler: RunProfilerConfig,
    /// Whether to enable gas calculation.
    pub gas_enabled: bool,
    /// Whether to print used resources after each test.
    pub print_resource_usage: bool,
}

/// The test cases compiler.
pub struct TestCompiler {
    pub db: RootDatabase,
    pub main_crate_ids: Vec<CrateId>,
    pub test_crate_ids: Vec<CrateId>,
    pub allow_warnings: bool,
    pub config: TestsCompilationConfig,
}

impl TestCompiler {
    /// Configure a new test compiler
    ///
    /// # Arguments
    ///
    /// * `path` - The path to compile and run its tests
    /// * `starknet` - Add the starknet plugin to run the tests
    pub fn try_new(
        path: &Path,
        allow_warnings: bool,
        gas_enabled: bool,
        config: TestsCompilationConfig,
    ) -> Result<Self> {
        let db = &mut {
            let mut b = RootDatabase::builder();
            let mut cfg = CfgSet::from_iter([Cfg::name("test"), Cfg::kv("target", "test")]);
            if !gas_enabled {
                cfg.insert(Cfg::kv("gas", "disabled"));
                b.skip_auto_withdraw_gas();
            }
            b.detect_corelib();
            b.with_cfg(cfg);
            b.with_default_plugin_suite(test_plugin_suite());
            if config.starknet {
                b.with_default_plugin_suite(starknet_plugin_suite());
            }
            b.build()?
        };

        let main_crate_ids = setup_project(db, Path::new(&path))?;

        Ok(Self {
            db: db.snapshot(),
            test_crate_ids: main_crate_ids.clone(),
            main_crate_ids,
            allow_warnings,
            config,
        })
    }

    /// Build the tests and collect metadata.
    pub fn build(&self) -> Result<TestCompilation> {
        let mut diag_reporter = DiagnosticsReporter::stderr().with_crates(&self.main_crate_ids);
        if self.allow_warnings {
            diag_reporter = diag_reporter.allow_warnings();
        }

        compile_test_prepared_db(
            &self.db,
            self.config.clone(),
            self.test_crate_ids.clone(),
            diag_reporter,
        )
    }
}

/// Filter compiled test cases with user provided arguments.
///
/// # Arguments
/// * `compiled` - Compiled test cases with metadata.
/// * `include_ignored` - Include ignored tests as well.
/// * `ignored` - Run ignored tests only.l
/// * `filter` - Include only tests containing the filter string.
/// # Returns
/// * (`TestCompilation`, `usize`) - The filtered test cases and the number of filtered out cases.
pub fn filter_test_cases(
    compiled: TestCompilation,
    include_ignored: bool,
    ignored: bool,
    filter: &str,
) -> (TestCompilation, usize) {
    let total_tests_count = compiled.metadata.named_tests.len();
    let named_tests = compiled
        .metadata
        .named_tests
        .into_iter()
        // Filtering unignored tests in `ignored` mode. Keep all tests in `include-ignored` mode.
        .filter(|(_, test)| !ignored || test.ignored || include_ignored)
        .map(|(func, mut test)| {
            // Un-ignoring all the tests in `include-ignored` and `ignored` mode.
            if include_ignored || ignored {
                test.ignored = false;
            }
            (func, test)
        })
        .filter(|(name, _)| name.contains(filter))
        .collect_vec();
    let filtered_out = total_tests_count - named_tests.len();
    let tests = TestCompilation {
        sierra_program: compiled.sierra_program,
        metadata: TestCompilationMetadata { named_tests, ..(compiled.metadata) },
    };
    (tests, filtered_out)
}

/// The status of a ran test.
enum TestStatus {
    Success,
    Fail(RunResultValue),
}

/// The result of a ran test.
struct TestResult {
    /// The status of the run.
    status: TestStatus,
    /// The gas usage of the run if relevant.
    gas_usage: Option<i64>,
    /// The used resources of the run.
    used_resources: StarknetExecutionResources,
    /// The profiling info of the run, if requested.
    profiling_info: Option<ProfilingInfo>,
}

/// Summary data of the ran tests.
pub struct TestsSummary {
    passed: Vec<String>,
    failed: Vec<String>,
    ignored: Vec<String>,
    failed_run_results: Vec<RunResultValue>,
}

/// Auxiliary data that is required when running tests with profiling.
pub struct PorfilingAuxData<'a> {
    pub db: &'a dyn SierraGenGroup,
    pub statements_functions: UnorderedHashMap<StatementIdx, String>,
}

/// Runs the tests and process the results for a summary.
pub fn run_tests(
    profiler_data: Option<PorfilingAuxData<'_>>,
    named_tests: Vec<(String, TestConfig)>,
    sierra_program: Program,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    contracts_info: OrderedHashMap<Felt252, ContractInfo>,
    config: &TestRunConfig,
) -> Result<TestsSummary> {
    let runner = SierraCasmRunner::new(
        sierra_program.clone(),
        if config.gas_enabled {
            Some(MetadataComputationConfig {
                function_set_costs,
                linear_gas_solver: true,
                linear_ap_change_solver: true,
                skip_non_linear_solver_comparisons: false,
                compute_runtime_costs: false,
            })
        } else {
            None
        },
        contracts_info,
        match config.run_profiler {
            RunProfilerConfig::None => None,
            RunProfilerConfig::Cairo | RunProfilerConfig::Sierra => {
                Some(ProfilingInfoCollectionConfig::default())
            }
        },
    )
    .with_context(|| "Failed setting up runner.")?;
    let suffix = if named_tests.len() != 1 { "s" } else { "" };
    println!("running {} test{}", named_tests.len(), suffix);
    let wrapped_summary = Mutex::new(Ok(TestsSummary {
        passed: vec![],
        failed: vec![],
        ignored: vec![],
        failed_run_results: vec![],
    }));

    // Run in parallel if possible. If running with db, parallelism is impossible.
    if profiler_data.is_none() {
        named_tests
            .into_par_iter()
            .map(|(name, test)| run_single_test(test, name, &runner))
            .for_each(|res| {
                update_summary(
                    &wrapped_summary,
                    res,
                    &None,
                    &sierra_program,
                    &ProfilingInfoProcessorParams {
                        process_by_original_user_function: false,
                        process_by_cairo_function: false,
                        ..ProfilingInfoProcessorParams::default()
                    },
                    config.print_resource_usage,
                );
            });
    } else {
        eprintln!("Note: Tests don't run in parallel when running with profiling.");
        named_tests
            .into_iter()
            .map(move |(name, test)| run_single_test(test, name, &runner))
            .for_each(|test_result| {
                update_summary(
                    &wrapped_summary,
                    test_result,
                    &profiler_data,
                    &sierra_program,
                    &ProfilingInfoProcessorParams::default(),
                    config.print_resource_usage,
                );
            });
    }

    wrapped_summary.into_inner().unwrap()
}

/// Runs a single test and returns a tuple of its name and result.
fn run_single_test(
    test: TestConfig,
    name: String,
    runner: &SierraCasmRunner,
) -> anyhow::Result<(String, Option<TestResult>)> {
    if test.ignored {
        return Ok((name, None));
    }
    let func = runner.find_function(name.as_str())?;
    let result = runner
        .run_function_with_starknet_context(func, vec![], test.available_gas, Default::default())
        .with_context(|| format!("Failed to run the function `{}`.", name.as_str()))?;
    Ok((
        name,
        Some(TestResult {
            status: match &result.value {
                RunResultValue::Success(_) => match test.expectation {
                    TestExpectation::Success => TestStatus::Success,
                    TestExpectation::Panics(_) => TestStatus::Fail(result.value),
                },
                RunResultValue::Panic(value) => match test.expectation {
                    TestExpectation::Success => TestStatus::Fail(result.value),
                    TestExpectation::Panics(panic_expectation) => match panic_expectation {
                        PanicExpectation::Exact(expected) if value != &expected => {
                            TestStatus::Fail(result.value)
                        }
                        _ => TestStatus::Success,
                    },
                },
            },
            gas_usage: test
                .available_gas
                .zip(result.gas_counter)
                .map(|(before, after)| {
                    before.into_or_panic::<i64>() - after.to_bigint().to_i64().unwrap()
                })
                .or_else(|| {
                    runner.initial_required_gas(func).map(|gas| gas.into_or_panic::<i64>())
                }),
            used_resources: result.used_resources,
            profiling_info: result.profiling_info,
        }),
    ))
}

/// Updates the test summary with the given test result.
fn update_summary(
    wrapped_summary: &Mutex<std::prelude::v1::Result<TestsSummary, anyhow::Error>>,
    test_result: std::prelude::v1::Result<(String, Option<TestResult>), anyhow::Error>,
    profiler_data: &Option<PorfilingAuxData<'_>>,
    sierra_program: &Program,
    profiling_params: &ProfilingInfoProcessorParams,
    print_resource_usage: bool,
) {
    let mut wrapped_summary = wrapped_summary.lock().unwrap();
    if wrapped_summary.is_err() {
        return;
    }
    let (name, opt_result) = match test_result {
        Ok((name, opt_result)) => (name, opt_result),
        Err(err) => {
            *wrapped_summary = Err(err);
            return;
        }
    };
    let summary = wrapped_summary.as_mut().unwrap();
    let (res_type, status_str, gas_usage, used_resources, profiling_info) =
        if let Some(result) = opt_result {
            let (res_type, status_str) = match result.status {
                TestStatus::Success => (&mut summary.passed, "ok".bright_green()),
                TestStatus::Fail(run_result) => {
                    summary.failed_run_results.push(run_result);
                    (&mut summary.failed, "fail".bright_red())
                }
            };
            (
                res_type,
                status_str,
                result.gas_usage,
                print_resource_usage.then_some(result.used_resources),
                result.profiling_info,
            )
        } else {
            (&mut summary.ignored, "ignored".bright_yellow(), None, None, None)
        };
    if let Some(gas_usage) = gas_usage {
        println!("test {name} ... {status_str} (gas usage est.: {gas_usage})");
    } else {
        println!("test {name} ... {status_str}");
    }
    if let Some(used_resources) = used_resources {
        let filtered = used_resources.basic_resources.filter_unused_builtins();
        // Prints the used resources per test. E.g.:
        // ```ignore
        // test cairo_level_tests::interoperability::test_contract_not_deployed ... ok (gas usage est.: 77320)
        //     steps: 42
        //     memory holes: 20
        //     builtins: ("range_check_builtin": 3)
        //     syscalls: ("CallContract": 1)
        // test cairo_level_tests::events::test_pop_log ... ok (gas usage est.: 55440)
        //     steps: 306
        //     memory holes: 35
        //     builtins: ("range_check_builtin": 24)
        //     syscalls: ("EmitEvent": 2)
        // ```
        println!("    steps: {}", filtered.n_steps);
        println!("    memory holes: {}", filtered.n_memory_holes);

        print_resource_map(
            filtered.builtin_instance_counter.into_iter().map(|(k, v)| (k.to_string(), v)),
            "builtins",
        );
        print_resource_map(used_resources.syscalls.into_iter(), "syscalls");
    }
    if let Some(profiling_info) = profiling_info {
        let Some(PorfilingAuxData { db, statements_functions }) = profiler_data else {
            panic!("profiler_data is None");
        };
        let profiling_processor = ProfilingInfoProcessor::new(
            Some(*db),
            sierra_program.clone(),
            statements_functions.clone(),
            Default::default(),
        );
        let processed_profiling_info =
            profiling_processor.process_ex(&profiling_info, profiling_params);
        println!("Profiling info:\n{processed_profiling_info}");
    }
    res_type.push(name);
}

/// Given an iterator of (String, usize) pairs, prints a usage map. E.g.:
///     syscalls: ("EmitEvent": 2)
///     syscalls: ("CallContract": 1)
fn print_resource_map(m: impl ExactSizeIterator<Item = (String, usize)>, resource_type: &str) {
    if m.len() != 0 {
        println!(
            "    {resource_type}: ({})",
            m.into_iter().sorted().map(|(k, v)| format!(r#""{k}": {v}"#)).join(", ")
        );
    }
}
