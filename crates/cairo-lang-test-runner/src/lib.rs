use std::path::Path;
use std::sync::Arc;
use std::sync::mpsc::channel;

use anyhow::{Context, Result, bail};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_debug::debug::DebugWithDb;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateInput;
use cairo_lang_runner::casm_run::{StarknetHintProcessor, format_for_panic};
use cairo_lang_runner::profiling::{
    ProfilerConfig, ProfilingInfo, ProfilingInfoProcessor, ProfilingInfoProcessorParams,
};
use cairo_lang_runner::{
    CairoHintProcessor, ProfilingInfoCollectionConfig, RunResultValue, RunnerError,
    SierraCasmRunner, StarknetExecutionResources,
};
use cairo_lang_sierra_to_casm::metadata::MetadataComputationConfig;
use cairo_lang_starknet::starknet_plugin_suite;
use cairo_lang_test_plugin::test_config::{PanicExpectation, TestExpectation};
use cairo_lang_test_plugin::{
    TestCompilation, TestCompilationMetadata, TestConfig, TestsCompilationConfig,
    compile_test_prepared_db, test_plugin_suite,
};
use cairo_lang_utils::casts::IntoOrPanic;
use colored::Colorize;
use itertools::Itertools;
use num_traits::ToPrimitive;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use salsa::Database;

#[cfg(test)]
mod test;

type ArcCustomHintProcessorFactory = Arc<
    dyn (for<'a> Fn(CairoHintProcessor<'a>) -> Box<dyn StarknetHintProcessor + 'a>) + Send + Sync,
>;

/// Compile and run tests.
pub struct TestRunner<'db> {
    compiler: TestCompiler<'db>,
    config: TestRunConfig,
    custom_hint_processor_factory: Option<ArcCustomHintProcessorFactory>,
}

impl<'db> TestRunner<'db> {
    /// Configure a new test runner
    ///
    /// # Arguments
    ///
    /// * `path` - The path to compile and run its tests
    /// * `filter` - Run only tests containing the filter string
    /// * `include_ignored` - Include ignored tests as well
    /// * `ignored` - Run ignored tests only
    /// * `starknet` - Add the Starknet plugin to run the tests
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
                add_statements_functions: config
                    .profiler_config
                    .as_ref()
                    .is_some_and(|c| c.requires_cairo_debug_info()),
                add_statements_code_locations: false,
                contract_declarations: None,
                contract_crate_ids: None,
                executable_crate_ids: None,
            },
        )?;
        Ok(Self { compiler, config, custom_hint_processor_factory: None })
    }

    /// Make this runner run tests using a custom hint processor.
    pub fn with_custom_hint_processor(
        &mut self,
        factory: impl (for<'a> Fn(CairoHintProcessor<'a>) -> Box<dyn StarknetHintProcessor + 'a>)
        + Send
        + Sync
        + 'static,
    ) -> &mut Self {
        self.custom_hint_processor_factory = Some(Arc::new(factory));
        self
    }

    /// Runs the tests and processes the results for a summary.
    pub fn run(&self) -> Result<Option<TestsSummary>> {
        let runner = CompiledTestRunner {
            compiled: self.compiler.build()?,
            config: self.config.clone(),
            custom_hint_processor_factory: self.custom_hint_processor_factory.clone(),
        };
        runner.run(Some(&self.compiler.db))
    }
}

pub struct CompiledTestRunner<'db> {
    compiled: TestCompilation<'db>,
    config: TestRunConfig,
    custom_hint_processor_factory: Option<ArcCustomHintProcessorFactory>,
}

impl<'db> CompiledTestRunner<'db> {
    /// Configure a new compiled test runner
    ///
    /// # Arguments
    ///
    /// * `compiled` - The compiled tests to run
    /// * `config` - Test run configuration
    pub fn new(compiled: TestCompilation<'db>, config: TestRunConfig) -> Self {
        Self { compiled, config, custom_hint_processor_factory: None }
    }

    /// Make this runner run tests using a custom hint processor.
    pub fn with_custom_hint_processor(
        &mut self,
        factory: impl (for<'a> Fn(CairoHintProcessor<'a>) -> Box<dyn StarknetHintProcessor + 'a>)
        + Send
        + Sync
        + 'static,
    ) -> &mut Self {
        self.custom_hint_processor_factory = Some(Arc::new(factory));
        self
    }

    /// Execute preconfigured test execution.
    pub fn run(self, opt_db: Option<&'db RootDatabase>) -> Result<Option<TestsSummary>> {
        let (compiled, filtered_out) = filter_test_cases(
            self.compiled,
            self.config.include_ignored,
            self.config.ignored,
            &self.config.filter,
        );

        let TestsSummary { passed, failed, ignored, failed_run_results } = run_tests(
            opt_db.map(|db| db as &dyn Database),
            compiled,
            &self.config,
            self.custom_hint_processor_factory,
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
                    Ok(RunResultValue::Success(_)) => {
                        println!("expected panic but finished successfully.");
                    }
                    Ok(RunResultValue::Panic(values)) => {
                        println!("{}", format_for_panic(values.into_iter()));
                    }
                    Err(err) => {
                        println!("{err}");
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

/// Configuration of compiled tests runner.
#[derive(Clone, Debug)]
pub struct TestRunConfig {
    pub filter: String,
    pub include_ignored: bool,
    pub ignored: bool,
    /// Whether to run the profiler and how.
    pub profiler_config: Option<ProfilerConfig>,
    /// Whether to enable gas calculation.
    pub gas_enabled: bool,
    /// Whether to print used resources after each test.
    pub print_resource_usage: bool,
}

/// The test cases compiler.
pub struct TestCompiler<'db> {
    pub db: RootDatabase,
    pub main_crate_ids: Vec<CrateInput>,
    pub test_crate_ids: Vec<CrateInput>,
    pub allow_warnings: bool,
    pub config: TestsCompilationConfig<'db>,
}

impl<'db> TestCompiler<'db> {
    /// Configure a new test compiler
    ///
    /// # Arguments
    ///
    /// * `path` - The path to compile and run its tests
    /// * `starknet` - Add the Starknet plugin to run the tests
    pub fn try_new(
        path: &Path,
        allow_warnings: bool,
        gas_enabled: bool,
        config: TestsCompilationConfig<'db>,
    ) -> Result<Self> {
        let mut db = {
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

        let main_crate_inputs = setup_project(&mut db, Path::new(&path))?;

        Ok(Self {
            db: db.snapshot(),
            test_crate_ids: main_crate_inputs.clone(),
            main_crate_ids: main_crate_inputs,
            allow_warnings,
            config,
        })
    }

    /// Build the tests and collect metadata.
    pub fn build(&self) -> Result<TestCompilation<'_>> {
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
pub fn filter_test_cases<'db>(
    compiled: TestCompilation<'db>,
    include_ignored: bool,
    ignored: bool,
    filter: &str,
) -> (TestCompilation<'db>, usize) {
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
        metadata: TestCompilationMetadata { named_tests, ..compiled.metadata },
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
    failed_run_results: Vec<Result<RunResultValue>>,
}

/// Runs the tests and process the results for a summary.
pub fn run_tests(
    opt_db: Option<&dyn Database>,
    compiled: TestCompilation<'_>,
    config: &TestRunConfig,
    custom_hint_processor_factory: Option<ArcCustomHintProcessorFactory>,
) -> Result<TestsSummary> {
    let TestCompilation {
        sierra_program: sierra_program_with_debug_info,
        metadata:
            TestCompilationMetadata {
                named_tests,
                function_set_costs,
                contracts_info,
                statements_locations,
            },
    } = compiled;
    let sierra_program = sierra_program_with_debug_info.program;
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
        config.profiler_config.as_ref().map(ProfilingInfoCollectionConfig::from_profiler_config),
    )
    .map_err(|err| {
        let (RunnerError::BuildError(err), Some(db), Some(statements_locations)) =
            (&err, opt_db, &statements_locations)
        else {
            return err.into();
        };
        let mut locs = vec![];
        for stmt_idx in err.stmt_indices() {
            if let Some(loc) = statements_locations.statement_diagnostic_location(db, stmt_idx) {
                locs.push(format!("#{stmt_idx} {:?}", loc.debug(db)))
            }
        }
        anyhow::anyhow!("{err}\n{}", locs.join("\n"))
    })
    .with_context(|| "Failed setting up runner.")?;
    let suffix = if named_tests.len() != 1 { "s" } else { "" };
    println!("running {} test{}", named_tests.len(), suffix);

    let (tx, rx) = channel::<_>();
    rayon::spawn(move || {
        named_tests.into_par_iter().for_each(|(name, test)| {
            let result =
                run_single_test(test, &name, &runner, custom_hint_processor_factory.clone());
            tx.send((name, result)).unwrap();
        })
    });

    let profiler_data = config.profiler_config.as_ref().and_then(|profiler_config| {
        if !profiler_config.requires_cairo_debug_info() {
            return None;
        }

        let db = opt_db.expect("db must be passed when doing cairo level profiling.");
        Some((
            ProfilingInfoProcessor::new(
                Some(db),
                &sierra_program,
                statements_locations
                    .expect(
                        "statements locations must be present when doing cairo level profiling.",
                    )
                    .get_statements_functions_map_for_tests(db),
            ),
            ProfilingInfoProcessorParams::from_profiler_config(profiler_config),
        ))
    });

    let mut summary = TestsSummary {
        passed: vec![],
        failed: vec![],
        ignored: vec![],
        failed_run_results: vec![],
    };
    while let Ok((name, result)) = rx.recv() {
        update_summary(&mut summary, name, result, &profiler_data, config.print_resource_usage);
    }

    Ok(summary)
}

/// Runs a single test and returns a tuple of its name and result.
fn run_single_test(
    test: TestConfig,
    name: &str,
    runner: &SierraCasmRunner,
    custom_hint_processor_factory: Option<ArcCustomHintProcessorFactory>,
) -> Result<Option<TestResult>> {
    if test.ignored {
        return Ok(None);
    }
    let func = runner.find_function(name)?;

    let (hint_processor, ctx) =
        runner.prepare_starknet_context(func, vec![], test.available_gas, Default::default())?;

    let mut hint_processor = match custom_hint_processor_factory {
        Some(f) => f(hint_processor),
        None => Box::new(hint_processor),
    };

    let result =
        runner.run_function_with_prepared_starknet_context(func, &mut *hint_processor, ctx)?;

    Ok(Some(TestResult {
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
            .or_else(|| runner.initial_required_gas(func).map(|gas| gas.into_or_panic::<i64>())),
        used_resources: result.used_resources,
        profiling_info: result.profiling_info,
    }))
}

/// Updates the test summary with the given test result.
fn update_summary(
    summary: &mut TestsSummary,
    name: String,
    test_result: Result<Option<TestResult>>,
    profiler_data: &Option<(ProfilingInfoProcessor<'_>, ProfilingInfoProcessorParams)>,
    print_resource_usage: bool,
) {
    let (res_type, status_str, gas_usage, used_resources, profiling_info) = match test_result {
        Ok(None) => (&mut summary.ignored, "ignored".bright_yellow(), None, None, None),
        Err(err) => {
            summary.failed_run_results.push(Err(err));
            (&mut summary.failed, "failed to run".bright_magenta(), None, None, None)
        }
        Ok(Some(result)) => {
            let (res_type, status_str) = match result.status {
                TestStatus::Success => (&mut summary.passed, "ok".bright_green()),
                TestStatus::Fail(run_result) => {
                    summary.failed_run_results.push(Ok(run_result));
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
        }
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
    if let Some((profiling_processor, profiling_params)) = profiler_data {
        let processed_profiling_info = profiling_processor.process(
            &profiling_info.expect("profiling_info must be Some when profiler_config is Some"),
            profiling_params,
        );
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
