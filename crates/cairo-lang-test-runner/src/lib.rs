use std::path::Path;
use std::sync::{Arc, Mutex};
use std::vec::IntoIter;

use anyhow::{bail, Context, Result};
use cairo_felt::{felt_str, Felt252};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_runner::short_string::{as_cairo_short_string, as_cairo_short_string_ex};
use cairo_lang_runner::{RunResultValue, SierraCasmRunner};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::Program;
use cairo_lang_sierra_to_casm::metadata::MetadataComputationConfig;
use cairo_lang_starknet::contract::ContractInfo;
use cairo_lang_starknet::inline_macros::selector::SelectorMacro;
use cairo_lang_starknet::plugin::StarkNetPlugin;
use cairo_lang_test_plugin::test_config::{
    PanicExpectation, TestExpectation, BYTES_IN_WORD, BYTE_ARRAY_PANIC_MAGIC,
};
use cairo_lang_test_plugin::{compile_test_prepared_db, TestCompilation, TestConfig, TestPlugin};
use cairo_lang_utils::casts::IntoOrPanic;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use colored::Colorize;
use itertools::Itertools;
use num_traits::ToPrimitive;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

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
    pub fn new(path: &Path, starknet: bool, config: TestRunConfig) -> Result<Self> {
        let compiler = TestCompiler::try_new(path, starknet)?;
        Ok(Self { compiler, config })
    }

    /// Runs the tests and process the results for a summary.
    pub fn run(&self) -> Result<Option<TestsSummary>> {
        let runner = CompiledTestRunner::new(self.compiler.build()?, self.config.clone());
        runner.run()
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
    pub fn run(self) -> Result<Option<TestsSummary>> {
        let (compiled, filtered_out) = filter_test_cases(
            self.compiled,
            self.config.include_ignored,
            self.config.ignored,
            self.config.filter,
        );

        let TestsSummary { passed, failed, ignored, failed_run_results } = run_tests(
            compiled.named_tests,
            compiled.sierra_program,
            compiled.function_set_costs,
            compiled.contracts_info,
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
                        print!("panicked with ");
                        let mut values_iter = values.into_iter();
                        let mut items = Vec::new();
                        while let Some(item) = next_printed_item(&mut values_iter) {
                            items.push(item);
                        }
                        if items.len() == 1 {
                            println!("{}.", items[0]);
                        } else {
                            println!("({}).", items.join(", "));
                        }
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

/// Formats a string or a `Felt252`.
fn next_printed_item(values: &mut IntoIter<Felt252>) -> Option<String> {
    let Some(first_felt) = values.next() else {
        return None;
    };

    Some(if first_felt == felt_str!(BYTE_ARRAY_PANIC_MAGIC, 16) {
        get_formatted_string(values)
    } else {
        get_formatted_short_string(&first_felt)
    })
}

/// Formats a `Felt252`, as a short string if possible.
fn get_formatted_short_string(value: &Felt252) -> String {
    let hex_value = value.to_biguint();
    match as_cairo_short_string(value) {
        Some(as_string) => format!("{hex_value:#x} ('{as_string}')"),
        None => format!("{hex_value:#x}"),
    }
}

/// Formats a string, represented as a sequence of `Felt252`s.
/// Assumes the sequence is a valid serialization of a ByteArray.
fn get_formatted_string(values: &mut IntoIter<Felt252>) -> String {
    let num_full_words = values.next().unwrap().to_usize().unwrap();
    let full_words = values.by_ref().take(num_full_words).collect_vec();
    let pending_word = values.next().unwrap();
    let pending_word_len = values.next().unwrap().to_usize().unwrap();

    let full_words_strings = full_words
        .into_iter()
        .map(|word| as_cairo_short_string_ex(&word, BYTES_IN_WORD))
        .collect::<Option<Vec<String>>>();
    let Some(full_words_strings) = full_words_strings else {
        return "<bad ByteArray serialization - full word>".to_string();
    };
    let full_words_string = full_words_strings.join("");
    let Some(pending_word_string) = as_cairo_short_string_ex(&pending_word, pending_word_len)
    else {
        return "<bad ByteArray serialization - pending word>".to_string();
    };
    format!("\"{full_words_string}{pending_word_string}\"")
}

/// Configuration of compiled tests runner.
#[derive(Clone, Debug)]
pub struct TestRunConfig {
    pub filter: String,
    pub include_ignored: bool,
    pub ignored: bool,
}

/// The test cases compiler.
pub struct TestCompiler {
    pub db: RootDatabase,
    pub main_crate_ids: Vec<CrateId>,
    pub test_crate_ids: Vec<CrateId>,
    pub starknet: bool,
}

impl TestCompiler {
    /// Configure a new test compiler
    ///
    /// # Arguments
    ///
    /// * `path` - The path to compile and run its tests
    /// * `starknet` - Add the starknet plugin to run the tests
    pub fn try_new(path: &Path, starknet: bool) -> Result<Self> {
        let db = &mut {
            let mut b = RootDatabase::builder();
            b.detect_corelib();
            b.with_cfg(CfgSet::from_iter([Cfg::name("test")]));
            b.with_macro_plugin(Arc::new(TestPlugin::default()));

            if starknet {
                b.with_macro_plugin(Arc::new(StarkNetPlugin::default()))
                    .with_inline_macro_plugin(SelectorMacro::NAME, Arc::new(SelectorMacro));
            }

            b.build()?
        };

        let main_crate_ids = setup_project(db, Path::new(&path))?;

        if DiagnosticsReporter::stderr().with_extra_crates(&main_crate_ids).check(db) {
            bail!("failed to compile: {}", path.display());
        }

        Ok(Self {
            db: db.snapshot(),
            test_crate_ids: main_crate_ids.clone(),
            main_crate_ids,
            starknet,
        })
    }

    /// Build the tests and collect metadata.
    pub fn build(&self) -> Result<TestCompilation> {
        compile_test_prepared_db(
            &self.db,
            self.starknet,
            self.main_crate_ids.clone(),
            self.test_crate_ids.clone(),
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
    filter: String,
) -> (TestCompilation, usize) {
    let total_tests_count = compiled.named_tests.len();
    let named_tests = compiled.named_tests
        .into_iter()
        .map(|(func, mut test)| {
            // Un-ignoring all the tests in `include-ignored` mode.
            if include_ignored {
                test.ignored = false;
            }
            (func, test)
        })
        .filter(|(name, _)| name.contains(&filter))
        // Filtering unignored tests in `ignored` mode
        .filter(|(_, test)| !ignored || test.ignored)
        .collect_vec();
    let filtered_out = total_tests_count - named_tests.len();
    let tests = TestCompilation { named_tests, ..compiled };
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
}

/// Summary data of the ran tests.
pub struct TestsSummary {
    passed: Vec<String>,
    failed: Vec<String>,
    ignored: Vec<String>,
    failed_run_results: Vec<RunResultValue>,
}

/// Runs the tests and process the results for a summary.
pub fn run_tests(
    named_tests: Vec<(String, TestConfig)>,
    sierra_program: Program,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    contracts_info: OrderedHashMap<Felt252, ContractInfo>,
) -> Result<TestsSummary> {
    let runner = SierraCasmRunner::new(
        sierra_program,
        Some(MetadataComputationConfig { function_set_costs }),
        contracts_info,
    )
    .with_context(|| "Failed setting up runner.")?;
    println!("running {} tests", named_tests.len());
    let wrapped_summary = Mutex::new(Ok(TestsSummary {
        passed: vec![],
        failed: vec![],
        ignored: vec![],
        failed_run_results: vec![],
    }));
    named_tests
        .into_par_iter()
        .map(|(name, test)| -> anyhow::Result<(String, Option<TestResult>)> {
            if test.ignored {
                return Ok((name, None));
            }
            let func = runner.find_function(name.as_str())?;
            let result = runner
                .run_function_with_starknet_context(
                    func,
                    &[],
                    test.available_gas,
                    Default::default(),
                )
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
                }),
            ))
        })
        .for_each(|r| {
            let mut wrapped_summary = wrapped_summary.lock().unwrap();
            if wrapped_summary.is_err() {
                return;
            }
            let (name, status) = match r {
                Ok((name, status)) => (name, status),
                Err(err) => {
                    *wrapped_summary = Err(err);
                    return;
                }
            };
            let summary = wrapped_summary.as_mut().unwrap();
            let (res_type, status_str, gas_usage) = match status {
                Some(TestResult { status: TestStatus::Success, gas_usage }) => {
                    (&mut summary.passed, "ok".bright_green(), gas_usage)
                }
                Some(TestResult { status: TestStatus::Fail(run_result), gas_usage }) => {
                    summary.failed_run_results.push(run_result);
                    (&mut summary.failed, "fail".bright_red(), gas_usage)
                }
                None => (&mut summary.ignored, "ignored".bright_yellow(), None),
            };
            if let Some(gas_usage) = gas_usage {
                println!("test {name} ... {status_str} (gas usage est.: {gas_usage})");
            } else {
                println!("test {name} ... {status_str}");
            }
            res_type.push(name);
        });
    wrapped_summary.into_inner().unwrap()
}
