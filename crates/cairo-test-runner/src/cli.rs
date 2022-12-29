//! Compiles and runs a Cairo program.

use std::collections::HashSet;
use std::path::Path;
use std::sync::{Arc, Mutex};

use anyhow::{bail, Context};
use cairo_compiler::db::RootDatabase;
use cairo_compiler::diagnostics::check_and_eprint_diagnostics;
use cairo_compiler::project::setup_project;
use cairo_debug::DebugWithDb;
use cairo_defs::ids::{FreeFunctionId, GenericFunctionId, ModuleItemId};
use cairo_diagnostics::ToOption;
use cairo_filesystem::ids::CrateId;
use cairo_plugins::config::ConfigPlugin;
use cairo_plugins::derive::DerivePlugin;
use cairo_plugins::panicable::PanicablePlugin;
use cairo_runner::{RunResultValue, SierraCasmRunner};
use cairo_semantic::db::SemanticGroup;
use cairo_semantic::plugin::SemanticPlugin;
use cairo_semantic::{ConcreteFunction, FunctionLongId};
use cairo_sierra_generator::db::SierraGenGroup;
use cairo_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_starknet::plugin::StarkNetPlugin;
use cairo_syntax::node::ast::Expr;
use cairo_syntax::node::Token;
use clap::Parser;
use colored::Colorize;
use itertools::Itertools;
use num_bigint::BigInt;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile and run.
    #[arg(short, long)]
    path: String,
    /// Should we add the starknet plugin to run the tests.
    #[arg(long, default_value_t = false)]
    starknet: bool,
}

/// The status of a ran test.
enum TestStatus {
    Success,
    Fail(RunResultValue),
    Ignore,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // TODO(orizi): Use `get_default_plugins` and just update the config plugin.
    let mut plugins: Vec<Arc<dyn SemanticPlugin>> = vec![
        Arc::new(DerivePlugin {}),
        Arc::new(PanicablePlugin {}),
        Arc::new(ConfigPlugin { configs: HashSet::from(["test".to_string()]) }),
    ];
    if args.starknet {
        plugins.push(Arc::new(StarkNetPlugin {}));
    }
    let mut db_val = RootDatabase::new(plugins);
    let db = &mut db_val;

    let main_crate_ids = setup_project(db, Path::new(&args.path))?;

    if check_and_eprint_diagnostics(db) {
        anyhow::bail!("failed to compile: {}", args.path);
    }
    let all_tests = find_all_tests(db, main_crate_ids);
    let sierra_program = db
        .get_sierra_program_for_functions(all_tests.iter().map(|t| t.func_id).collect())
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;
    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);
    let named_tests = all_tests
        .into_iter()
        .map(|test| {
            (
                format!(
                    "{:?}",
                    FunctionLongId {
                        function: ConcreteFunction {
                            generic_function: GenericFunctionId::Free(test.func_id),
                            generic_args: vec![]
                        }
                    }
                    .debug(db)
                ),
                test,
            )
        })
        .collect_vec();
    let TestsSummary { passed, failed, ignored, failed_run_results } =
        run_tests(named_tests, sierra_program)?;
    if failed.is_empty() {
        println!(
            "test result: {}. {} passed; {} failed; {} ignored",
            "ok".bright_green(),
            passed.len(),
            failed.len(),
            ignored.len()
        );
        Ok(())
    } else {
        println!("failures:");
        for (failure, run_result) in failed.iter().zip_eq(failed_run_results) {
            print!("   {failure} - ");
            match run_result {
                RunResultValue::Success(_) => {
                    println!("expected panic but finished successfully.");
                }
                RunResultValue::Panic(values) => {
                    print!("panicked with [");
                    for value in &values {
                        match as_cairo_short_string(value) {
                            Some(as_string) => print!("{value} ('{as_string}'), "),
                            None => print!("{value}, "),
                        }
                    }
                    println!("].")
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

/// Converts a bigint representing a felt to a Cairo short-string.
fn as_cairo_short_string(value: &BigInt) -> Option<String> {
    let mut as_string = String::default();
    let mut is_end = false;
    for byte in value.to_bytes_be().1 {
        if byte == 0 {
            is_end = true;
        } else if is_end || !byte.is_ascii() {
            return None;
        } else {
            as_string.push(byte as char);
        }
    }
    Some(as_string)
}

/// Summary data of the ran tests.
struct TestsSummary {
    passed: Vec<String>,
    failed: Vec<String>,
    ignored: Vec<String>,
    failed_run_results: Vec<RunResultValue>,
}

/// Runs the tests and process the results for a summary.
fn run_tests(
    named_tests: Vec<(String, TestConfig)>,
    sierra_program: cairo_sierra::program::Program,
) -> anyhow::Result<TestsSummary> {
    let runner =
        SierraCasmRunner::new(sierra_program, true).with_context(|| "Failed setting up runner.")?;
    println!("running {} tests", named_tests.len());
    let wrapped_summary = Mutex::new(Ok(TestsSummary {
        passed: vec![],
        failed: vec![],
        ignored: vec![],
        failed_run_results: vec![],
    }));
    named_tests
        .into_par_iter()
        .map(|(name, test)| -> anyhow::Result<(String, TestStatus)> {
            if test.ignored {
                return Ok((name, TestStatus::Ignore));
            }
            let result = runner
                .run_function(name.as_str(), &[], test.available_gas)
                .with_context(|| "Failed to run the function.")?;
            Ok((
                name,
                match (&result.value, test.expectation) {
                    (RunResultValue::Success(_), TestExpectation::Success)
                    | (RunResultValue::Panic(_), TestExpectation::Panics) => TestStatus::Success,
                    (RunResultValue::Success(_), TestExpectation::Panics)
                    | (RunResultValue::Panic(_), TestExpectation::Success) => {
                        TestStatus::Fail(result.value)
                    }
                },
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
            let (res_type, status_str) = match status {
                TestStatus::Success => (&mut summary.passed, "ok".bright_green()),
                TestStatus::Fail(run_result) => {
                    summary.failed_run_results.push(run_result);
                    (&mut summary.failed, "fail".bright_red())
                }
                TestStatus::Ignore => (&mut summary.ignored, "ignored".bright_yellow()),
            };
            println!("test {name} ... {status_str}",);
            res_type.push(name);
        });
    wrapped_summary.into_inner().unwrap()
}

/// Expectation for a result of a test.
enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics,
}

/// The configuration for running a single test.
struct TestConfig {
    /// The function id of the test function.
    func_id: FreeFunctionId,
    /// The amount of gas the test requested.
    available_gas: Option<usize>,
    /// The expected result of the run.
    expectation: TestExpectation,
    /// Should the test be ignored.
    ignored: bool,
}

/// Finds the tests in the requested crates.
fn find_all_tests(db: &dyn SemanticGroup, main_crates: Vec<CrateId>) -> Vec<TestConfig> {
    let mut tests = vec![];
    for crate_id in main_crates {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Ok(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for item in module_items.items.values() {
                if let ModuleItemId::FreeFunction(func_id) = item {
                    if let Ok(attrs) = db.free_function_declaration_attributes(*func_id) {
                        let mut is_test = false;
                        let mut available_gas = None;
                        let mut ignored = false;
                        let mut should_panic = false;
                        for attr in attrs {
                            match attr.id.as_str() {
                                "test" => {
                                    is_test = true;
                                }
                                "available_gas" => {
                                    // TODO(orizi): Provide diagnostics when this does not match.
                                    if let [Expr::Literal(literal)] = &attr.args[..] {
                                        available_gas = literal
                                            .token(db.upcast())
                                            .text(db.upcast())
                                            .parse::<usize>()
                                            .ok();
                                    }
                                }
                                "should_panic" => {
                                    should_panic = true;
                                }
                                "ignore" => {
                                    ignored = true;
                                }
                                _ => {}
                            }
                        }
                        if is_test {
                            tests.push(TestConfig {
                                func_id: *func_id,
                                available_gas,
                                expectation: if should_panic {
                                    TestExpectation::Panics
                                } else {
                                    TestExpectation::Success
                                },
                                ignored,
                            })
                        }
                    }
                }
            }
        }
    }
    tests
}
