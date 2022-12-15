//! Compiles and runs a Cairo program.

use std::path::Path;
use std::sync::Mutex;

use anyhow::{bail, Context};
use clap::Parser;
use colored::Colorize;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_and_eprint_diagnostics;
use compiler::project::setup_project;
use debug::DebugWithDb;
use defs::ids::{FreeFunctionId, GenericFunctionId, ModuleItemId};
use diagnostics::ToOption;
use filesystem::ids::CrateId;
use itertools::Itertools;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use runner::SierraCasmRunner;
use semantic::db::SemanticGroup;
use semantic::{ConcreteFunction, FunctionLongId};
use sierra_generator::db::SierraGenGroup;
use sierra_generator::replace_ids::replace_sierra_ids_in_program;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The file to compile and run.
    #[arg(short, long)]
    path: String,
}

/// The status of a ran test.
enum TestStatus {
    Success,
    Fail,
    Ignore,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let mut db_val = RootDatabase::default();
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
    let TestsSummary { passed, failed, ignored } = run_tests(named_tests, sierra_program)?;
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
        for failure in &failed {
            println!("   {failure}");
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

/// Summary data of the ran tests.
struct TestsSummary {
    passed: Vec<String>,
    failed: Vec<String>,
    ignored: Vec<String>,
}

/// Runs the tests and process the results for a summary.
fn run_tests(
    named_tests: Vec<(String, TestConfig)>,
    sierra_program: sierra::program::Program,
) -> anyhow::Result<TestsSummary> {
    println!("running {} tests", named_tests.len());
    let wrapped_summary =
        Mutex::new(Ok(TestsSummary { passed: vec![], failed: vec![], ignored: vec![] }));
    named_tests
        .into_par_iter()
        .map(|(name, test)| -> anyhow::Result<(String, TestStatus)> {
            if test.ignored {
                return Ok((name, TestStatus::Ignore));
            }
            let runner = SierraCasmRunner::new(sierra_program.clone(), false)
                .with_context(|| "Failed setting up runner.")?;
            let result = runner
                .run_function(name.as_str(), &[], &None)
                .with_context(|| "Failed to run the function.")?;
            Ok((
                name,
                match (result.value, test.expectation) {
                    (runner::RunResultValue::Success(_), TestExpectation::Success)
                    | (runner::RunResultValue::Panic(_), TestExpectation::Panics) => {
                        TestStatus::Success
                    }
                    (runner::RunResultValue::Success(_), TestExpectation::Panics)
                    | (runner::RunResultValue::Panic(_), TestExpectation::Success) => {
                        TestStatus::Fail
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
                TestStatus::Fail => (&mut summary.failed, "fail".bright_red()),
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
                        let mut ignored = false;
                        let mut should_panic = false;
                        for attr in attrs {
                            match attr.id.as_str() {
                                "test" => {
                                    is_test = true;
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
