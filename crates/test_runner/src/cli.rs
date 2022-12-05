//! Compiles and runs a Cairo program.

use std::path::Path;

use anyhow::{Context, Ok};
use clap::Parser;
use colored::Colorize;
use compiler::db::RootDatabase;
use compiler::diagnostics::check_and_eprint_diagnostics;
use compiler::project::setup_project;
use debug::DebugWithDb;
use defs::ids::{FreeFunctionId, GenericFunctionId, ModuleItemId};
use filesystem::ids::CrateId;
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
        .with_context(|| "Compilation failed without any diagnostics.")?;
    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);
    let mut fail_count = 0;
    let mut pass_count = 0;
    let mut ignore_count = 0;
    for test in &all_tests {
        let name = format!(
            "{:?}",
            FunctionLongId {
                function: ConcreteFunction {
                    generic_function: GenericFunctionId::Free(test.func_id),
                    generic_args: vec![]
                }
            }
            .debug(db)
        );
        print!("{name} - ");
        if test.ignored {
            ignore_count += 1;
            println!("{}", "ignored".bright_yellow());
            continue;
        }
        let runner = SierraCasmRunner::new(sierra_program.clone(), false)
            .with_context(|| "Failed setting up runner.")?;
        let result = runner
            .run_function(name.as_str(), &[], &None)
            .with_context(|| "Failed to run the function.")?;
        match result.value {
            runner::RunResultValue::Success(_) => match test.expectation {
                TestExpectation::Success => {
                    pass_count += 1;
                    println!("{}", "ok".bright_green())
                }
                TestExpectation::Panics => {
                    fail_count += 1;
                    println!("{}", "fail".bright_red())
                }
            },
            runner::RunResultValue::Panic(_) => match test.expectation {
                TestExpectation::Success => {
                    fail_count += 1;
                    println!("{}", "fail".bright_red())
                }
                TestExpectation::Panics => {
                    pass_count += 1;
                    println!("{}", "ok".bright_green())
                }
            },
        }
    }
    println!(
        "{} tests, {} passed, {} failed, {} ignored.",
        all_tests.len(),
        pass_count,
        fail_count,
        ignore_count
    );
    if fail_count > 0 {
        anyhow::bail!("{fail_count} tests failed!");
    }
    Ok(())
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
            let Some(module_items) = db.module_items(*module_id) else {
                continue;
            };

            for item in module_items.items.values() {
                if let ModuleItemId::FreeFunction(func_id) = item {
                    if let Some(attrs) = db.free_function_declaration_attributes(*func_id) {
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
