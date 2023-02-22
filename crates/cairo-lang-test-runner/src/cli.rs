//! Compiles and runs a Cairo program.

use std::collections::HashSet;
use std::path::Path;
use std::sync::{Arc, Mutex};

use anyhow::{bail, Context};
use cairo_felt::Felt;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_plugins::config::ConfigPlugin;
use cairo_lang_plugins::derive::DerivePlugin;
use cairo_lang_plugins::panicable::PanicablePlugin;
use cairo_lang_runner::short_string::as_cairo_short_string;
use cairo_lang_runner::{RunResultValue, SierraCasmRunner};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::Attribute;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::literals::LiteralLongId;
use cairo_lang_semantic::plugin::SemanticPlugin;
use cairo_lang_semantic::{ConcreteFunction, ConcreteFunctionWithBodyId, FunctionLongId};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::replace_sierra_ids_in_program;
use cairo_lang_starknet::plugin::StarkNetPlugin;
use cairo_lang_syntax::node::{ast, Terminal, Token, TypedSyntaxNode};
use clap::Parser;
use colored::Colorize;
use itertools::Itertools;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use unescaper::unescape;

/// Command line args parser.
/// Exits with 0/1 if the input is formatted correctly/incorrectly.
#[derive(Parser, Debug)]
#[clap(version, verbatim_doc_comment)]
struct Args {
    /// The path to compile and run its tests.
    #[arg(short, long)]
    path: String,
    /// The filter for the tests, running only tests containing the filter string.
    #[arg(short, long, default_value_t = String::default())]
    filter: String,
    /// Should we run ignored tests as well.
    #[arg(long, default_value_t = false)]
    include_ignored: bool,
    /// Should we run only the ignored tests.
    #[arg(long, default_value_t = false)]
    ignored: bool,
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
    let db = &mut RootDatabase::builder().with_plugins(plugins).detect_corelib().build()?;

    let main_crate_ids = setup_project(db, Path::new(&args.path))?;

    if DiagnosticsReporter::stderr().check(db) {
        bail!("failed to compile: {}", args.path);
    }
    let all_tests = find_all_tests(db, main_crate_ids);
    let sierra_program = db
        .get_sierra_program_for_functions(
            all_tests
                .iter()
                .flat_map(|t| ConcreteFunctionWithBodyId::from_no_generics_free(db, t.func_id))
                .collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?;
    let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);
    let total_tests_count = all_tests.len();
    let named_tests = all_tests
        .into_iter()
        .map(|mut test| {
            // Un-ignoring all the tests in `include-ignored` mode.
            if args.include_ignored {
                test.ignored = false;
            }
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
        .filter(|(name, _)| name.contains(&args.filter))
        // Filtering unignored tests in `ignored` mode.
        .filter(|(_, test)| !args.ignored || test.ignored)
        .collect_vec();
    let filtered_out = total_tests_count - named_tests.len();
    let TestsSummary { passed, failed, ignored, failed_run_results } =
        run_tests(named_tests, sierra_program)?;
    if failed.is_empty() {
        println!(
            "test result: {}. {} passed; {} failed; {} ignored; {filtered_out} filtered out;",
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
    sierra_program: cairo_lang_sierra::program::Program,
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
                .with_context(|| format!("Failed to run the function `{}`.", name.as_str()))?;
            Ok((
                name,
                match (&result.value, test.expectation) {
                    (RunResultValue::Success(_), TestExpectation::Success) | (RunResultValue::Panic(_), TestExpectation::Panics(PanicExpectation::Any)) => TestStatus::Success,
                    (RunResultValue::Panic(value), TestExpectation::Panics(PanicExpectation::Exact(expected))) if value == &expected  => TestStatus::Success,
                    _ => {
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

/// Expectation for a panic case.
enum PanicExpectation {
    /// Accept any panic value.
    Any,
    /// Accept only this specific vector of panics.
    Exact(Vec<Felt>),
}

/// Expectation for a result of a test.
enum TestExpectation {
    /// Running the test should not panic.
    Success,
    /// Running the test should result in a panic.
    Panics(PanicExpectation),
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

            for item in module_items.iter() {
                if let ModuleItemId::FreeFunction(func_id) = item {
                    if let Ok(attrs) =
                        db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id))
                    {
                        let mut is_test = false;
                        let mut available_gas = None;
                        let mut ignored = false;
                        let mut should_panic = false;
                        let mut expected_panic_value = None;
                        for attr in attrs {
                            match attr.id.as_str() {
                                "test" => {
                                    is_test = true;
                                }
                                "available_gas" => {
                                    // TODO(orizi): Provide diagnostics when this does not match.
                                    if let [ast::Expr::Literal(literal)] = &attr.args[..] {
                                        available_gas = literal
                                            .token(db.upcast())
                                            .text(db.upcast())
                                            .parse::<usize>()
                                            .ok();
                                    }
                                }
                                "should_panic" => {
                                    should_panic = true;
                                    expected_panic_value = extract_panic_values(db, attr);
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
                                    TestExpectation::Panics(
                                        if let Some(values) = expected_panic_value {
                                            PanicExpectation::Exact(values)
                                        } else {
                                            PanicExpectation::Any
                                        },
                                    )
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

/// Tries to extract the relevant expected panic values.
fn extract_panic_values(db: &dyn SemanticGroup, attr: Attribute) -> Option<Vec<Felt>> {
    // TODO(orizi): Provide diagnostics when this does not match.
    let [ast::Expr::Binary(binary)] = &attr.args[..] else { return None };
    let syntax_db = db.upcast();
    if !matches!(binary.op(syntax_db), ast::BinaryOperator::Eq(_)) {
        return None;
    }
    if binary.lhs(syntax_db).as_syntax_node().get_text_without_trivia(syntax_db) != "expected" {
        return None;
    }
    let ast::Expr::Tuple(panics) = binary.rhs(syntax_db) else { return None };
    panics
        .expressions(syntax_db)
        .elements(syntax_db)
        .into_iter()
        .map(|value| match value {
            ast::Expr::Literal(literal) => Felt::try_from(
                LiteralLongId::try_from(literal.token(syntax_db).text(syntax_db)).ok()?.value,
            )
            .ok(),
            ast::Expr::ShortString(short_string_syntax) => {
                let text = short_string_syntax.text(syntax_db);
                let (literal, _) = text[1..].rsplit_once('\'')?;
                let unescaped_literal = unescape(literal).ok()?;
                Some(Felt::from_bytes_be(unescaped_literal.as_bytes()))
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()
}
