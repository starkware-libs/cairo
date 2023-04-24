use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};

use anyhow::{bail, Context, Result};
use cairo_felt::Felt252;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_plugins::get_default_plugins;
use cairo_lang_runner::short_string::as_cairo_short_string;
use cairo_lang_runner::{RunResultValue, SierraCasmRunner};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{DebugReplacer, SierraIdReplacer};
use cairo_lang_sierra_to_casm::metadata::MetadataComputationConfig;
use cairo_lang_starknet::casm_contract_class::ENTRY_POINT_COST;
use cairo_lang_starknet::contract::{
    find_contracts, get_contracts_info, get_module_functions, ContractInfo,
};
use cairo_lang_starknet::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use cairo_lang_starknet::plugin::StarkNetPlugin;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use colored::Colorize;
use itertools::{chain, Itertools};
use plugin::TestPlugin;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};
use test_config::{try_extract_test_config, TestConfig};

use crate::test_config::{PanicExpectation, TestExpectation};

mod plugin;
mod test_config;

pub struct TestRunner {
    path: String,
    filter: String,
    include_ignored: bool,
    ignored: bool,
    starknet: bool,
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
        path: &str,
        filter: &str,
        include_ignored: bool,
        ignored: bool,
        starknet: bool,
    ) -> Self {
        Self { path: path.into(), filter: filter.into(), include_ignored, ignored, starknet }
    }

    /// Runs the tests and process the results for a summary.
    pub fn run(&self) -> Result<Option<TestsSummary>> {
        let mut plugins = get_default_plugins();
        plugins.push(Arc::new(TestPlugin::default()));
        if self.starknet {
            plugins.push(Arc::new(StarkNetPlugin::default()));
        }
        let db = &mut RootDatabase::builder()
            .with_cfg(CfgSet::from_iter([Cfg::name("test")]))
            .with_plugins(plugins)
            .detect_corelib()
            .build()?;

        let main_crate_ids = setup_project(db, Path::new(&self.path))?;

        if DiagnosticsReporter::stderr().check(db) {
            bail!("failed to compile: {}", self.path);
        }
        let all_entry_points = if self.starknet {
            find_contracts(db, &main_crate_ids)
                .iter()
                .flat_map(|contract| {
                    chain!(
                        get_module_functions(db, contract, EXTERNAL_MODULE).unwrap(),
                        get_module_functions(db, contract, CONSTRUCTOR_MODULE).unwrap(),
                        get_module_functions(db, contract, L1_HANDLER_MODULE).unwrap()
                    )
                })
                .flat_map(|func_id| ConcreteFunctionWithBodyId::from_no_generics_free(db, func_id))
                .collect()
        } else {
            vec![]
        };
        let function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>> =
            all_entry_points
                .iter()
                .map(|func_id| {
                    (
                        db.function_with_body_sierra(*func_id).unwrap().id.clone(),
                        [(CostTokenType::Const, ENTRY_POINT_COST)].into(),
                    )
                })
                .collect();
        let all_tests = find_all_tests(db, main_crate_ids.clone());
        let sierra_program = db
            .get_sierra_program_for_functions(
                chain!(
                    all_entry_points.into_iter(),
                    all_tests.iter().flat_map(|(func_id, _cfg)| {
                        ConcreteFunctionWithBodyId::from_no_generics_free(db, *func_id)
                    })
                )
                .collect(),
            )
            .to_option()
            .with_context(|| "Compilation failed without any diagnostics.")?;
        let replacer = DebugReplacer { db };
        let sierra_program = replacer.apply(&sierra_program);
        let total_tests_count = all_tests.len();
        let named_tests = all_tests
          .into_iter()
          .map(|(func_id, mut test)| {
              // Un-ignoring all the tests in `include-ignored` mode.
              if self.include_ignored {
                  test.ignored = false;
              }
              (
                  format!(
                      "{:?}",
                      FunctionLongId {
                          function: ConcreteFunction {
                              generic_function: GenericFunctionId::Free(func_id),
                              generic_args: vec![]
                          }
                      }
                      .debug(db)
                  ),
                  test,
              )
          })
          .filter(|(name, _)| name.contains(&self.filter))
          // Filtering unignored tests in `ignored` mode.
          .filter(|(_, test)| !self.ignored || test.ignored)
          .collect_vec();
        let filtered_out = total_tests_count - named_tests.len();
        let contracts_info = get_contracts_info(db, main_crate_ids, &replacer)?;
        let TestsSummary { passed, failed, ignored, failed_run_results } =
            run_tests(named_tests, sierra_program, function_set_costs, contracts_info)?;
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
}

/// The status of a ran test.
enum TestStatus {
    Success,
    Fail(RunResultValue),
    Ignore,
}

/// Summary data of the ran tests.
pub struct TestsSummary {
    passed: Vec<String>,
    failed: Vec<String>,
    ignored: Vec<String>,
    failed_run_results: Vec<RunResultValue>,
}

/// Runs the tests and process the results for a summary.
fn run_tests(
    named_tests: Vec<(String, TestConfig)>,
    sierra_program: cairo_lang_sierra::program::Program,
    function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    contracts_info: HashMap<Felt252, ContractInfo>,
) -> anyhow::Result<TestsSummary> {
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
        .map(|(name, test)| -> anyhow::Result<(String, TestStatus)> {
            if test.ignored {
                return Ok((name, TestStatus::Ignore));
            }
            let result = runner
                .run_function(
                    runner.find_function(name.as_str())?,
                    &[],
                    test.available_gas,
                    Default::default(),
                )
                .with_context(|| format!("Failed to run the function `{}`.", name.as_str()))?;
            Ok((
                name,
                match &result.value {
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

/// Finds the tests in the requested crates.
fn find_all_tests(
    db: &dyn SemanticGroup,
    main_crates: Vec<CrateId>,
) -> Vec<(FreeFunctionId, TestConfig)> {
    let mut tests = vec![];
    for crate_id in main_crates {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Ok(module_items) = db.module_items(*module_id) else {
              continue;
          };
            tests.extend(
              module_items.iter().filter_map(|item| {
                  let ModuleItemId::FreeFunction(func_id) = item else { return None };
                  let Ok(attrs) = db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id)) else { return None };
                  Some((*func_id, try_extract_test_config(db.upcast(), attrs).unwrap()?))
              }),
          );
        }
    }
    tests
}
