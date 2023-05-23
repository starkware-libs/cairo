use std::fs;
use std::path::Path;

use crate::casm_generator::{SierraCasmGenerator, TestConfig as TestConfigInternal};
use crate::find_all_tests;
use crate::plugin::TestPlugin;
use anyhow::{Context, Result};
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::project::setup_project;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::extensions::enm::EnumType;
use cairo_lang_sierra::extensions::NamedType;
use cairo_lang_sierra::program::{GenericArg, Program};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::{DebugReplacer, SierraIdReplacer};
use cairo_lang_starknet::contract::{find_contracts, get_module_functions};
use cairo_lang_starknet::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use cairo_lang_starknet::plugin::StarkNetPlugin;
use itertools::{chain, Itertools};
use std::sync::Arc;

pub fn collect_tests(
    input_path: &String,
    output_path: Option<&String>,
    maybe_cairo_paths: Option<Vec<&String>>,
    maybe_builtins: Option<Vec<&String>>,
) -> Result<(Option<String>, Vec<TestConfigInternal>)> {
    let db = &mut {
        let mut b = RootDatabase::builder();
        b.detect_corelib();
        b.with_cfg(CfgSet::from_iter([Cfg::name("test")]));
        b.with_semantic_plugin(Arc::new(TestPlugin::default()));
        b.with_semantic_plugin(Arc::new(StarkNetPlugin::default()));
        b.build()?
    };

    let main_crate_ids = setup_project(db, Path::new(&input_path))?;

    if let Some(cairo_paths) = maybe_cairo_paths {
        for cairo_path in cairo_paths {
            setup_project(db, Path::new(cairo_path))
                .with_context(|| format!("Failed to add linked library ({})", input_path))?;
        }
    }

    let all_entry_points: Vec<ConcreteFunctionWithBodyId> = find_contracts(db, &main_crate_ids)
        .iter()
        .flat_map(|contract| {
            chain!(
                get_module_functions(db, contract, EXTERNAL_MODULE).unwrap(),
                get_module_functions(db, contract, CONSTRUCTOR_MODULE).unwrap(),
                get_module_functions(db, contract, L1_HANDLER_MODULE).unwrap()
            )
        })
        .flat_map(|func_id| ConcreteFunctionWithBodyId::from_no_generics_free(db, func_id))
        .collect();

    // let function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>> =
    //     all_entry_points
    //         .iter()
    //         .map(|func_id| {
    //             (
    //                 db.function_with_body_sierra(*func_id).unwrap().id.clone(),
    //                 [(CostTokenType::Const, ENTRY_POINT_COST)].into(),
    //             )
    //         })
    //         .collect();

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

    let collected_tests = all_tests
        .into_iter()
        .map(|(func_id, test)| {
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
        .collect_vec()
        .into_iter()
        .map(|(test_name, config)| TestConfigInternal {
            name: test_name,
            available_gas: config.available_gas,
        })
        .collect();

    // // code taken from crates/cairo-lang-test-runner/src/cli.rs
    // let plugins: Vec<Arc<dyn SemanticPlugin>> = vec![
    //     Arc::new(DerivePlugin {}),
    //     Arc::new(PanicablePlugin {}),
    //     Arc::new(ConfigPlugin { configs: HashSet::from(["test".to_string()]) }),
    //     Arc::new(StarkNetPlugin {}),
    // ];
    // let db = &mut RootDatabase::builder()
    //     .with_plugins(plugins)
    //     .detect_corelib()
    //     .build()
    //     .context("Failed to build database")?;

    // let main_crate_ids = setup_project(db, Path::new(&input_path))
    //     .with_context(|| format!("Failed to setup project for path({})", input_path))?;

    // if let Some(cairo_paths) = maybe_cairo_paths {
    //     for cairo_path in cairo_paths {
    //         setup_project(db, Path::new(cairo_path))
    //             .with_context(|| format!("Failed to add linked library ({})", input_path))?;
    //     }
    // }

    // if DiagnosticsReporter::stderr().check(db) {
    //     return Err(anyhow!(
    //         "Failed to add linked library, for a detailed information, please go through the logs \
    //          above"
    //     ));
    // }
    // let all_tests = find_all_tests(db, main_crate_ids);

    // let sierra_program = db
    //     .get_sierra_program_for_functions(
    //         all_tests
    //             .iter()
    //             .flat_map(|(func_id, _cfg)| {
    //                 ConcreteFunctionWithBodyId::from_no_generics_free(db, *func_id)
    //             })
    //             .collect(),
    //     )
    //     .to_option()
    //     .context("Compilation failed without any diagnostics")
    //     .context("Failed to get sierra program")?;

    // let collected_tests: Vec<TestConfigInternal> = all_tests
    //     .into_iter()
    //     .map(|(func_id, test)| {
    //         (
    //             format!(
    //                 "{:?}",
    //                 FunctionLongId {
    //                     function: ConcreteFunction {
    //                         generic_function: GenericFunctionId::Free(func_id),
    //                         generic_args: vec![]
    //                     }
    //                 }
    //                 .debug(db)
    //             ),
    //             test,
    //         )
    //     })
    //     .collect_vec()
    //     .into_iter()
    //     .map(|(test_name, config)| TestConfigInternal {
    //         name: test_name,
    //         available_gas: config.available_gas,
    //     })
    //     .collect();

    // let sierra_program = replace_sierra_ids_in_program(db, &sierra_program);

    let mut builtins = vec![];
    if let Some(unwrapped_builtins) = maybe_builtins {
        builtins = unwrapped_builtins.iter().map(|s| s.to_string()).collect();
    }

    validate_tests(sierra_program.clone(), &collected_tests, builtins)
        .context("Test validation failed")?;

    let mut result_contents = None;
    if let Some(path) = output_path {
        fs::write(path, &sierra_program.to_string()).context("Failed to write output")?;
    } else {
        result_contents = Some(sierra_program.to_string());
    }
    Ok((result_contents, collected_tests))
}

fn validate_tests(
    sierra_program: Program,
    collected_tests: &Vec<TestConfigInternal>,
    ignored_params: Vec<String>,
) -> Result<(), anyhow::Error> {
    let casm_generator = match SierraCasmGenerator::new(sierra_program) {
        Ok(casm_generator) => casm_generator,
        Err(e) => panic!("{}", e),
    };
    for test in collected_tests {
        let func = casm_generator.find_function(&test.name)?;
        let mut filtered_params: Vec<String> = Vec::new();
        for param in &func.params {
            let param_str = &param.ty.debug_name.as_ref().unwrap().to_string();
            if !ignored_params.contains(&param_str) {
                filtered_params.push(param_str.to_string());
            }
        }
        if !filtered_params.is_empty() {
            anyhow::bail!(format!(
                "Invalid number of parameters for test {}: expected 0, got {}",
                test.name,
                func.params.len()
            ));
        }
        let signature = &func.signature;
        let ret_types = &signature.ret_types;
        let tp = &ret_types[ret_types.len() - 1];
        let info = casm_generator.get_info(&tp);
        let mut maybe_return_type_name = None;
        if info.long_id.generic_id == EnumType::ID {
            if let GenericArg::UserType(ut) = &info.long_id.generic_args[0] {
                if let Some(name) = ut.debug_name.as_ref() {
                    maybe_return_type_name = Some(name.as_str());
                }
            }
        }
        if let Some(return_type_name) = maybe_return_type_name {
            if !return_type_name.starts_with("core::PanicResult::") {
                anyhow::bail!("Test function {} must be panicable but it's not", test.name);
            }
            if return_type_name != "core::PanicResult::<((),)>" {
                anyhow::bail!(
                    "Test function {} returns a value {}, it is required that test functions do \
                     not return values",
                    test.name,
                    return_type_name
                );
            }
        } else {
            anyhow::bail!(
                "Couldn't read result type for test function {} possible cause: Test function {} \
                 must be panicable but it's not",
                test.name,
                test.name
            );
        }
    }

    Ok(())
}
