use std::sync::Arc;

use anyhow::{Context, Result};
use cairo_felt::Felt252;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{Program, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::{DebugReplacer, SierraIdReplacer};
use cairo_lang_starknet::contract::{
    find_contracts, get_contract_abi_functions, get_contracts_info, ContractInfo,
};
use cairo_lang_starknet::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use cairo_lang_starknet_classes::casm_contract_class::ENTRY_POINT_COST;
use cairo_lang_utils::ordered_hash_map::{
    deserialize_ordered_hashmap_vec, serialize_ordered_hashmap_vec, OrderedHashMap,
};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::{chain, Itertools};
pub use plugin::TestPlugin;
use serde::{Deserialize, Serialize};
pub use test_config::{try_extract_test_config, TestConfig};

mod inline_macros;
pub mod plugin;
pub mod test_config;

const TEST_ATTR: &str = "test";
const SHOULD_PANIC_ATTR: &str = "should_panic";
const IGNORE_ATTR: &str = "ignore";
const AVAILABLE_GAS_ATTR: &str = "available_gas";
const STATIC_GAS_ARG: &str = "static";

/// Runs Cairo compiler.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `starknet` - Add the starknet contracts to the compiled tests.
/// * `main_crate_ids` - [`CrateId`]s to compile. Use `db.intern_crate(CrateLongId::Real(name))` in
///   order to obtain [`CrateId`] from its name.
/// * `test_crate_ids` - [`CrateId`]s to find tests cases in. Must be a subset of `main_crate_ids`.
/// # Returns
/// * `Ok(TestCompilation)` - The compiled test cases with metadata.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_test_prepared_db(
    db: &RootDatabase,
    starknet: bool,
    main_crate_ids: Vec<CrateId>,
    test_crate_ids: Vec<CrateId>,
) -> Result<TestCompilation> {
    let all_entry_points = if starknet {
        find_contracts(db, &main_crate_ids)
            .iter()
            .flat_map(|contract| {
                chain!(
                    get_contract_abi_functions(db, contract, EXTERNAL_MODULE).unwrap(),
                    get_contract_abi_functions(db, contract, CONSTRUCTOR_MODULE).unwrap(),
                    get_contract_abi_functions(db, contract, L1_HANDLER_MODULE).unwrap(),
                )
            })
            .map(|func| ConcreteFunctionWithBodyId::from_semantic(db, func.value))
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
    let all_tests = find_all_tests(db, test_crate_ids.clone());
    let SierraProgramWithDebug { program: sierra_program, debug_info } = Arc::unwrap_or_clone(
        db.get_sierra_program_for_functions(
            chain!(
                all_entry_points.into_iter(),
                all_tests.iter().flat_map(|(func_id, _cfg)| {
                    ConcreteFunctionWithBodyId::from_no_generics_free(db, *func_id)
                })
            )
            .collect(),
        )
        .to_option()
        .with_context(|| "Compilation failed without any diagnostics.")?,
    );
    let replacer = DebugReplacer { db };
    let sierra_program = replacer.apply(&sierra_program);
    let statements_functions =
        debug_info.statements_locations.get_statements_functions_map_for_tests(db);

    let named_tests = all_tests
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
        .collect_vec();
    let contracts_info = get_contracts_info(db, main_crate_ids.clone(), &replacer)?;

    Ok(TestCompilation {
        named_tests,
        sierra_program,
        function_set_costs,
        contracts_info,
        statements_functions,
    })
}

/// Compiled test cases.
#[derive(Clone, Serialize, Deserialize)]
pub struct TestCompilation {
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
    pub contracts_info: OrderedHashMap<Felt252, ContractInfo>,
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
    pub function_set_costs: OrderedHashMap<FunctionId, OrderedHashMap<CostTokenType, i32>>,
    pub named_tests: Vec<(String, TestConfig)>,
    pub sierra_program: Program,
    /// A map between sierra statement index and the string representation of the Cairo function
    /// that generated it. The function representation is composed of the function name and the
    /// path (modules and impls) to the function in the file. Used only if the tests are running
    /// with profiling.
    // TODO(Gil): consider serializing this field once it is stable.
    #[serde(skip)]
    pub statements_functions: UnorderedHashMap<StatementIdx, String>,
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
            tests.extend(module_items.iter().filter_map(|item| {
                let ModuleItemId::FreeFunction(func_id) = item else { return None };
                let Ok(attrs) =
                    db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id))
                else {
                    return None;
                };
                Some((*func_id, try_extract_test_config(db.upcast(), attrs).unwrap()?))
            }));
        }
    }
    tests
}

/// The suite of plugins for compilation for testing.
pub fn test_plugin_suite() -> PluginSuite {
    let mut suite = PluginSuite::default();
    suite
        .add_plugin::<TestPlugin>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertEqMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertNeMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertLtMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertLeMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertGtMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertGeMacro>();
    suite
}
