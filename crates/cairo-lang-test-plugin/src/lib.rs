use std::default::Default;
use std::sync::Arc;

use anyhow::Result;
use cairo_lang_compiler::db::RootDatabase;
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::get_sierra_program_for_functions;
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::debug_info::{Annotations, DebugInfo};
use cairo_lang_sierra::extensions::gas::CostTokenType;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::{ProgramArtifact, StatementIdx};
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::{collect_executables, find_executable_function_ids};
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::DebugReplacer;
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
use starknet_types_core::felt::Felt as Felt252;
pub use test_config::{try_extract_test_config, TestConfig};

mod inline_macros;
pub mod plugin;
pub mod test_config;

const TEST_ATTR: &str = "test";
const SHOULD_PANIC_ATTR: &str = "should_panic";
const IGNORE_ATTR: &str = "ignore";
const AVAILABLE_GAS_ATTR: &str = "available_gas";
const STATIC_GAS_ARG: &str = "static";

/// Configuration for test compilation.
#[derive(Clone)]
pub struct TestsCompilationConfig {
    /// Adds the starknet contracts to the compiled tests.
    pub starknet: bool,

    /// Adds mapping used by [cairo-profiler](https://github.com/software-mansion/cairo-profiler) to
    /// [Annotations] in [DebugInfo] in the compiled tests.
    pub add_statements_functions: bool,
}

/// Runs Cairo compiler.
///
/// # Arguments
/// * `db` - Preloaded compilation database.
/// * `tests_compilation_config` - The compiler configuration for tests compilation.
/// * `main_crate_ids` - [`CrateId`]s to compile. Use `CrateLongId::Real(name).intern(db)` in order
///   to obtain [`CrateId`] from its name.
/// * `test_crate_ids` - [`CrateId`]s to find tests cases in. Must be a subset of `main_crate_ids`.
/// # Returns
/// * `Ok(TestCompilation)` - The compiled test cases with metadata.
/// * `Err(anyhow::Error)` - Compilation failed.
pub fn compile_test_prepared_db(
    db: &RootDatabase,
    tests_compilation_config: TestsCompilationConfig,
    main_crate_ids: Vec<CrateId>,
    test_crate_ids: Vec<CrateId>,
    allow_warnings: bool,
) -> Result<TestCompilation> {
    let all_entry_points = if tests_compilation_config.starknet {
        find_contracts(db, &main_crate_ids)
            .iter()
            .flat_map(|contract| {
                chain!(
                    get_contract_abi_functions(db, contract, EXTERNAL_MODULE).unwrap_or_default(),
                    get_contract_abi_functions(db, contract, CONSTRUCTOR_MODULE)
                        .unwrap_or_default(),
                    get_contract_abi_functions(db, contract, L1_HANDLER_MODULE).unwrap_or_default(),
                )
            })
            .map(|func| ConcreteFunctionWithBodyId::from_semantic(db, func.value))
            .collect()
    } else {
        vec![]
    };

    let executable_functions = find_executable_function_ids(db, main_crate_ids.clone());
    let all_tests = find_all_tests(db, test_crate_ids.clone());

    let func_ids = chain!(
        executable_functions.clone().into_keys(),
        all_entry_points.iter().cloned(),
        // TODO(maciektr): Remove test entrypoints after migration to executable attr.
        all_tests.iter().flat_map(|(func_id, _cfg)| {
            ConcreteFunctionWithBodyId::from_no_generics_free(db, *func_id)
        })
    )
    .collect();

    let mut diag_reporter = DiagnosticsReporter::stderr().with_crates(&main_crate_ids);
    if allow_warnings {
        diag_reporter = diag_reporter.allow_warnings();
    }

    let SierraProgramWithDebug { program: mut sierra_program, debug_info } =
        Arc::unwrap_or_clone(get_sierra_program_for_functions(db, func_ids, diag_reporter)?);

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

    let replacer = DebugReplacer { db };
    replacer.enrich_function_names(&mut sierra_program);

    let (annotations, statements_functions_for_tests) =
        if tests_compilation_config.add_statements_functions {
            (
                Annotations::from(debug_info.statements_locations.extract_statements_functions(db)),
                Some(debug_info.statements_locations.get_statements_functions_map_for_tests(db)),
            )
        } else {
            (Annotations::default(), None)
        };

    let executables = collect_executables(db, executable_functions, &sierra_program);
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
    let sierra_program = ProgramArtifact::stripped(sierra_program).with_debug_info(DebugInfo {
        executables,
        annotations,
        ..DebugInfo::default()
    });

    Ok(TestCompilation {
        sierra_program,
        metadata: TestCompilationMetadata {
            named_tests,
            function_set_costs,
            contracts_info,
            statements_functions: statements_functions_for_tests,
        },
    })
}

/// Encapsulation of all data required to execute tests.
/// This includes the source code compiled to a Sierra program and all cairo-test specific
/// data extracted from it.
/// This can be stored on the filesystem and shared externally.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct TestCompilation {
    pub sierra_program: ProgramArtifact,
    #[serde(flatten)]
    pub metadata: TestCompilationMetadata,
}

/// Encapsulation of all data required to execute tests, except for the Sierra program itself.
/// This includes all cairo-test specific data extracted from the program.
/// This can be stored on the filesystem and shared externally.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct TestCompilationMetadata {
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
    /// A map between sierra statement index and the string representation of the Cairo function
    /// that generated it. The function representation is composed of the function name and the
    /// path (modules and impls) to the function in the file. Used only if the tests are running
    /// with profiling.
    // TODO(Gil): consider serializing this field once it is stable.
    #[serde(skip)]
    pub statements_functions: Option<UnorderedHashMap<StatementIdx, String>>,
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

/// The suite of plugins that implements assert macros for tests.
pub fn test_assert_suite() -> PluginSuite {
    let mut suite = PluginSuite::default();
    suite
        .add_inline_macro_plugin::<inline_macros::assert::AssertEqMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertNeMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertLtMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertLeMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertGtMacro>()
        .add_inline_macro_plugin::<inline_macros::assert::AssertGeMacro>();
    suite
}

/// The suite of plugins for compilation for testing.
pub fn test_plugin_suite() -> PluginSuite {
    let mut suite = PluginSuite::default();
    suite.add_plugin::<TestPlugin>().add(test_assert_suite());
    suite
}
