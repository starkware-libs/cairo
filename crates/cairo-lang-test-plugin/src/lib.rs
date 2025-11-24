use anyhow::{Result, ensure};
use cairo_lang_compiler::diagnostics::DiagnosticsReporter;
use cairo_lang_compiler::{ensure_diagnostics, get_sierra_program_for_functions};
use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{FreeFunctionId, FunctionWithBodyId, ModuleItemId};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{CrateId, CrateInput};
use cairo_lang_lowering::ids::ConcreteFunctionWithBodyId;
use cairo_lang_semantic::items::function_with_body::FunctionWithBodySemantic;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::plugin::PluginSuite;
use cairo_lang_semantic::{ConcreteFunction, FunctionLongId};
use cairo_lang_sierra::debug_info::{Annotations, DebugInfo};
use cairo_lang_sierra::extensions::gas::{CostTokenMap, CostTokenType};
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra::program::ProgramArtifact;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::executables::{collect_executables, find_executable_function_ids};
use cairo_lang_sierra_generator::program_generator::SierraProgramWithDebug;
use cairo_lang_sierra_generator::replace_ids::DebugReplacer;
use cairo_lang_sierra_generator::statements_locations::StatementsLocations;
use cairo_lang_starknet::contract::{
    ContractDeclaration, ContractInfo, find_contracts, get_contract_abi_functions,
    get_contracts_info,
};
use cairo_lang_starknet::plugin::consts::{CONSTRUCTOR_MODULE, EXTERNAL_MODULE, L1_HANDLER_MODULE};
use cairo_lang_starknet_classes::casm_contract_class::ENTRY_POINT_COST;
use cairo_lang_utils::ordered_hash_map::{
    OrderedHashMap, deserialize_ordered_hashmap_vec, serialize_ordered_hashmap_vec,
};
use itertools::{Itertools, chain};
pub use plugin::TestPlugin;
use salsa::Database;
use serde::{Deserialize, Serialize};
use starknet_types_core::felt::Felt as Felt252;
pub use test_config::{TestConfig, try_extract_test_config};

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
pub struct TestsCompilationConfig<'db> {
    /// Adds the Starknet contracts to the compiled tests.
    pub starknet: bool,

    /// Contracts to compile.
    /// If defined, only these contracts will be available in tests.
    /// If not, all contracts from `contract_crate_ids` will be compiled.
    pub contract_declarations: Option<Vec<ContractDeclaration<'db>>>,

    /// Crates to be searched for contracts.
    /// If not defined, all crates will be searched.
    pub contract_crate_ids: Option<&'db [CrateId<'db>]>,

    /// Crates to be searched for executable attributes.
    /// If not defined, test crates will be searched.
    pub executable_crate_ids: Option<Vec<CrateId<'db>>>,

    /// Adds a mapping used by [cairo-profiler](https://github.com/software-mansion/cairo-profiler) to
    /// [Annotations] in [DebugInfo] in the compiled tests.
    pub add_statements_functions: bool,

    /// Adds a mapping used by [cairo-coverage](https://github.com/software-mansion/cairo-coverage) to
    /// [Annotations] in [DebugInfo] in the compiled tests.
    pub add_statements_code_locations: bool,
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
pub fn compile_test_prepared_db<'db>(
    db: &'db dyn Database,
    tests_compilation_config: TestsCompilationConfig<'db>,
    test_crate_ids: Vec<CrateInput>,
    mut diagnostics_reporter: DiagnosticsReporter<'_>,
) -> Result<TestCompilation<'db>> {
    ensure!(
        tests_compilation_config.starknet
            || tests_compilation_config.contract_declarations.is_none(),
        "Contract declarations can be provided only when starknet is enabled."
    );
    ensure!(
        tests_compilation_config.starknet || tests_compilation_config.contract_crate_ids.is_none(),
        "Contract crate ids can be provided only when starknet is enabled."
    );

    ensure_diagnostics(db, &mut diagnostics_reporter)?;

    let contracts = tests_compilation_config.contract_declarations.unwrap_or_else(|| {
        find_contracts(
            db,
            tests_compilation_config.contract_crate_ids.unwrap_or_else(|| db.crates()),
        )
    });
    let all_entry_points = if tests_compilation_config.starknet {
        contracts
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

    let test_crate_ids = CrateInput::into_crate_ids(db, test_crate_ids);
    let executable_functions = find_executable_function_ids(
        db,
        tests_compilation_config.executable_crate_ids.unwrap_or_else(|| test_crate_ids.clone()),
    );
    let all_tests = find_all_tests(db, test_crate_ids);

    let func_ids = chain!(
        executable_functions.keys().cloned(),
        all_entry_points.iter().cloned(),
        // TODO(maciektr): Remove test entrypoints after migration to executable attr.
        all_tests.iter().flat_map(|(func_id, _cfg)| {
            ConcreteFunctionWithBodyId::from_no_generics_free(db, *func_id)
        })
    )
    .collect();

    let SierraProgramWithDebug { program: sierra_program, debug_info } =
        get_sierra_program_for_functions(db, func_ids)?;

    let function_set_costs: OrderedHashMap<FunctionId, CostTokenMap<i32>> = all_entry_points
        .iter()
        .map(|func_id| {
            (
                db.function_with_body_sierra(*func_id).unwrap().id.clone(),
                CostTokenMap::from_iter([(CostTokenType::Const, ENTRY_POINT_COST)]),
            )
        })
        .collect();

    let replacer = DebugReplacer { db };
    let mut sierra_program = sierra_program.clone();
    replacer.enrich_function_names(&mut sierra_program);

    let mut annotations = Annotations::default();
    if tests_compilation_config.add_statements_functions {
        annotations.extend(Annotations::from(
            debug_info.statements_locations.extract_statements_functions(db),
        ))
    }
    if tests_compilation_config.add_statements_code_locations {
        annotations.extend(Annotations::from(
            debug_info.statements_locations.extract_statements_source_code_locations(db),
        ))
    }

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
    let contracts_info = get_contracts_info(db, contracts, &replacer)?;
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
            statements_locations: Some(debug_info.statements_locations.clone()),
        },
    })
}

/// Encapsulation of all data required to execute tests.
///
/// This includes the source code compiled to a Sierra program and all cairo-test-specific
/// data extracted from it.
/// This can be stored on the filesystem and shared externally.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct TestCompilation<'db> {
    pub sierra_program: ProgramArtifact,
    #[serde(flatten)]
    pub metadata: TestCompilationMetadata<'db>,
}

/// Encapsulation of all data required to execute tests, except for the Sierra program itself.
///
/// This includes all cairo-test-specific data extracted from the program.
/// This can be stored on the filesystem and shared externally.
#[derive(Clone, Serialize, Deserialize, Debug, PartialEq)]
pub struct TestCompilationMetadata<'db> {
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
    pub contracts_info: OrderedHashMap<Felt252, ContractInfo>,
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
    pub function_set_costs: OrderedHashMap<FunctionId, CostTokenMap<i32>>,
    pub named_tests: Vec<(String, TestConfig)>,
    /// Optional `StatementsLocations` for the compiled tests.
    /// See [StatementsLocations] for more information.
    // TODO(Gil): consider serializing this field once it is stable.
    #[serde(skip)]
    pub statements_locations: Option<StatementsLocations<'db>>,
}

/// Finds the tests in the requested crates.
fn find_all_tests<'db>(
    db: &'db dyn Database,
    main_crates: Vec<CrateId<'db>>,
) -> Vec<(FreeFunctionId<'db>, TestConfig)> {
    let mut tests = vec![];
    for crate_id in main_crates {
        let modules = db.crate_modules(crate_id);
        for module_id in modules.iter() {
            let Ok(module_data) = module_id.module_data(db) else {
                continue;
            };
            tests.extend(module_data.items(db).iter().filter_map(|item| {
                let ModuleItemId::FreeFunction(func_id) = item else { return None };
                let Ok(attrs) =
                    db.function_with_body_attributes(FunctionWithBodyId::Free(*func_id))
                else {
                    return None;
                };
                Some((*func_id, try_extract_test_config(db, attrs).ok()??))
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
