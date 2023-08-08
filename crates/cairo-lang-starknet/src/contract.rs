use anyhow::{bail, Context};
use cairo_felt::Felt252;
use cairo_lang_defs::ids::{
    FreeFunctionId, LanguageElementId, ModuleId, ModuleItemId, SubmoduleId,
};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_lowering::ids::{ConcreteFunctionWithBodyId, FunctionWithBodyLongId};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::us::SemanticUseEx;
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_semantic::resolve::ResolvedGenericItem;
use cairo_lang_semantic::Expr;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::SierraIdReplacer;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{extract_matches, try_extract_matches};
use num_bigint::BigUint;
use sha3::{Digest, Keccak256};

use crate::contract_class::{extract_semantic_entrypoints, SemanticEntryPoints};
use crate::plugin::aux_data::StarkNetContractAuxData;
use crate::plugin::consts::WRAPPER_PREFIX;

#[cfg(test)]
#[path = "contract_test.rs"]
mod test;

/// Represents a declaration of a contract.
pub struct ContractDeclaration {
    /// The id of the module that defines the contract.
    pub submodule_id: SubmoduleId,
}

impl ContractDeclaration {
    pub fn module_id(&self) -> ModuleId {
        ModuleId::Submodule(self.submodule_id)
    }
}

/// A variant of eth-keccak that computes a value that fits in a Starknet field element.
pub fn starknet_keccak(data: &[u8]) -> BigUint {
    let mut hasher = Keccak256::new();
    hasher.update(data);
    let mut result = hasher.finalize();

    // Truncate result to 250 bits.
    *result.first_mut().unwrap() &= 3;
    BigUint::from_bytes_be(&result)
}

/// Finds the inline modules annotated as contracts in the given crate_ids and
/// returns the corresponding ContractDeclarations.
pub fn find_contracts(db: &dyn SemanticGroup, crate_ids: &[CrateId]) -> Vec<ContractDeclaration> {
    let mut contracts = vec![];
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            let generated_file_infos =
                db.module_generated_file_infos(*module_id).unwrap_or_default();

            // When a module is generated by a plugin the same generated_file_info appears in two
            // places:
            //   1. db.module_generated_file_infos(*original_module_id)?[k] (with k > 0).
            //   2. db.module_generated_file_infos(*generated_module_id)?[0].
            // We are interested in modules that the plugin acted on and not modules that were
            // created by the plugin, so we skip generated_file_infos[0].
            // For example if we have
            // mod a {
            //    #[starknet::contract]
            //    mod b {
            //    }
            // }
            // Then we want lookup b inside a and not inside b.

            for generated_file_info in generated_file_infos.iter().skip(1) {
                let Some(generated_file_info) = generated_file_info else {
                    continue;
                };
                let Some(mapper) =
                    generated_file_info.aux_data.0.as_any().downcast_ref::<DynPluginAuxData>()
                else {
                    continue;
                };
                let Some(aux_data) = mapper.0.as_any().downcast_ref::<StarkNetContractAuxData>()
                else {
                    continue;
                };

                for contract_name in &aux_data.contracts {
                    if let ModuleId::Submodule(submodule_id) = *module_id {
                        contracts.push(ContractDeclaration { submodule_id });
                    } else {
                        panic!("Contract `{contract_name}` was not found.");
                    }
                }
            }
        }
    }
    contracts
}

/// Returns the list of functions in a given module.
pub fn get_module_functions(
    db: &dyn SemanticGroup,
    contract: &ContractDeclaration,
    module_name: &str,
) -> anyhow::Result<Vec<FreeFunctionId>> {
    let generated_module_id = get_generated_contract_module(db, contract)?;
    let module_id = match db
        .module_item_by_name(generated_module_id, module_name.into())
        .to_option()
        .with_context(|| "Failed to initiate a lookup in the {module_name} module.")?
    {
        Some(ModuleItemId::Submodule(external_module_id)) => {
            ModuleId::Submodule(external_module_id)
        }
        _ => anyhow::bail!("Failed to get the external module."),
    };
    let mut functions = vec![];
    for use_id in db
        .module_uses_ids(module_id)
        .to_option()
        .with_context(|| "Failed to get external module functions.")?
        .iter()
    {
        let ResolvedGenericItem::GenericFunction(function) = db
            .use_resolved_item(*use_id)
            .to_option()
            .with_context(|| "Failed to fetch used function.")?
        else {
            bail!("Fetched item not a function.")
        };
        match function {
            GenericFunctionId::Free(function) => functions.push(function),
            _ => bail!("Expected a free function."),
        }
    }
    Ok(functions)
}

/// Returns the generated contract module.
fn get_generated_contract_module(
    db: &dyn SemanticGroup,
    contract: &ContractDeclaration,
) -> anyhow::Result<ModuleId> {
    let parent_module_id = contract.submodule_id.parent_module(db.upcast());
    let contract_name = contract.submodule_id.name(db.upcast());

    match db
        .module_item_by_name(parent_module_id, contract_name.clone())
        .to_option()
        .with_context(|| "Failed to initiate a lookup in the root module.")?
    {
        Some(ModuleItemId::Submodule(generated_module_id)) => {
            Ok(ModuleId::Submodule(generated_module_id))
        }
        _ => anyhow::bail!(format!("Failed to get generated module {contract_name}.")),
    }
}

/// Sierra informaton of a contract.
pub struct ContractInfo {
    /// Sierra function of the constructor.
    pub constructor: Option<FunctionId>,
    /// Sierra functions of the external functions.
    pub externals: OrderedHashMap<Felt252, FunctionId>,
    /// Sierra functions of the l1 handler functions.
    pub l1_handlers: OrderedHashMap<Felt252, FunctionId>,
}

/// Returns the list of functions in a given module.
pub fn get_contracts_info<T: SierraIdReplacer>(
    db: &dyn SierraGenGroup,
    main_crate_ids: Vec<CrateId>,
    replacer: &T,
) -> Result<OrderedHashMap<Felt252, ContractInfo>, anyhow::Error> {
    let contracts = find_contracts(db.upcast(), &main_crate_ids);
    let mut contracts_info = OrderedHashMap::default();
    for contract in contracts {
        let (class_hash, contract_info) = analyze_contract(db, &contract, replacer)?;
        contracts_info.insert(class_hash, contract_info);
    }
    Ok(contracts_info)
}

/// Analyzes a contract and returns its class hash and a list of its functions.
fn analyze_contract<T: SierraIdReplacer>(
    db: &dyn SierraGenGroup,
    contract: &ContractDeclaration,
    replacer: &T,
) -> anyhow::Result<(cairo_felt::Felt252, ContractInfo)> {
    // Extract class hash.
    let item =
        db.module_item_by_name(contract.module_id(), "TEST_CLASS_HASH".into()).unwrap().unwrap();
    let constant_id = extract_matches!(item, ModuleItemId::Constant);
    let value =
        extract_matches!(db.constant_semantic_data(constant_id).unwrap().value, Expr::Literal)
            .value;
    let class_hash = Felt252::try_from(value).unwrap();

    // Extract functions.
    let SemanticEntryPoints { external, l1_handler, constructor } =
        extract_semantic_entrypoints(db, contract)?;
    let externals =
        external.into_iter().map(|f| get_selector_and_sierra_function(db, f, replacer)).collect();
    let l1_handlers =
        l1_handler.into_iter().map(|f| get_selector_and_sierra_function(db, f, replacer)).collect();
    let constructors: Vec<_> = constructor
        .into_iter()
        .map(|f| get_selector_and_sierra_function(db, f, replacer))
        .collect();

    let contract_info = ContractInfo {
        externals,
        l1_handlers,
        constructor: constructors.into_iter().next().map(|x| x.1),
    };
    Ok((class_hash, contract_info))
}

/// Converts a function to a Sierra function.
/// Returns the selector and the sierra function id.
pub fn get_selector_and_sierra_function<T: SierraIdReplacer>(
    db: &dyn SierraGenGroup,
    function_with_body: ConcreteFunctionWithBodyId,
    replacer: &T,
) -> (Felt252, FunctionId) {
    let function_id = function_with_body.function_id(db.upcast()).expect("Function error.");
    let sierra_id = replacer.replace_function_id(&db.intern_sierra_function(function_id));
    let semantic = try_extract_matches!(
        db.lookup_intern_lowering_function_with_body(
            function_with_body.function_with_body_id(db.upcast())
        ),
        FunctionWithBodyLongId::Semantic
    )
    .expect("Entrypoint cannot be a generated function.");
    let name = semantic.name(db.upcast());
    let used_name = name
        .strip_prefix(WRAPPER_PREFIX)
        .unwrap_or_else(|| panic!("Wrapper with unexpected prefix: `{name}`."));
    let selector = Felt252::try_from(starknet_keccak(used_name.as_bytes())).unwrap();
    (selector, sierra_id)
}
