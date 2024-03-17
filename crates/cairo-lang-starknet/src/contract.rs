use anyhow::{bail, Context};
use cairo_felt::Felt252;
use cairo_lang_defs::ids::{
    FileIndex, FreeFunctionId, LanguageElementId, LookupItemId, ModuleFileId, ModuleId,
    ModuleItemId, NamedLanguageElementId, SubmoduleId,
};
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::CrateId;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::diagnostic::{NotFoundItemType, SemanticDiagnostics};
use cairo_lang_semantic::expr::inference::canonic::ResultNoErrEx;
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::functions::{
    ConcreteFunctionWithBodyId as SemanticConcreteFunctionWithBodyId, GenericFunctionId,
};
use cairo_lang_semantic::items::us::SemanticUseEx;
use cairo_lang_semantic::resolve::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use cairo_lang_semantic::substitution::SemanticRewriter;
use cairo_lang_semantic::Expr;
use cairo_lang_sierra::ids::FunctionId;
use cairo_lang_sierra_generator::db::SierraGenGroup;
use cairo_lang_sierra_generator::replace_ids::SierraIdReplacer;
use cairo_lang_starknet_classes::keccak::starknet_keccak;
use cairo_lang_syntax::node::helpers::{GetIdentifier, PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::{
    deserialize_ordered_hashmap_vec, serialize_ordered_hashmap_vec, OrderedHashMap,
};
use itertools::chain;
use serde::{Deserialize, Serialize};
use {cairo_lang_lowering as lowering, cairo_lang_semantic as semantic};

use crate::aliased::Aliased;
use crate::compile::{extract_semantic_entrypoints, SemanticEntryPoints};
use crate::plugin::aux_data::StarkNetContractAuxData;
use crate::plugin::consts::{ABI_ATTR, ABI_ATTR_EMBED_V0_ARG};

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

/// Returns the contract declaration of a given module if it is a contract module.
pub fn module_contract(db: &dyn SemanticGroup, module_id: ModuleId) -> Option<ContractDeclaration> {
    let generated_file_infos = db.module_generated_file_infos(module_id).unwrap_or_default();

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
    generated_file_infos.iter().skip(1).find_map(|generated_file_info| {
        let StarkNetContractAuxData { contract_name } =
            generated_file_info.as_ref()?.aux_data.as_ref()?.as_any().downcast_ref()?;
        if let ModuleId::Submodule(submodule_id) = module_id {
            Some(ContractDeclaration { submodule_id })
        } else {
            unreachable!("Contract `{contract_name}` was not found.");
        }
    })
}

/// Finds the inline modules annotated as contracts in the given crate_ids and
/// returns the corresponding ContractDeclarations.
pub fn find_contracts(db: &dyn SemanticGroup, crate_ids: &[CrateId]) -> Vec<ContractDeclaration> {
    let mut contract_declarations = vec![];
    for crate_id in crate_ids {
        let modules = db.crate_modules(*crate_id);
        for module_id in modules.iter() {
            contract_declarations.extend(module_contract(db, *module_id));
        }
    }
    contract_declarations
}

/// Returns the ABI functions of a given contract.
/// Assumes the given module is a contract module.
pub fn get_contract_abi_functions(
    db: &dyn SemanticGroup,
    contract: &ContractDeclaration,
    module_name: &str,
) -> anyhow::Result<Vec<Aliased<semantic::ConcreteFunctionWithBodyId>>> {
    Ok(chain!(
        get_contract_internal_module_abi_functions(db, contract, module_name)?,
        get_impl_aliases_abi_functions(db, contract, module_name)?
    )
    .collect())
}

/// Returns the ABI functions in a given internal module in the contract.
fn get_contract_internal_module_abi_functions(
    db: &dyn SemanticGroup,
    contract: &ContractDeclaration,
    module_name: &str,
) -> anyhow::Result<Vec<Aliased<SemanticConcreteFunctionWithBodyId>>> {
    let generated_module_id = get_generated_contract_module(db, contract)?;
    let module_id = get_submodule_id(db.upcast(), generated_module_id, module_name)?;
    get_module_aliased_functions(db, module_id)?
        .into_iter()
        .map(|f| f.try_map(|f| semantic::ConcreteFunctionWithBodyId::from_no_generics_free(db, f)))
        .collect::<Option<Vec<_>>>()
        .with_context(|| "Generics are not allowed in wrapper functions")
}

/// Returns the list of functions in a given module with their aliases.
/// Assumes the given module is a generated module containing `use` items pointing to wrapper ABI
/// functions.
fn get_module_aliased_functions(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
) -> anyhow::Result<Vec<Aliased<FreeFunctionId>>> {
    db.module_uses(module_id)
        .to_option()
        .with_context(|| "Failed to get external module uses.")?
        .iter()
        .map(|(use_id, leaf)| {
            if let ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(function_id)) = db
                .use_resolved_item(*use_id)
                .to_option()
                .with_context(|| "Failed to fetch used function.")?
            {
                Ok(Aliased {
                    value: function_id,
                    alias: leaf.stable_ptr().identifier(db.upcast()).to_string(),
                })
            } else {
                bail!("Expected a free function.")
            }
        })
        .collect::<Result<Vec<_>, _>>()
}

/// Returns the abi functions of the impl aliases embedded in the given contract.
/// `module_prefix` is the prefix of the generated module name outside of the contract, the rest of
/// the name is defined by the name of the aliased impl.
fn get_impl_aliases_abi_functions(
    db: &dyn SemanticGroup,
    contract: &ContractDeclaration,
    module_prefix: &str,
) -> anyhow::Result<Vec<Aliased<SemanticConcreteFunctionWithBodyId>>> {
    let syntax_db = db.upcast();
    let generated_module_id = get_generated_contract_module(db, contract)?;
    let module_file_id = ModuleFileId(generated_module_id, FileIndex(0));
    let mut diagnostics = SemanticDiagnostics::new(module_file_id.file_id(db.upcast()).unwrap());
    let mut all_abi_functions = vec![];
    for (impl_alias_id, impl_alias) in db
        .module_impl_aliases(generated_module_id)
        .to_option()
        .with_context(|| "Failed to get external module impl aliases.")?
        .iter()
    {
        if !impl_alias.has_attr_with_arg(db.upcast(), ABI_ATTR, ABI_ATTR_EMBED_V0_ARG) {
            continue;
        }
        let resolver_data = db
            .impl_alias_resolver_data(*impl_alias_id)
            .to_option()
            .with_context(|| "Internal error: Failed to get impl alias resolver data.")?;
        let mut resolver = Resolver::with_data(
            db,
            resolver_data.clone_with_inference_id(
                db,
                InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
                    ModuleItemId::ImplAlias(*impl_alias_id),
                )),
            ),
        );

        let impl_path_elements = impl_alias.impl_path(syntax_db).elements(syntax_db);
        let Some((impl_final_part, impl_module)) = impl_path_elements.split_last() else {
            unreachable!("impl_path should have at least one segment")
        };
        let impl_name = impl_final_part.identifier(syntax_db);
        let generic_args = impl_final_part.generic_args(syntax_db).unwrap_or_default();
        let ResolvedConcreteItem::Module(impl_module) = resolver
            .resolve_concrete_path(
                &mut diagnostics,
                impl_module.to_vec(),
                NotFoundItemType::Identifier,
            )
            .to_option()
            .with_context(|| "Internal error: Failed to resolve impl module.")?
        else {
            bail!("Internal error: Impl alias pointed to an object with non module parent.");
        };
        let module_id = get_submodule_id(db, impl_module, &format!("{module_prefix}_{impl_name}"))?;
        for abi_function in get_module_aliased_functions(db, module_id)? {
            all_abi_functions.extend(abi_function.try_map(|f| {
                let concrete_wrapper = resolver
                    .specialize_function(
                        &mut diagnostics,
                        impl_alias.stable_ptr().untyped(),
                        GenericFunctionId::Free(f),
                        generic_args.clone(),
                    )
                    .to_option()?
                    .get_concrete(db)
                    .body(db)
                    .to_option()??;
                let inference = &mut resolver.inference();
                assert_eq!(
                    inference.finalize(),
                    None,
                    "All inferences should be solved at this point."
                );
                Some(inference.rewrite(concrete_wrapper).no_err())
            }));
        }
    }
    diagnostics
        .build()
        .expect_with_db(db.elongate(), "Internal error: Inference for wrappers generics failed.");
    Ok(all_abi_functions)
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

/// Returns the module id of the submodule of a module.
fn get_submodule_id(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    submodule_name: &str,
) -> anyhow::Result<ModuleId> {
    match db
        .module_item_by_name(module_id, submodule_name.into())
        .to_option()
        .with_context(|| "Failed to initiate a lookup in the {module_name} module.")?
    {
        Some(ModuleItemId::Submodule(submodule_id)) => Ok(ModuleId::Submodule(submodule_id)),
        _ => anyhow::bail!(
            "Failed to get the submodule `{submodule_name}` of `{}`.",
            module_id.full_path(db.upcast())
        ),
    }
}

/// Sierra information of a contract.
#[derive(Clone, Serialize, Deserialize, PartialEq, Debug)]
pub struct ContractInfo {
    /// Sierra function of the constructor.
    pub constructor: Option<FunctionId>,
    /// Sierra functions of the external functions.
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
    pub externals: OrderedHashMap<Felt252, FunctionId>,
    /// Sierra functions of the l1 handler functions.
    #[serde(
        serialize_with = "serialize_ordered_hashmap_vec",
        deserialize_with = "deserialize_ordered_hashmap_vec"
    )]
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
    let constant = db.constant_semantic_data(constant_id).unwrap();
    let class_hash: Felt252 =
        extract_matches!(&constant.exprs[constant.value], Expr::Literal).value.clone().into();

    // Extract functions.
    let SemanticEntryPoints { external, l1_handler, constructor } =
        extract_semantic_entrypoints(db.upcast(), contract)?;
    let externals =
        external.into_iter().map(|f| get_selector_and_sierra_function(db, &f, replacer)).collect();
    let l1_handlers = l1_handler
        .into_iter()
        .map(|f| get_selector_and_sierra_function(db, &f, replacer))
        .collect();
    let constructors: Vec<_> = constructor
        .into_iter()
        .map(|f| get_selector_and_sierra_function(db, &f, replacer))
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
    function_with_body: &Aliased<lowering::ids::ConcreteFunctionWithBodyId>,
    replacer: &T,
) -> (Felt252, FunctionId) {
    let function_id = function_with_body.value.function_id(db.upcast()).expect("Function error.");
    let sierra_id = replacer.replace_function_id(&db.intern_sierra_function(function_id));
    let selector: Felt252 = starknet_keccak(function_with_body.alias.as_bytes()).into();
    (selector, sierra_id)
}
