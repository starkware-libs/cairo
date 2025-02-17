use cairo_lang_defs::ids::{EnumId, LanguageElementId, ModuleId, ModuleItemId, StructId};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::helper::ModuleHelper;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::items::imp::ImplLookupContext;
use cairo_lang_semantic::items::structure::Member;
use cairo_lang_semantic::plugin::AnalyzerPlugin;
use cairo_lang_semantic::types::get_impl_at_context;
use cairo_lang_semantic::{
    ConcreteTraitId, ConcreteTraitLongId, ConcreteTypeId, GenericArgumentId, TypeId, TypeLongId,
};
use cairo_lang_syntax::attribute::consts::STARKNET_INTERFACE_ATTR;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern};
use smol_str::SmolStr;

use crate::abi::{ABIError, AbiBuilder, BuilderConfig};
use crate::contract::module_contract;
use crate::plugin::consts::{
    COMPONENT_ATTR, CONTRACT_ATTR, EMBEDDABLE_ATTR, STORAGE_ATTR, STORAGE_NODE_ATTR,
    STORAGE_STRUCT_NAME, STORE_TRAIT,
};
use crate::plugin::storage_interfaces::{StorageMemberKind, get_member_storage_config};
use crate::plugin::utils::has_derive;

const ALLOW_NO_DEFAULT_VARIANT_ATTR: &str = "starknet::store_no_default_variant";
const ALLOW_COLLIDING_PATHS_ATTR: &str = "starknet::colliding_storage_paths";
const ALLOW_INVALID_STORAGE_MEMBERS_ATTR: &str = "starknet::invalid_storage_member_types";

/// Plugin to add diagnostics for contracts for bad ABI generation.
#[derive(Default, Debug)]
pub struct ABIAnalyzer;

impl AnalyzerPlugin for ABIAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        add_non_starknet_interface_embeddable_diagnostics(db, module_id, &mut diagnostics);
        add_abi_diagnostics(db, module_id, &mut diagnostics);
        diagnostics
    }
}

/// Add diagnostics for embeddable impls that do not implement a starknet interface.
fn add_non_starknet_interface_embeddable_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Ok(impls) = db.module_impls(module_id) else {
        return;
    };
    for (id, item) in impls.iter() {
        if !item.has_attr(db.upcast(), EMBEDDABLE_ATTR) {
            continue;
        }
        let Ok(impl_trait) = db.impl_def_trait(*id) else { continue };
        if !impl_trait.has_attr(db.upcast(), STARKNET_INTERFACE_ATTR).unwrap_or(true) {
            diagnostics.push(PluginDiagnostic::warning(
                item.stable_ptr().untyped(),
                "Impls with the embeddable attribute must implement a starknet interface trait."
                    .to_string(),
            ));
        }
    }
}

/// Add diagnostics for ABI generation.
fn add_abi_diagnostics(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Some(contract) = module_contract(db, module_id) else {
        return;
    };
    let Ok(abi_builder) = AbiBuilder::from_submodule(
        db,
        contract.submodule_id,
        BuilderConfig { account_contract_validations: true },
    ) else {
        return;
    };
    for err in abi_builder.errors() {
        if !matches!(err, ABIError::SemanticError) {
            let location = err.location(db).unwrap_or_else(|| {
                if let Ok(Some(attr)) = contract.module_id().find_attr(db, CONTRACT_ATTR) {
                    attr.stable_ptr.untyped()
                } else {
                    contract.submodule_id.stable_ptr(db.upcast()).untyped()
                }
            });

            diagnostics.push(PluginDiagnostic::warning(
                location,
                format!("Failed to generate ABI: {err}"),
            ));
        }
    }
}

/// Plugin to add diagnostics for contracts with multiple paths to the same location in storage.
#[derive(Default, Debug)]
pub struct StorageAnalyzer;

impl AnalyzerPlugin for StorageAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];

        let syntax_db = db.upcast();

        // Analyze all the structs in the module.
        if let Ok(module_structs) = db.module_structs(module_id) {
            for (id, item) in module_structs.iter() {
                // Only run the analysis on storage structs or structs with the storage attribute.
                if item.has_attr(syntax_db, STORAGE_NODE_ATTR)
                    || item.has_attr(syntax_db, STORAGE_ATTR)
                    || (item.name(syntax_db).text(syntax_db) == STORAGE_STRUCT_NAME
                        && (module_id.has_attr(db, CONTRACT_ATTR).unwrap_or_default()
                            || module_id.has_attr(db, COMPONENT_ATTR).unwrap_or_default()))
                {
                    analyze_storage_struct(db, *id, &mut diagnostics);
                }
            }
        }
        // Analyze all the enums in the module.
        if let Ok(module_enums) = db.module_enums(module_id) {
            for (id, item) in module_enums.iter() {
                if has_derive(item, syntax_db, STORE_TRAIT).is_some()
                    && !item.has_attr_with_arg(syntax_db, "allow", ALLOW_NO_DEFAULT_VARIANT_ATTR)
                {
                    add_derive_store_enum_diags(db, *id, &mut diagnostics);
                }
            }
        }
        diagnostics
    }

    fn declared_allows(&self) -> Vec<String> {
        vec![
            ALLOW_NO_DEFAULT_VARIANT_ATTR.to_string(),
            ALLOW_COLLIDING_PATHS_ATTR.to_string(),
            ALLOW_INVALID_STORAGE_MEMBERS_ATTR.to_string(),
        ]
    }
}

/// Analyzes a storage struct:
/// - Ensures all members implement `ValidStorageTypeTrait`.
/// - Checks for multiple paths to the same location in storage.
fn analyze_storage_struct(
    db: &dyn SemanticGroup,
    struct_id: StructId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Ok(members) = db.struct_members(struct_id) else {
        return;
    };
    let allow_invalid_members =
        struct_id.has_attr_with_arg(db, "allow", ALLOW_INVALID_STORAGE_MEMBERS_ATTR) == Ok(true);
    let allow_collisions =
        struct_id.has_attr_with_arg(db, "allow", ALLOW_COLLIDING_PATHS_ATTR) == Ok(true);

    let lookup_context = ImplLookupContext::new(
        struct_id.module_file_id(db.upcast()).0,
        match db.struct_generic_params(struct_id) {
            Ok(params) => params.into_iter().map(|p| p.id()).collect(),
            Err(_) => return,
        },
    );
    let paths_data = &mut StorageStructMembers { name_to_paths: OrderedHashMap::default() };

    for (member_name, member) in members.iter() {
        let member_ast = member.id.stable_ptr(db.upcast()).lookup(db.upcast());
        let member_type = member.ty.lookup_intern(db);
        let concrete_trait_id = concrete_valid_storage_trait(db, db.intern_type(member_type));

        let member_allows_invalid =
            member_ast.has_attr_with_arg(db.upcast(), "allow", ALLOW_INVALID_STORAGE_MEMBERS_ATTR);

        if !(allow_invalid_members || member_allows_invalid) {
            let inference_result =
                get_impl_at_context(db, lookup_context.clone(), concrete_trait_id, None);

            if let Err(inference_error) = inference_result {
                let type_pointer = member_ast.type_clause(db.upcast()).ty(db.upcast());
                diagnostics.push(PluginDiagnostic::warning(
                    &type_pointer,
                    format!(
                        "Missing `ValidStorageTypeTrait` for member type. Inference failed with: \
                         `{}`. Possible solutions: implement `Store`, mark type with \
                         `#[storage_node]`, or use valid args for `Vec` or `Map` library types. \
                         To suppress this warning, use \
                         `#[allow(starknet::invalid_storage_member_types)]`.",
                        inference_error.format(db.elongate())
                    ),
                ));
            }
        }

        // Check for storage path collisions.
        if allow_collisions
            || member_ast.has_attr_with_arg(db.upcast(), "allow", ALLOW_COLLIDING_PATHS_ATTR)
        {
            continue;
        }

        member_analyze(
            db,
            member,
            member_name.clone(),
            paths_data,
            &mut vec![],
            member_ast.name(db.upcast()).stable_ptr().untyped(),
            diagnostics,
        );
    }
}

/// Helper for the storage analyzer.
pub struct StorageStructMembers {
    /// Maps the name in actual storage to the path in actual user code.
    pub name_to_paths: OrderedHashMap<SmolStr, Vec<SmolStr>>,
}

impl StorageStructMembers {
    fn handle(
        &mut self,
        member_name: SmolStr,
        path_to_member: Vec<SmolStr>,
        pointer_to_code: SyntaxStablePtrId,
        diagnostics: &mut Vec<PluginDiagnostic>,
    ) {
        if let Some(existing_path) = self.name_to_paths.get(&member_name) {
            diagnostics.push(PluginDiagnostic::warning(
                pointer_to_code,
                format!(
                    "The path `{}` collides with existing path `{}`. Fix or add \
                     `#[allow({ALLOW_COLLIDING_PATHS_ATTR})]` if intentional.",
                    path_to_member.join("."),
                    existing_path.join(".")
                ),
            ));
        } else {
            self.name_to_paths.insert(member_name.clone(), path_to_member);
        }
    }
}

/// This function is called recursively to analyze all the members of a struct.
/// For every member, it checks if it has the `flat` attribute. If it does, it
/// recursively analyzes all the members of the struct that the member points to.
/// Otherwise, it adds the path to the `StorageStructMembers` struct.
/// For example, if the path is:
/// member1.member2.member3
/// where member1 is flat with a member member2 which is flat and a member member3 which is not
/// flat, The function will iterate until it reaches member3 and add the path
/// member1.member2.member3 to the `StorageStructMembers` struct.
fn member_analyze(
    db: &dyn SemanticGroup,
    member: &Member,
    member_name: SmolStr,
    paths_data: &mut StorageStructMembers,
    user_data_path: &mut Vec<SmolStr>,
    pointer_to_code: SyntaxStablePtrId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    user_data_path.push(member_name.clone());
    let member_ast = member.id.stable_ptr(db.upcast()).lookup(db.upcast());
    // Ignoring diagnostics as these would have been reported previously.
    let config = get_member_storage_config(db.upcast(), &member_ast, &mut vec![]);
    if config.kind == StorageMemberKind::Basic {
        let name = config.rename.map(Into::into).unwrap_or(member_name);
        paths_data.handle(name, user_data_path.clone(), pointer_to_code, diagnostics);
        user_data_path.pop();
        return;
    }
    let TypeLongId::Concrete(ConcreteTypeId::Struct(member_struct)) = member.ty.lookup_intern(db)
    else {
        paths_data.handle(member_name, user_data_path.clone(), pointer_to_code, diagnostics);
        user_data_path.pop();
        return;
    };
    for (inner_member_name, inner_member) in
        db.struct_members(member_struct.lookup_intern(db).struct_id).unwrap().iter()
    {
        member_analyze(
            db,
            inner_member,
            inner_member_name.clone(),
            paths_data,
            user_data_path,
            pointer_to_code,
            diagnostics,
        );
    }
    user_data_path.pop();
}

/// Adds diagnostics for an enum deriving `starknet::Store`.
///
/// Specifically finds cases missing a `#[default]` variant.
fn add_derive_store_enum_diags(
    db: &dyn SemanticGroup,
    id: EnumId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    let Ok(variants) = db.enum_variants(id) else { return };
    if !variants.iter().any(|(_, variant_id)| {
        variant_id.stable_ptr(db.upcast()).lookup(db.upcast()).has_attr(db.upcast(), "default")
    }) {
        diagnostics.push(PluginDiagnostic::warning(
            id.stable_ptr(db.upcast()).untyped(),
            format!(
                "Enum with `#[derive({STORE_TRAIT})] has no default variant. Either add one, or \
                 add `#[allow({ALLOW_NO_DEFAULT_VARIANT_ATTR})]`"
            ),
        ));
    }
}

/// Resolves the concrete `ValidStorageTypeTrait` for a given type.
fn concrete_valid_storage_trait(db: &dyn SemanticGroup, ty: TypeId) -> ConcreteTraitId {
    let module_id = ModuleHelper::core(db).submodule("starknet").submodule("storage").id;
    let name = "ValidStorageTypeTrait";
    let Ok(Some(ModuleItemId::Trait(trait_id))) = db.module_item_by_name(module_id, name.into())
    else {
        panic!("`{}` not found in `{}`.", name, module_id.full_path(db.upcast()));
    };
    ConcreteTraitLongId { trait_id, generic_args: vec![GenericArgumentId::Type(ty)] }.intern(db)
}
