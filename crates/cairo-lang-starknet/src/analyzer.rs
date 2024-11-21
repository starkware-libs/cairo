use cairo_lang_defs::ids::{EnumId, ModuleId, StructId};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::attribute::SemanticQueryAttrs;
use cairo_lang_semantic::items::structure::Member;
use cairo_lang_semantic::plugin::AnalyzerPlugin;
use cairo_lang_semantic::{ConcreteTypeId, TypeLongId};
use cairo_lang_syntax::attribute::consts::STARKNET_INTERFACE_ATTR;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
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

/// Plugin to add diagnostics for contracts for bad ABI generation.
#[derive(Default, Debug, PartialEq, Eq, Hash)]
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
    let Ok(abi_builder) = AbiBuilder::from_submodule(db, contract.submodule_id, BuilderConfig {
        account_contract_validations: true,
    }) else {
        return;
    };
    for err in abi_builder.errors() {
        if !matches!(err, ABIError::SemanticError) {
            diagnostics.push(PluginDiagnostic::warning(
                err.location(db)
                    .unwrap_or_else(|| contract.submodule_id.stable_ptr(db.upcast()).untyped()),
                format!("Failed to generate ABI: {err}"),
            ));
        }
    }
}

/// Plugin to add diagnostics for contracts with multiple paths to the same location in storage.
#[derive(Default, Debug, PartialEq, Eq, Hash)]
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
                    add_storage_struct_diags(db, *id, &mut diagnostics);
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
        vec![ALLOW_NO_DEFAULT_VARIANT_ATTR.to_string(), ALLOW_COLLIDING_PATHS_ATTR.to_string()]
    }
}

/// Adds diagnostics for a storage struct.
///
/// Specifically finds cases where there are multiple paths to the same location in storage.
fn add_storage_struct_diags(
    db: &dyn SemanticGroup,
    id: StructId,
    diagnostics: &mut Vec<PluginDiagnostic>,
) {
    if id.has_attr_with_arg(db, "allow", ALLOW_COLLIDING_PATHS_ATTR) == Ok(true) {
        return;
    }
    let Ok(members) = db.struct_members(id) else {
        return;
    };
    let paths_data = &mut StorageStructMembers { name_to_paths: OrderedHashMap::default() };
    for (member_name, member) in members.iter() {
        if member.id.stable_ptr(db.upcast()).lookup(db.upcast()).has_attr_with_arg(
            db.upcast(),
            "allow",
            ALLOW_COLLIDING_PATHS_ATTR,
        ) {
            continue;
        }
        member_analyze(
            db,
            member,
            member_name.clone(),
            paths_data,
            &mut vec![],
            member
                .id
                .stable_ptr(db.upcast())
                .lookup(db.upcast())
                .name(db.upcast())
                .stable_ptr()
                .untyped(),
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

/// Adds diagnostics for a enum deriving `starknet::Store`.
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
