use cairo_lang_defs::ids::ModuleId;
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
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::LookupIntern;
use smol_str::SmolStr;

use crate::abi::{ABIError, AbiBuilder, BuilderConfig};
use crate::contract::module_contract;
use crate::plugin::consts::{
    COMPONENT_ATTR, CONTRACT_ATTR, EMBEDDABLE_ATTR, FLAT_ATTR, STORAGE_ATTR, STORAGE_NODE_ATTR,
    STORAGE_STRUCT_NAME, SUBSTORAGE_ATTR,
};

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
            diagnostics.push(PluginDiagnostic::warning(
                err.location(db)
                    .unwrap_or_else(|| contract.submodule_id.stable_ptr(db.upcast()).untyped()),
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
        let Ok(module_structs) = db.module_structs(module_id) else {
            return vec![];
        };

        // Analyze all the members of the struct.
        for (id, item) in module_structs.iter() {
            if !item.has_attr(db.upcast(), STORAGE_NODE_ATTR)
                && !item.has_attr(db.upcast(), STORAGE_ATTR)
                && (item.name(db.upcast()).text(db.upcast()) != STORAGE_STRUCT_NAME
                    || (!module_id.has_attr(db, CONTRACT_ATTR).unwrap_or_default()
                        && !module_id.has_attr(db, COMPONENT_ATTR).unwrap_or_default()))
            {
                continue;
            }
            let paths_data = &mut StorageStructMembers { name_to_paths: OrderedHashMap::default() };
            let Ok(members) = db.struct_members(*id) else { continue };
            for (member_name, member) in members.iter() {
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
                    &mut diagnostics,
                );
            }
        }
        diagnostics
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
                    "The path `{}` collides with existing path `{}`.",
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
    if !(member.id.stable_ptr(db.upcast()).lookup(db.upcast()).has_attr(db.upcast(), FLAT_ATTR)
        || member
            .id
            .stable_ptr(db.upcast())
            .lookup(db.upcast())
            .has_attr(db.upcast(), SUBSTORAGE_ATTR))
    {
        paths_data.handle(member_name, user_data_path.clone(), pointer_to_code, diagnostics);
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
