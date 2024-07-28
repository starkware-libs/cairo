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
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::LookupIntern;
use smol_str::SmolStr;

use crate::abi::{ABIError, AbiBuilder, BuilderConfig};
use crate::contract::module_contract;
use crate::plugin::consts::{EMBEDDABLE_ATTR, FLAT_ATTR, STORAGE_NODE_ATTR};

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

/// Plugin to add diagnostics for contracts with multiple paths to the same place in the memory.
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
            if !item.has_attr(db.upcast(), STORAGE_NODE_ATTR) {
                continue;
            }
            let paths_data =
                &mut StorageAnalyzerStatus { term_to_virtual_paths: OrderedHashMap::default() };
            let Ok(members) = db.struct_members(*id) else { continue };
            for (member_name, member) in members.iter() {
                member_analyze(
                    db,
                    member,
                    member_name.clone(),
                    paths_data,
                    &mut vec![],
                    member.id.stable_ptr(db.upcast()).untyped(),
                );
            }
            paths_data.find_duplicate_paths(&mut diagnostics);
        }
        diagnostics
    }
}

/// Status of the storage analyzer.
/// Used to contain and spawn every virtual path by the first non-flat member.
pub struct StorageAnalyzerStatus {
    pub term_to_virtual_paths: OrderedHashMap<SmolStr, (Vec<Vec<SmolStr>>, SyntaxStablePtrId)>,
}

impl StorageAnalyzerStatus {
    fn add_new_path(
        &mut self,
        member_name: SmolStr,
        user_data_path: Vec<SmolStr>,
        pointer_to_code: SyntaxStablePtrId,
    ) {
        if !self.term_to_virtual_paths.contains_key(&member_name) {
            self.term_to_virtual_paths.insert(member_name.clone(), (vec![], pointer_to_code));
        }
        self.term_to_virtual_paths.get_mut(&member_name).unwrap().0.push(user_data_path);
    }
    /// Find duplicate paths in the storage that end in the same non-flat member.
    fn find_duplicate_paths(&self, diagnostics: &mut Vec<PluginDiagnostic>) {
        for (_, (user_data_paths, pointer_to_code)) in self.term_to_virtual_paths.iter() {
            if user_data_paths.len() <= 1 {
                continue;
            }
            diagnostics.push(PluginDiagnostic::warning(
                *pointer_to_code,
                format!(
                    "{} different paths mapped to one actual path in memory.\n",
                    user_data_paths.len(),
                ) + user_data_paths
                    .iter()
                    .enumerate()
                    .map(|(index, path)| {
                        format!("                    {} path is: {}", index + 1, path.join("."))
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .as_str(),
            ));
        }
    }
}

/// Traverse the members of a struct and analyze them.
fn member_analyze(
    db: &dyn SemanticGroup,
    member: &Member,
    member_name: SmolStr,
    paths_data: &mut StorageAnalyzerStatus,
    user_data_path: &mut Vec<SmolStr>,
    pointer_to_code: SyntaxStablePtrId,
) {
    user_data_path.push(member_name.clone());
    if !member.id.stable_ptr(db.upcast()).lookup(db.upcast()).has_attr(db.upcast(), FLAT_ATTR) {
        paths_data.add_new_path(member_name, user_data_path.clone(), pointer_to_code);
        user_data_path.pop();
        return;
    }
    let TypeLongId::Concrete(ConcreteTypeId::Struct(member_struct)) = member.ty.lookup_intern(db)
    else {
        paths_data.add_new_path(member_name, user_data_path.clone(), pointer_to_code);
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
        );
    }
    user_data_path.pop();
}
