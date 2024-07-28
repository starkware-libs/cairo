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
#[derive(Default, Debug)]
pub struct StorageAnalyzer;

impl AnalyzerPlugin for StorageAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        add_storage_diagnostics(db, module_id)
    }
}

pub trait StorageAnalyzerStatusTrait {
    fn add_new_path(
        &mut self,
        data_path_code: Vec<SmolStr>,
        data_path_user: Vec<SmolStr>,
        pointer_to_code: SyntaxStablePtrId,
    );
    fn find_duplicate_paths(&self) -> Vec<PluginDiagnostic>;
}

pub struct StorageAnalyzerStatus {
    pub actual_to_hole: OrderedHashMap<Vec<SmolStr>, (Vec<Vec<SmolStr>>, SyntaxStablePtrId)>,
}

impl StorageAnalyzerStatusTrait for StorageAnalyzerStatus {
    fn add_new_path(
        &mut self,
        data_path_code: Vec<SmolStr>,
        data_path_user: Vec<SmolStr>,
        pointer_to_code: SyntaxStablePtrId,
    ) {
        if !self.actual_to_hole.contains_key(&data_path_code) {
            self.actual_to_hole.insert(data_path_code.clone(), (vec![], pointer_to_code));
        }
        self.actual_to_hole.get_mut(&data_path_code).unwrap().0.push(data_path_user);
    }
    fn find_duplicate_paths(&self) -> Vec<PluginDiagnostic> {
        let mut diagnostics = vec![];
        for (data_path_code, (data_pathes_user, pointer_to_code)) in self.actual_to_hole.iter() {
            if data_pathes_user.len() <= 1 {
                continue;
            }
            diagnostics.push(PluginDiagnostic::warning(
                *pointer_to_code,
                format!(
                    "{} different virtual paths mapped to one actual path in memory.
                    The actual path is: {}\n",
                    data_pathes_user.len(),
                    data_path_code.join("."),
                ) + data_pathes_user
                    .iter()
                    .enumerate()
                    .map(|(index, path)| {
                        format!("                    {} virtual path is: {}", index, path.join("."))
                    })
                    .collect::<Vec<_>>()
                    .join("\n")
                    .as_str(),
            ));
        }
        diagnostics
    }
}

fn member_analyze(
    db: &dyn SemanticGroup,
    member: &Member,
    member_name: SmolStr,
    paths_data: &mut StorageAnalyzerStatus,
    data_path_code: &mut Vec<SmolStr>,
    data_path_user: &mut Vec<SmolStr>,
    pointer_to_code: SyntaxStablePtrId,
) {
    let is_flat =
        member.id.stable_ptr(db.upcast()).lookup(db.upcast()).has_attr(db.upcast(), FLAT_ATTR);
    data_path_user.push(member_name.clone());
    if !is_flat {
        data_path_code.push(member_name.clone());
    }
    let TypeLongId::Concrete(ConcreteTypeId::Struct(member_struct)) = member.ty.lookup_intern(db)
    else {
        paths_data.add_new_path(data_path_code.clone(), data_path_user.clone(), pointer_to_code);
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
            data_path_code,
            data_path_user,
            pointer_to_code,
        );
    }
    if !is_flat {
        data_path_code.pop();
    }
    data_path_user.pop();
}

fn add_storage_diagnostics(db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
    let Ok(module_structs) = db.module_structs(module_id) else {
        return vec![];
    };
    let paths_data = &mut StorageAnalyzerStatus { actual_to_hole: OrderedHashMap::default() };

    for (id, item) in module_structs.iter() {
        if !item.has_attr(db.upcast(), STORAGE_NODE_ATTR) {
            continue;
        }
        let Ok(members) = db.struct_members(*id) else { continue };
        for (member_name, member) in members.iter() {
            member_analyze(
                db,
                member,
                member_name.clone(),
                paths_data,
                &mut vec![],
                &mut vec![],
                item.stable_ptr().untyped(),
            );
        }
    }
    paths_data.find_duplicate_paths()
}
