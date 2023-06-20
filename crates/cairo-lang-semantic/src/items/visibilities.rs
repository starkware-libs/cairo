use smol_str::SmolStr;
use cairo_lang_defs::ids::{ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, LanguageElementId, ModuleId, StructId, TraitId, TypeAliasId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;

/// Query implementation of [SemanticGroup::constant_visible_in].
pub fn constant_visible_in(db: &dyn SemanticGroup, constant_id: ConstantId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &constant_id);
    let visibility = db.constant_visibility(constant_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::module_visible_in].
pub fn module_visible_in(db: &dyn SemanticGroup, source_module_id: ModuleId, module_id: ModuleId) -> Maybe<bool> {
    match module_id {
        ModuleId::CrateRoot(_) => Ok(true),
        ModuleId::Submodule(submodule_id) => {
            let visibility = db.module_visibility(submodule_id)?;
            Ok(item_visible_in(db, &visibility, source_module_id, module_id))
        }
    }
}

/// Query implementation of [SemanticGroup::extern_function_visible_in].
pub fn extern_function_visible_in(db: &dyn SemanticGroup, extern_function_id: ExternFunctionId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &extern_function_id);
    let visibility = db.extern_function_visibility(extern_function_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::free_function_visible_in].
pub fn free_function_visible_in(db: &dyn SemanticGroup, free_function_id: FreeFunctionId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &free_function_id);
    let visibility = db.free_function_visibility(free_function_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::enum_visible_in].
pub fn enum_visible_in(db: &dyn SemanticGroup, enum_id: EnumId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &enum_id);
    let visibility = db.enum_visibility(enum_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::struct_visible_in].
pub fn struct_visible_in(db: &dyn SemanticGroup, struct_id: StructId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &struct_id);
    let visibility = db.struct_visibility(struct_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::extern_type_visible_in].
pub fn extern_type_visible_in(db: &dyn SemanticGroup, extern_type_id: ExternTypeId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &extern_type_id);
    let visibility = db.extern_type_visibility(extern_type_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::type_alias_visible_in].
pub fn type_alias_visible_in(db: &dyn SemanticGroup, type_alias_id: TypeAliasId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &type_alias_id);
    let visibility = db.type_alias_visibility(type_alias_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::trait_visible_in].
pub fn trait_visible_in(db: &dyn SemanticGroup, trait_id: TraitId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &trait_id);
    let visibility = db.trait_visibility(trait_id)?;
    Ok(item_visible_in(db, &visibility, source_module_id, module_id))
}

/// Query implementation of [SemanticGroup::struct_member_visible_in].
pub fn struct_member_visible_in(db: &dyn SemanticGroup, struct_id: StructId, member_name: SmolStr, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &struct_id);
    if let Some(visibility) = db.struct_member_visibility(struct_id, member_name)? {
        return Ok(item_visible_in(db, &visibility, source_module_id, module_id))
    } else {
        Ok(false)
    }
}

fn item_visible_in(db: &dyn SemanticGroup, visibility: &ast::Visibility, source_module_id: ModuleId, module_id: ModuleId) -> bool {
    if source_module_id == module_id {
        return true
    }
    match visibility {
        ast::Visibility::Public(_) => true,
        ast::Visibility::Default(_) => db.module_ancestors(module_id).contains(&source_module_id),
    }
}

fn item_module_id<T: LanguageElementId>(db: &dyn SemanticGroup, item_id: &T) -> ModuleId {
    item_id.module_file_id(db.upcast()).0
}
