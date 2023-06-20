use smol_str::SmolStr;
use cairo_lang_defs::ids::{ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, LanguageElementId, ModuleId, StructId, TraitId, TypeAliasId};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_syntax::node::ast;
use crate::db::SemanticGroup;
use crate::Visibility;

pub fn semantic_visibility(visibility: &ast::Visibility) -> Visibility {
    match visibility {
        ast::Visibility::Public(_) => Visibility::Public,
        ast::Visibility::Default(_) => Visibility::SubModule,
    }
}

/// Query implementation of [SemanticGroup::constant_visible_in].
pub fn constant_visible_in(db: &dyn SemanticGroup, constant_id: ConstantId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &constant_id);
    let visibility = db.constant_semantic_data(constant_id)?.visibility;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::module_visible_in].
pub fn module_visible_in(db: &dyn SemanticGroup, source_module_id: ModuleId, module_id: ModuleId) -> Maybe<bool> {
    let visibility = db.module_visibility(source_module_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::extern_function_visible_in].
pub fn extern_function_visible_in(db: &dyn SemanticGroup, extern_function_id: ExternFunctionId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &extern_function_id);
    let visibility = db.extern_function_visibility(extern_function_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::free_function_visible_in].
pub fn free_function_visible_in(db: &dyn SemanticGroup, free_function_id: FreeFunctionId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &free_function_id);
    let visibility = db.free_function_visibility(free_function_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::enum_visible_in].
pub fn enum_visible_in(db: &dyn SemanticGroup, enum_id: EnumId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &enum_id);
    let visibility = db.enum_visibility(enum_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::struct_visible_in].
pub fn struct_visible_in(db: &dyn SemanticGroup, struct_id: StructId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &struct_id);
    let visibility = db.struct_visibility(struct_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::extern_type_visible_in].
pub fn extern_type_visible_in(db: &dyn SemanticGroup, extern_type_id: ExternTypeId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &extern_type_id);
    let visibility = db.extern_type_visibility(extern_type_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::type_alias_visible_in].
pub fn type_alias_visible_in(db: &dyn SemanticGroup, type_alias_id: TypeAliasId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &type_alias_id);
    let visibility = db.type_alias_visibility(type_alias_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::trait_visible_in].
pub fn trait_visible_in(db: &dyn SemanticGroup, trait_id: TraitId, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &trait_id);
    let visibility = db.trait_visibility(trait_id)?;
    item_visible_in(db, &visibility, source_module_id, module_id)
}

/// Query implementation of [SemanticGroup::struct_member_visible_in].
pub fn struct_member_visible_in(db: &dyn SemanticGroup, struct_id: StructId, member_name: SmolStr, module_id: ModuleId) -> Maybe<bool> {
    let source_module_id = item_module_id(db, &struct_id);
    let members = db.struct_members(struct_id)?;
    if let Some(member) = members.get(&member_name) {
        item_visible_in(db, &member.vis, source_module_id, module_id)
    } else {
        Ok(false)
    }
}

fn item_visible_in(db: &dyn SemanticGroup, visibility: &Visibility, source_module_id: ModuleId, module_id: ModuleId) -> Maybe<bool> {
    match visibility {
        Visibility::Public => Ok(true),
        Visibility::SubModule => {
            let mut nodes = vec![source_module_id];
            while let Some(current) = nodes.pop() {
                if module_id == current {
                    return Ok(true)
                }
                for submodule_id in db.module_submodules_ids(current.clone())? {
                    nodes.push(ModuleId::Submodule(submodule_id));
                }
            }
            Ok(false)
        }
    }
}

fn item_module_id<T: LanguageElementId>(db: &dyn SemanticGroup, item_id: &T) -> ModuleId {
    item_id.module_file_id(db.upcast()).0
}
