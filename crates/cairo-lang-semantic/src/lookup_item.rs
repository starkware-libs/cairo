use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FileIndex, FreeFunctionId,
    FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LookupItemId,
    ModuleFileId, ModuleId, ModuleItemId, StructId, SubmoduleId, TraitId, TypeAliasId, UseId,
};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::items::visibilities::visible_in;
use crate::resolve::ResolverData;

pub trait HasResolverData {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;
    
    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool>;
}

pub trait LookupItemEx: HasResolverData {
    fn function_with_body(&self) -> Option<FunctionWithBodyId>;
}

impl LookupItemEx for LookupItemId {
    fn function_with_body(&self) -> Option<FunctionWithBodyId> {
        match self {
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
                Some(FunctionWithBodyId::Free(*free_function_id))
            }
            LookupItemId::ImplFunction(impl_function_id) => {
                Some(FunctionWithBodyId::Impl(*impl_function_id))
            }
            _ => None,
        }
    }
}

impl HasResolverData for LookupItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            LookupItemId::ModuleItem(item) => item.resolver_data(db),
            LookupItemId::ImplFunction(item) => item.resolver_data(db),
        }
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        match self {
            LookupItemId::ModuleItem(item) => item.visible_in(db, module_id),
            LookupItemId::ImplFunction(item) => item.visible_in(db, module_id),
        }
    }
}

impl HasResolverData for ModuleItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            ModuleItemId::Constant(item) => item.resolver_data(db),
            ModuleItemId::Submodule(item) => item.resolver_data(db),
            ModuleItemId::Use(item) => item.resolver_data(db),
            ModuleItemId::ImplAlias(item) => item.resolver_data(db),
            ModuleItemId::Impl(item) => item.resolver_data(db),
            ModuleItemId::ExternType(item) => item.resolver_data(db),
            ModuleItemId::ExternFunction(item) => item.resolver_data(db),
            ModuleItemId::FreeFunction(item) => item.resolver_data(db),
            ModuleItemId::Struct(item) => item.resolver_data(db),
            ModuleItemId::Enum(item) => item.resolver_data(db),
            ModuleItemId::TypeAlias(item) => item.resolver_data(db),
            ModuleItemId::Trait(item) => item.resolver_data(db),
        }
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        match self {
            ModuleItemId::Constant(item) => item.visible_in(db, module_id),
            ModuleItemId::Submodule(item) => item.visible_in(db, module_id),
            ModuleItemId::Use(item) => item.visible_in(db, module_id),
            ModuleItemId::ImplAlias(item) => item.visible_in(db, module_id),
            ModuleItemId::Impl(item) => item.visible_in(db, module_id),
            ModuleItemId::ExternType(item) => item.visible_in(db, module_id),
            ModuleItemId::ExternFunction(item) => item.visible_in(db, module_id),
            ModuleItemId::FreeFunction(item) => item.visible_in(db, module_id),
            ModuleItemId::Struct(item) => item.visible_in(db, module_id),
            ModuleItemId::Enum(item) => item.visible_in(db, module_id),
            ModuleItemId::TypeAlias(item) => item.visible_in(db, module_id),
            ModuleItemId::Trait(item) => item.visible_in(db, module_id),
        }
    }
}

impl HasResolverData for ConstantId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.constant_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.constant_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for SubmoduleId {
    fn resolver_data(&self, _db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        let module_id = ModuleId::Submodule(*self);
        let module_file_id = ModuleFileId(module_id, FileIndex(0));
        Ok(Arc::new(ResolverData::new(module_file_id)))
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = ModuleId::Submodule(*self);
        let visibility = db.module_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for UseId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.use_resolver_data(*self)
    }

    fn visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl HasResolverData for ImplAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_alias_resolver_data(*self)
    }

    fn visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl HasResolverData for ImplDefId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_def_resolver_data(*self)
    }

    fn visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl HasResolverData for ExternTypeId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()))))
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.extern_type_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for ExternFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.extern_function_declaration_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.extern_function_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for FreeFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.free_function_declaration_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.free_function_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for StructId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.struct_declaration_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.struct_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for EnumId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.enum_declaration_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.enum_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for TypeAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.type_alias_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.type_alias_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl HasResolverData for TraitId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_resolver_data(*self)
    }

    fn visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.trait_visibility(*self)?;
        Ok(visible_in(db, &visibility, source_module_id, module_id))
    }
}

impl HasResolverData for ImplFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_function_resolver_data(*self)
    }
    fn visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
