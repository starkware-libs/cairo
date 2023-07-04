use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FileIndex, FreeFunctionId,
    FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LookupItemId,
    ModuleFileId, ModuleId, ModuleItemId, StructId, SubmoduleId, TraitId, TypeAliasId, UseId,
};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::items::visibilities::peek_visible_in;
use crate::resolve::ResolverData;

pub trait ItemEx {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool>;
}

pub trait LookupItemEx: ItemEx {
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

impl ItemEx for LookupItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            LookupItemId::ModuleItem(item) => item.resolver_data(db),
            LookupItemId::ImplFunction(item) => item.resolver_data(db),
        }
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        match self {
            LookupItemId::ModuleItem(item) => item.peek_visible_in(db, module_id),
            LookupItemId::ImplFunction(item) => item.peek_visible_in(db, module_id),
        }
    }
}

impl ItemEx for ModuleItemId {
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

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        match self {
            ModuleItemId::Constant(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Submodule(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Use(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::ImplAlias(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Impl(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::ExternType(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::ExternFunction(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::FreeFunction(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Struct(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Enum(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::TypeAlias(item) => item.peek_visible_in(db, module_id),
            ModuleItemId::Trait(item) => item.peek_visible_in(db, module_id),
        }
    }
}

impl ItemEx for ConstantId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.constant_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.constant_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for SubmoduleId {
    fn resolver_data(&self, _db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        let module_id = ModuleId::Submodule(*self);
        let module_file_id = ModuleFileId(module_id, FileIndex(0));
        Ok(Arc::new(ResolverData::new(module_file_id)))
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.module_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for UseId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.use_resolver_data(*self)
    }

    // TODO(spapini): Use need some reexport logic.
    fn peek_visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl ItemEx for ImplAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_alias_resolver_data(*self)
    }

    // TODO(spapini): Impl need to inference logic.
    // During the inference stage it needs to hide the impl which is not visible in current scope.
    // It will reduce the chance of conflict impls.
    fn peek_visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl ItemEx for ImplDefId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_def_resolver_data(*self)
    }

    // TODO(spapini): Similar inference logic as impl
    fn peek_visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
impl ItemEx for ExternTypeId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()))))
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.extern_type_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for ExternFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.extern_function_declaration_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.extern_function_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for FreeFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.free_function_declaration_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.free_function_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for StructId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.struct_declaration_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.struct_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for EnumId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.enum_declaration_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.enum_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for TypeAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.type_alias_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.type_alias_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}
impl ItemEx for TraitId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_resolver_data(*self)
    }

    fn peek_visible_in(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Maybe<bool> {
        let source_module_id = self.parent_module(db.upcast());
        let visibility = db.trait_visibility(*self)?;
        Ok(peek_visible_in(db, &visibility, source_module_id, module_id))
    }
}

impl ItemEx for ImplFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_function_resolver_data(*self)
    }

    // TODO(spapini): Distinguish trait function and non-trait function.
    // Trait function must always be public, while non-trait visibility could be any possible
    // modifier.
    fn peek_visible_in(&self, _db: &dyn SemanticGroup, _module_id: ModuleId) -> Maybe<bool> {
        Ok(true)
    }
}
