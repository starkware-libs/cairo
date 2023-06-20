use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FileIndex, FreeFunctionId,
    FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId, LanguageElementId, LookupItemId,
    ModuleFileId, ModuleId, ModuleItemId, StructId, SubmoduleId, TraitId, TypeAliasId, UseId,
};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::resolve::ResolverData;

pub trait HasResolverData {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;
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
}

impl HasResolverData for ConstantId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.constant_resolver_data(*self)
    }
}
impl HasResolverData for SubmoduleId {
    fn resolver_data(&self, _db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        let module_id = ModuleId::Submodule(*self);
        let module_file_id = ModuleFileId(module_id, FileIndex(0));
        Ok(Arc::new(ResolverData::new(module_file_id)))
    }
}
impl HasResolverData for UseId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.use_resolver_data(*self)
    }
}
impl HasResolverData for ImplAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_alias_resolver_data(*self)
    }
}
impl HasResolverData for ImplDefId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_def_resolver_data(*self)
    }
}
impl HasResolverData for ExternTypeId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()))))
    }
}
impl HasResolverData for ExternFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.extern_function_declaration_resolver_data(*self)
    }
}
impl HasResolverData for FreeFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.free_function_declaration_resolver_data(*self)
    }
}
impl HasResolverData for StructId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.struct_declaration_resolver_data(*self)
    }
}
impl HasResolverData for EnumId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.enum_declaration_resolver_data(*self)
    }
}
impl HasResolverData for TypeAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.type_alias_resolver_data(*self)
    }
}
impl HasResolverData for TraitId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_resolver_data(*self)
    }
}

impl HasResolverData for ImplFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_function_resolver_data(*self)
    }
}
