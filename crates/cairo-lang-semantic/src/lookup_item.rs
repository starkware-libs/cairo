use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, ImplAliasId, ImplDefId,
    ImplFunctionId, LanguageElementId, LookupItemId, ModuleItemId, StructId, SubmoduleId, TraitId,
    TypeAliasId, UseId,
};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::resolve::ResolverData;

pub trait LookupItemEx {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;
}

impl LookupItemEx for LookupItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            LookupItemId::ModuleItem(item) => item.resolver_data(db),
            LookupItemId::ImplFunction(item) => item.resolver_data(db),
        }
    }
}

impl LookupItemEx for ModuleItemId {
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

impl LookupItemEx for ConstantId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.constant_resolver_data(*self)
    }
}
impl LookupItemEx for SubmoduleId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()))))
    }
}
impl LookupItemEx for UseId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.use_resolver_data(*self)
    }
}
impl LookupItemEx for ImplAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_alias_resolver_data(*self)
    }
}
impl LookupItemEx for ImplDefId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_def_resolver_data(*self)
    }
}
impl LookupItemEx for ExternTypeId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()))))
    }
}
impl LookupItemEx for ExternFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.extern_function_declaration_resolver_data(*self)
    }
}
impl LookupItemEx for FreeFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.free_function_declaration_resolver_data(*self)
    }
}
impl LookupItemEx for StructId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.struct_declaration_resolver_data(*self)
    }
}
impl LookupItemEx for EnumId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.enum_declaration_resolver_data(*self)
    }
}
impl LookupItemEx for TypeAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.type_alias_resolver_data(*self)
    }
}
impl LookupItemEx for TraitId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_resolver_data(*self)
    }
}

impl LookupItemEx for ImplFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_function_resolver_data(*self)
    }
}
