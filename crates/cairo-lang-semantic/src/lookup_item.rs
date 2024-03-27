use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FileIndex, FreeFunctionId,
    FunctionWithBodyId, ImplAliasId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId,
    LanguageElementId, LookupItemId, ModuleFileId, ModuleId, ModuleItemId, ModuleTypeAliasId,
    StructId, SubmoduleId, TraitFunctionId, TraitId, TraitItemId, TraitTypeId, UseId,
};
use cairo_lang_diagnostics::Maybe;

use crate::db::SemanticGroup;
use crate::expr::inference::InferenceId;
use crate::resolve::ResolverData;

pub trait HasResolverData {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;
}

pub trait LookupItemEx: HasResolverData {
    fn function_with_body(&self) -> Option<FunctionWithBodyId>;

    /// Returns the resolver data of the parent generic item if exist.
    fn resolver_context(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>>;
}

impl LookupItemEx for LookupItemId {
    fn function_with_body(&self) -> Option<FunctionWithBodyId> {
        match self {
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
                Some(FunctionWithBodyId::Free(*free_function_id))
            }
            LookupItemId::ImplItem(ImplItemId::Function(impl_function_id)) => {
                Some(FunctionWithBodyId::Impl(*impl_function_id))
            }
            _ => None,
        }
    }

    fn resolver_context(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            LookupItemId::ImplItem(impl_item_id) => {
                let impl_def_id = impl_item_id.impl_def_id(db.upcast());
                let resolver_data = impl_def_id.resolver_data(db.upcast())?;
                Ok(resolver_data)
            }
            LookupItemId::TraitItem(item) => {
                let trait_id = item.trait_id(db.upcast());
                let resolver_data = trait_id.resolver_data(db.upcast())?;
                Ok(resolver_data)
            }
            LookupItemId::ModuleItem(item) => {
                // Top level does not have an outer context, create an empty resolver data.
                let module_file_id = item.module_file_id(db.upcast());
                let resolver_data =
                    Arc::new(ResolverData::new(module_file_id, InferenceId::NoContext));
                Ok(resolver_data)
            }
        }
    }
}

impl HasResolverData for LookupItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            LookupItemId::ModuleItem(item) => item.resolver_data(db),
            LookupItemId::TraitItem(item) => item.resolver_data(db),
            LookupItemId::ImplItem(item) => item.resolver_data(db),
        }
    }
}

// === Module Items ===

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
        let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
            ModuleItemId::Submodule(*self),
        ));
        Ok(Arc::new(ResolverData::new(module_file_id, inference_id)))
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
        let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
            ModuleItemId::ExternType(*self),
        ));
        Ok(Arc::new(ResolverData::new(self.module_file_id(db.upcast()), inference_id)))
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
impl HasResolverData for ModuleTypeAliasId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.module_type_alias_resolver_data(*self)
    }
}

impl HasResolverData for TraitId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_resolver_data(*self)
    }
}

// === Trait Items ===

impl HasResolverData for TraitItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            TraitItemId::Function(item) => item.resolver_data(db),
            TraitItemId::Type(item) => item.resolver_data(db),
        }
    }
}

impl HasResolverData for TraitTypeId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_type_resolver_data(*self)
    }
}

impl HasResolverData for TraitFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.trait_function_resolver_data(*self)
    }
}

// === Impl Items ===

impl HasResolverData for ImplItemId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        match self {
            ImplItemId::Function(item) => item.resolver_data(db),
            ImplItemId::Type(item) => item.resolver_data(db),
        }
    }
}

impl HasResolverData for ImplTypeDefId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_type_def_resolver_data(*self)
    }
}

impl HasResolverData for ImplFunctionId {
    fn resolver_data(&self, db: &dyn SemanticGroup) -> Maybe<Arc<ResolverData>> {
        db.impl_function_resolver_data(*self)
    }
}
