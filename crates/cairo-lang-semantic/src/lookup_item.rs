use std::sync::Arc;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, FunctionWithBodyId,
    ImplAliasId, ImplConstantDefId, ImplDefId, ImplFunctionId, ImplImplDefId, ImplItemId,
    ImplTypeDefId, LanguageElementId, LookupItemId, MacroDeclarationId, ModuleId, ModuleItemId,
    ModuleTypeAliasId, StructId, SubmoduleId, TraitConstantId, TraitFunctionId, TraitId,
    TraitImplId, TraitItemId, TraitTypeId, UseId,
};
use cairo_lang_diagnostics::Maybe;
use salsa::Database;

use crate::expr::inference::InferenceId;
use crate::items::constant::ConstantSemantic;
use crate::items::enm::EnumSemantic;
use crate::items::extern_function::ExternFunctionSemantic;
use crate::items::free_function::FreeFunctionSemantic;
use crate::items::imp::ImplSemantic;
use crate::items::impl_alias::ImplAliasSemantic;
use crate::items::macro_declaration::MacroDeclarationSemantic;
use crate::items::module_type_alias::ModuleTypeAliasSemantic;
use crate::items::structure::StructSemantic;
use crate::items::trt::TraitSemantic;
use crate::items::us::UseSemantic;
use crate::resolve::ResolverData;

pub trait HasResolverData<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>>;
}

pub trait LookupItemEx<'db>: HasResolverData<'db> {
    fn function_with_body(&self) -> Option<FunctionWithBodyId<'db>>;

    /// Returns the resolver data of the parent generic item if exist.
    fn resolver_context(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>>;
}

impl<'db> LookupItemEx<'db> for LookupItemId<'db> {
    fn function_with_body(&self) -> Option<FunctionWithBodyId<'db>> {
        match self {
            LookupItemId::ModuleItem(ModuleItemId::FreeFunction(free_function_id)) => {
                Some(FunctionWithBodyId::Free(*free_function_id))
            }
            LookupItemId::TraitItem(TraitItemId::Function(trait_function_id)) => {
                Some(FunctionWithBodyId::Trait(*trait_function_id))
            }
            LookupItemId::ImplItem(ImplItemId::Function(impl_function_id)) => {
                Some(FunctionWithBodyId::Impl(*impl_function_id))
            }
            _ => None,
        }
    }

    fn resolver_context(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        match self {
            LookupItemId::ImplItem(impl_item_id) => {
                let impl_def_id: ImplDefId<'db> = impl_item_id.impl_def_id(db);
                let resolver_data = impl_def_id.resolver_data(db)?;
                Ok(resolver_data)
            }
            LookupItemId::TraitItem(item) => {
                let trait_id = item.trait_id(db);
                let resolver_data = trait_id.resolver_data(db)?;
                Ok(resolver_data)
            }
            LookupItemId::ModuleItem(item) => {
                // Top level does not have an outer context, create an empty resolver data.
                let module_id = item.module_id(db);
                let resolver_data = Arc::new(ResolverData::new(module_id, InferenceId::NoContext));
                Ok(resolver_data)
            }
        }
    }
}

impl<'db> HasResolverData<'db> for LookupItemId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        match self {
            LookupItemId::ModuleItem(item) => item.resolver_data(db),
            LookupItemId::TraitItem(item) => item.resolver_data(db),
            LookupItemId::ImplItem(item) => item.resolver_data(db),
        }
    }
}

// === Module Items ===

impl<'db> HasResolverData<'db> for ModuleItemId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
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
            ModuleItemId::MacroDeclaration(item) => item.resolver_data(db),
        }
    }
}

impl<'db> HasResolverData<'db> for ConstantId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.constant_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for SubmoduleId<'db> {
    fn resolver_data(&self, _db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        let module_id = ModuleId::Submodule(*self);
        let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
            ModuleItemId::Submodule(*self),
        ));
        Ok(Arc::new(ResolverData::new(module_id, inference_id)))
    }
}

impl<'db> HasResolverData<'db> for UseId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.use_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ImplAliasId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_alias_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ImplDefId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_def_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ExternTypeId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
            ModuleItemId::ExternType(*self),
        ));
        Ok(Arc::new(ResolverData::new(self.module_id(db), inference_id)))
    }
}

impl<'db> HasResolverData<'db> for ExternFunctionId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.extern_function_declaration_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for FreeFunctionId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.free_function_declaration_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for StructId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.struct_declaration_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for EnumId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.enum_declaration_resolver_data(*self)
    }
}
impl<'db> HasResolverData<'db> for ModuleTypeAliasId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.module_type_alias_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for TraitId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.trait_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for MacroDeclarationId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.macro_declaration_resolver_data(*self)
    }
}

// === Trait Items ===

impl<'db> HasResolverData<'db> for TraitItemId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        match self {
            TraitItemId::Function(item) => item.resolver_data(db),
            TraitItemId::Type(item) => item.resolver_data(db),
            TraitItemId::Constant(item) => item.resolver_data(db),
            TraitItemId::Impl(item) => item.resolver_data(db),
        }
    }
}

impl<'db> HasResolverData<'db> for TraitTypeId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.trait_type_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for TraitConstantId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.trait_constant_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for TraitImplId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.trait_impl_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for TraitFunctionId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.trait_function_resolver_data(*self)
    }
}

// === Impl Items ===

impl<'db> HasResolverData<'db> for ImplItemId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        match self {
            ImplItemId::Function(item) => item.resolver_data(db),
            ImplItemId::Type(item) => item.resolver_data(db),
            ImplItemId::Constant(item) => item.resolver_data(db),
            ImplItemId::Impl(item) => item.resolver_data(db),
        }
    }
}

impl<'db> HasResolverData<'db> for ImplTypeDefId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_type_def_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ImplConstantDefId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_constant_def_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ImplImplDefId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_impl_def_resolver_data(*self)
    }
}

impl<'db> HasResolverData<'db> for ImplFunctionId<'db> {
    fn resolver_data(&self, db: &'db dyn Database) -> Maybe<Arc<ResolverData<'db>>> {
        db.impl_function_resolver_data(*self)
    }
}
