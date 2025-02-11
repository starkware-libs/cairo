use cairo_lang_defs::ids::{
    ConstantId, GenericTypeId, ImplAliasId, ImplDefId, ImplItemId, LookupItemId, ModuleId,
    ModuleItemId, ModuleTypeAliasId, TopLevelLanguageElementId, TraitId, TraitItemId, VarId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};

use crate::db::SemanticGroup;
use crate::items::constant::ConstValueId;
use crate::items::functions::{GenericFunctionId, ImplGenericFunctionId};
use crate::items::imp::{ImplId, ImplLongId};
use crate::items::us::SemanticUseEx;
use crate::{
    ConcreteImplLongId, ConcreteTraitId, ConcreteVariant, FunctionId, TypeId, TypeLongId, Variant,
};

// Resolved items:
// ResolvedConcreteItem - returned by resolve_concrete_path(). Paths with generic arguments.
// ResolvedGenericItem - returned by resolve_generic_path(). Paths without generic arguments.

#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedGenericItem {
    GenericConstant(ConstantId),
    Module(ModuleId),
    GenericFunction(GenericFunctionId),
    GenericType(GenericTypeId),
    GenericTypeAlias(ModuleTypeAliasId),
    GenericImplAlias(ImplAliasId),
    Variant(Variant),
    Trait(TraitId),
    Impl(ImplDefId),
    Variable(VarId),
}
impl ResolvedGenericItem {
    /// Wraps a [`ModuleItemId`] with the corresponding [`ResolvedGenericItem`].
    pub fn from_module_item(db: &dyn SemanticGroup, module_item: ModuleItemId) -> Maybe<Self> {
        Ok(match module_item {
            ModuleItemId::Constant(id) => Self::GenericConstant(id),
            ModuleItemId::Submodule(id) => Self::Module(ModuleId::Submodule(id)),
            ModuleItemId::Use(id) => {
                // Note that `use_resolved_item` needs to be called before
                // `use_semantic_diagnostics` to handle cycles.
                db.use_resolved_item(id)?
            }
            ModuleItemId::FreeFunction(id) => Self::GenericFunction(GenericFunctionId::Free(id)),
            ModuleItemId::ExternFunction(id) => {
                Self::GenericFunction(GenericFunctionId::Extern(id))
            }
            ModuleItemId::Struct(id) => Self::GenericType(GenericTypeId::Struct(id)),
            ModuleItemId::Enum(id) => Self::GenericType(GenericTypeId::Enum(id)),
            ModuleItemId::TypeAlias(id) => Self::GenericTypeAlias(id),
            ModuleItemId::ImplAlias(id) => Self::GenericImplAlias(id),
            ModuleItemId::ExternType(id) => Self::GenericType(GenericTypeId::Extern(id)),
            ModuleItemId::Trait(id) => Self::Trait(id),
            ModuleItemId::Impl(id) => Self::Impl(id),
        })
    }

    /// Wraps a [`TraitItemId`] into the corresponding [`ResolvedGenericItem`].
    pub fn from_trait_item(db: &dyn SemanticGroup, trait_item: TraitItemId) -> Maybe<Self> {
        // if let TraitItemId::Function(trait_function_id) = trait_item {
        //     let parent_trait = trait_item.trait_id(db);
        //     let generic_parameters = db.trait_generic_params(parent_trait).to_option()?;
        //     let concrete_trait = ConcreteTraitLongId {
        //         trait_id: parent_trait,
        //         generic_args: generic_params_to_args(&generic_parameters, db),
        //     };
        //     let concrete_trait = db.intern_concrete_trait(concrete_trait);
        //
        //     ResolvedGenericItem::GenericFunction(GenericFunctionId::Impl(ImplGenericFunctionId {
        //         impl_id: ImplLongId::SelfImpl(concrete_trait).intern(db),
        //         function: trait_function_id,
        //     }))
        // } else {
        //     ResolvedGenericItem::Trait(trait_item.trait_id(db))
        // }
        todo!()
    }

    /// Wraps an [`ImplItemId`] into the corresponding [`ResolvedGenericItem`].
    pub fn from_impl_item(db: &dyn SemanticGroup, impl_item: ImplItemId) -> Maybe<Self> {
        Ok(match impl_item {
            ImplItemId::Function(impl_function) => {
                let impl_def_id = impl_function.impl_def_id(db.upcast());
                let impl_id = ImplLongId::Concrete(
                    ConcreteImplLongId { impl_def_id, generic_args: vec![] }.intern(db.upcast()),
                )
                .intern(db.upcast());
                let function;

                Self::GenericFunction(GenericFunctionId::Impl(ImplGenericFunctionId {
                    impl_id,
                    function,
                }))
            }
            ImplItemId::Type(impl_type) => {}
            ImplItemId::Constant(impl_const) => {}
            ImplItemId::Impl(impl_impl) => {}
        })
    }

    /// Re-wraps a [`LookupItemId`] into the corresponding [`ResolvedGenericItem`].
    ///
    /// This method is used by CairoLS.
    pub fn from_lookup_item(db: &dyn SemanticGroup, lookup_item: LookupItemId) -> Maybe<Self> {
        match lookup_item {
            LookupItemId::ModuleItem(module_item) => Self::from_module_item(db, module_item),
            LookupItemId::TraitItem(trait_item) => Self::from_trait_item(db, trait_item),
            LookupItemId::ImplItem(impl_item) => Self::from_impl_item(db, impl_item),
        }
    }

    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        let defs_db = db.upcast();
        match self {
            ResolvedGenericItem::GenericConstant(_) => "".into(),
            ResolvedGenericItem::Module(id) => id.full_path(defs_db),
            ResolvedGenericItem::GenericFunction(id) => id.format(db),
            ResolvedGenericItem::GenericType(id) => id.full_path(defs_db),
            ResolvedGenericItem::GenericTypeAlias(id) => id.full_path(defs_db),
            ResolvedGenericItem::GenericImplAlias(id) => id.full_path(defs_db),
            ResolvedGenericItem::Variant(id) => id.id.full_path(defs_db),
            ResolvedGenericItem::Trait(id) => id.full_path(defs_db),
            ResolvedGenericItem::Impl(id) => id.full_path(defs_db),
            ResolvedGenericItem::Variable(_) => "".into(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedConcreteItem {
    Constant(ConstValueId),
    Module(ModuleId),
    Function(FunctionId),
    Type(TypeId),
    Variant(ConcreteVariant),
    Trait(ConcreteTraitId),
    SelfTrait(ConcreteTraitId),
    Impl(ImplId),
}

impl ResolvedConcreteItem {
    pub fn generic(&self, db: &dyn SemanticGroup) -> Option<ResolvedGenericItem> {
        Some(match self {
            ResolvedConcreteItem::Constant(_) => return None,
            ResolvedConcreteItem::Module(item) => ResolvedGenericItem::Module(*item),
            ResolvedConcreteItem::Function(function) => ResolvedGenericItem::GenericFunction(
                function.lookup_intern(db).function.generic_function,
            ),
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(concrete) = ty.lookup_intern(db) {
                    ResolvedGenericItem::GenericType(concrete.generic_type(db))
                } else {
                    return None;
                }
            }
            ResolvedConcreteItem::Variant(ConcreteVariant { concrete_enum_id, id, ty, idx }) => {
                ResolvedGenericItem::Variant(Variant {
                    enum_id: concrete_enum_id.enum_id(db),
                    id: *id,
                    ty: *ty,
                    idx: *idx,
                })
            }
            ResolvedConcreteItem::Trait(concrete_trait) => {
                ResolvedGenericItem::Trait(concrete_trait.lookup_intern(db).trait_id)
            }
            ResolvedConcreteItem::SelfTrait(concrete_trait_id) => {
                ResolvedGenericItem::Trait(concrete_trait_id.trait_id(db))
            }
            ResolvedConcreteItem::Impl(impl_id) => match impl_id.lookup_intern(db) {
                ImplLongId::Concrete(concrete_impl_id) => {
                    ResolvedGenericItem::Impl(concrete_impl_id.lookup_intern(db).impl_def_id)
                }
                ImplLongId::GenericParameter(_)
                | ImplLongId::ImplVar(_)
                | ImplLongId::ImplImpl(_)
                | ImplLongId::SelfImpl(_)
                | ImplLongId::GeneratedImpl(_) => return None,
            },
        })
    }
}
