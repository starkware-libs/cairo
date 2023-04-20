use cairo_lang_defs::ids::{
    ConstantId, GenericTypeId, ImplAliasId, ImplDefId, ModuleId, ModuleItemId, TraitFunctionId,
    TraitId, TypeAliasId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::items::functions::GenericFunctionId;
use crate::items::imp::ImplId;
use crate::items::trt::ConcreteTraitGenericFunctionId;
use crate::items::us::SemanticUseEx;
use crate::{ConcreteTraitId, ConcreteVariant, FunctionId, TypeId, TypeLongId, Variant};

// Resolved items:
// ResolvedConcreteItem - returned by resolve_concrete_path(). Paths with generic arguments.
// ResolvedGenericItem - returned by resolve_generic_path(). Paths without generic arguments.

#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedGenericItem {
    Constant(ConstantId),
    Module(ModuleId),
    GenericFunction(GenericFunctionId),
    TraitFunction(TraitFunctionId),
    GenericType(GenericTypeId),
    GenericTypeAlias(TypeAliasId),
    GenericImplAlias(ImplAliasId),
    Variant(Variant),
    Trait(TraitId),
    Impl(ImplDefId),
}
impl ResolvedGenericItem {
    /// Wraps a ModuleItem with the corresponding ResolveGenericItem.
    pub fn from_module_item(
        db: &dyn SemanticGroup,
        module_item: ModuleItemId,
    ) -> Maybe<ResolvedGenericItem> {
        Ok(match module_item {
            ModuleItemId::Constant(id) => ResolvedGenericItem::Constant(id),
            ModuleItemId::Submodule(id) => ResolvedGenericItem::Module(ModuleId::Submodule(id)),
            ModuleItemId::Use(id) => {
                // Note that `use_resolved_item` needs to be called before
                // `use_semantic_diagnostics` to handle cycles.
                db.use_resolved_item(id)?
            }
            ModuleItemId::FreeFunction(id) => {
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Free(id))
            }
            ModuleItemId::ExternFunction(id) => {
                ResolvedGenericItem::GenericFunction(GenericFunctionId::Extern(id))
            }
            ModuleItemId::Struct(id) => ResolvedGenericItem::GenericType(GenericTypeId::Struct(id)),
            ModuleItemId::Enum(id) => ResolvedGenericItem::GenericType(GenericTypeId::Enum(id)),
            ModuleItemId::TypeAlias(id) => ResolvedGenericItem::GenericTypeAlias(id),
            ModuleItemId::ImplAlias(id) => ResolvedGenericItem::GenericImplAlias(id),
            ModuleItemId::ExternType(id) => {
                ResolvedGenericItem::GenericType(GenericTypeId::Extern(id))
            }
            ModuleItemId::Trait(id) => ResolvedGenericItem::Trait(id),
            ModuleItemId::Impl(id) => ResolvedGenericItem::Impl(id),
        })
    }
}

#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedConcreteItem {
    Constant(ConstantId),
    Module(ModuleId),
    Function(FunctionId),
    TraitFunction(ConcreteTraitGenericFunctionId),
    Type(TypeId),
    Variant(ConcreteVariant),
    Trait(ConcreteTraitId),
    Impl(ImplId),
}
impl ResolvedConcreteItem {
    pub fn generic(&self, db: &dyn SemanticGroup) -> Option<ResolvedGenericItem> {
        Some(match self {
            ResolvedConcreteItem::Constant(id) => ResolvedGenericItem::Constant(*id),
            ResolvedConcreteItem::Module(item) => ResolvedGenericItem::Module(*item),
            ResolvedConcreteItem::Function(function) => ResolvedGenericItem::GenericFunction(
                db.lookup_intern_function(*function).function.generic_function,
            ),
            ResolvedConcreteItem::TraitFunction(trait_function) => {
                ResolvedGenericItem::TraitFunction(trait_function.function_id(db))
            }
            ResolvedConcreteItem::Type(ty) => {
                if let TypeLongId::Concrete(concrete) = db.lookup_intern_type(*ty) {
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
            ResolvedConcreteItem::Trait(concrete_trait) => ResolvedGenericItem::Trait(
                db.lookup_intern_concrete_trait(*concrete_trait).trait_id,
            ),
            ResolvedConcreteItem::Impl(impl_id) => match impl_id {
                ImplId::Concrete(concrete_impl_id) => ResolvedGenericItem::Impl(
                    db.lookup_intern_concrete_impl(*concrete_impl_id).impl_def_id,
                ),
                ImplId::GenericParameter(_) | ImplId::ImplVar(_) => return None,
            },
        })
    }
}
