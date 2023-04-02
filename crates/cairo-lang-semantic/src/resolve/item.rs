use cairo_lang_defs::ids::{
    ConstantId, GenericTypeId, ImplDefId, ModuleId, TraitFunctionId, TraitId, TypeAliasId,
};
use cairo_lang_proc_macros::DebugWithDb;

use crate::db::SemanticGroup;
use crate::items::functions::GenericFunctionId;
use crate::items::imp::ImplId;
use crate::items::trt::ConcreteTraitGenericFunctionId;
use crate::{ConcreteTraitId, ConcreteVariant, FunctionId, TypeId, TypeLongId, Variant};

// Resolved items:
// ResolvedConcreteItem - returned by resolve_concrete_path(). Paths with generic arguments.
// ResolvedGenericItem - returned by resolve_generic_path(). Paths without generic arguments.
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
#[derive(Clone, PartialEq, Eq, Debug, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub enum ResolvedGenericItem {
    Constant(ConstantId),
    Module(ModuleId),
    GenericFunction(GenericFunctionId),
    TraitFunction(TraitFunctionId),
    GenericType(GenericTypeId),
    GenericTypeAlias(TypeAliasId),
    Variant(Variant),
    Trait(TraitId),
    Impl(ImplDefId),
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
