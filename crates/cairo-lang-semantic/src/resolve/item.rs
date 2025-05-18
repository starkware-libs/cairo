use cairo_lang_defs::ids::{
    ConstantId, GenericTypeId, ImplAliasId, ImplDefId, MacroDeclarationId, ModuleId, ModuleItemId,
    ModuleTypeAliasId, TopLevelLanguageElementId, TraitId, TraitItemId, VarId,
};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_proc_macros::DebugWithDb;
use cairo_lang_utils::LookupIntern;

use crate::db::SemanticGroup;
use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::functions::GenericFunctionId;
use crate::items::imp::{ImplId, ImplLongId};
use crate::items::us::SemanticUseEx;
use crate::{ConcreteTraitId, ConcreteVariant, FunctionId, TypeId, TypeLongId, Variant};

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
    Macro(MacroDeclarationId),
    TraitItem(TraitItemId),
}
impl ResolvedGenericItem {
    /// Wraps a ModuleItem with the corresponding ResolveGenericItem.
    pub fn from_module_item(
        db: &dyn SemanticGroup,
        module_item: ModuleItemId,
    ) -> Maybe<ResolvedGenericItem> {
        Ok(match module_item {
            ModuleItemId::Constant(id) => ResolvedGenericItem::GenericConstant(id),
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
            ModuleItemId::MacroDeclaration(id) => ResolvedGenericItem::Macro(id),
        })
    }

    pub fn full_path(&self, db: &dyn SemanticGroup) -> String {
        match self {
            ResolvedGenericItem::GenericConstant(_) => "".into(),
            ResolvedGenericItem::Module(id) => id.full_path(db),
            ResolvedGenericItem::GenericFunction(id) => id.format(db),
            ResolvedGenericItem::GenericType(id) => id.full_path(db),
            ResolvedGenericItem::GenericTypeAlias(id) => id.full_path(db),
            ResolvedGenericItem::GenericImplAlias(id) => id.full_path(db),
            ResolvedGenericItem::Variant(id) => id.id.full_path(db),
            ResolvedGenericItem::Trait(id) => id.full_path(db),
            ResolvedGenericItem::Impl(id) => id.full_path(db),
            ResolvedGenericItem::Macro(id) => id.full_path(db),
            ResolvedGenericItem::Variable(_) => "".into(),
            ResolvedGenericItem::TraitItem(id) => id.full_path(db),
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
    Macro(MacroDeclarationId),
}

impl ResolvedConcreteItem {
    pub fn generic(&self, db: &dyn SemanticGroup) -> Option<ResolvedGenericItem> {
        Some(match self {
            ResolvedConcreteItem::Constant(id) => {
                if let ConstValue::ImplConstant(impl_constant_id) = id.lookup_intern(db) {
                    ResolvedGenericItem::TraitItem(TraitItemId::Constant(
                        impl_constant_id.trait_constant_id(),
                    ))
                } else {
                    return None;
                }
            }
            ResolvedConcreteItem::Module(item) => ResolvedGenericItem::Module(*item),
            ResolvedConcreteItem::Function(function) => ResolvedGenericItem::GenericFunction(
                function.lookup_intern(db).function.generic_function,
            ),
            ResolvedConcreteItem::Type(ty) => match ty.lookup_intern(db) {
                TypeLongId::Concrete(concrete) => {
                    ResolvedGenericItem::GenericType(concrete.generic_type(db))
                }
                TypeLongId::ImplType(impl_type_id) => {
                    ResolvedGenericItem::TraitItem(TraitItemId::Type(impl_type_id.ty()))
                }
                _ => return None,
            },
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
                ImplLongId::ImplImpl(impl_impl_id) => {
                    ResolvedGenericItem::TraitItem(TraitItemId::Impl(impl_impl_id.trait_impl_id()))
                }
                ImplLongId::SelfImpl(concrete_trait_id) => {
                    ResolvedGenericItem::Trait(concrete_trait_id.trait_id(db))
                }
                ImplLongId::GenericParameter(_)
                | ImplLongId::ImplVar(_)
                | ImplLongId::GeneratedImpl(_) => return None,
            },
            ResolvedConcreteItem::Macro(macro_declaration_id) => {
                ResolvedGenericItem::Macro(*macro_declaration_id)
            }
        })
    }
}
