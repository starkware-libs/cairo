//! A total order on inference-free semantic objects that is consistent across builds: unlike the
//! order of their ids, it depends only on the definitions they are built from and the values of
//! their constants.

use std::cmp::Ordering;

use cairo_lang_defs::ids::{FunctionTitleId, LanguageElementId};
use cairo_lang_defs::stable_order::{cmp_elements, cmp_nodes};
use salsa::Database;

use crate::items::constant::{ConstValue, ConstValueId};
use crate::items::enm::ConcreteVariant;
use crate::items::functions::{
    ConcreteFunctionWithBodyId, GenericFunctionId, GenericFunctionWithBodyId,
};
use crate::items::imp::{
    GeneratedImplItems, ImplId, ImplLongId, NegativeImplId, NegativeImplLongId, UninferredImpl,
};
use crate::items::trt::ConcreteTraitId;
use crate::types::{TypeId, TypeLongId};
use crate::{FunctionId, GenericArgumentId};

/// A semantic object with a place in the build-independent order.
pub trait StableOrd<'db> {
    /// Compares two objects of the same type; distinct inference-free objects never compare equal.
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering;
}

/// Compares two sequences lexicographically.
pub fn cmp_slices<'db, T: StableOrd<'db>>(db: &'db dyn Database, a: &[T], b: &[T]) -> Ordering {
    a.iter()
        .zip(b)
        .map(|(a, b)| a.stable_cmp(b, db))
        .find(|ordering| ordering.is_ne())
        .unwrap_or(a.len().cmp(&b.len()))
}

/// Compares two generic definitions applied to generic arguments.
fn cmp_concrete<'db>(
    db: &'db dyn Database,
    (a, a_args): (&impl LanguageElementId<'db>, &[GenericArgumentId<'db>]),
    (b, b_args): (&impl LanguageElementId<'db>, &[GenericArgumentId<'db>]),
) -> Ordering {
    cmp_elements(db, a, b).then_with(|| cmp_slices(db, a_args, b_args))
}

/// `Ok` values precede errors, which are all equal.
impl<'db, T: StableOrd<'db>, E> StableOrd<'db> for Result<T, E> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        match (self, other) {
            (Ok(a), Ok(b)) => a.stable_cmp(b, db),
            (Ok(_), Err(_)) => Ordering::Less,
            (Err(_), Ok(_)) => Ordering::Greater,
            (Err(_), Err(_)) => Ordering::Equal,
        }
    }
}

impl<'db> StableOrd<'db> for GenericArgumentId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        let kind = |arg: &Self| match arg {
            Self::Type(_) => 0,
            Self::Constant(_) => 1,
            Self::Impl(_) => 2,
            Self::NegImpl(_) => 3,
        };
        match (self, other) {
            (Self::Type(a), Self::Type(b)) => a.stable_cmp(b, db),
            (Self::Constant(a), Self::Constant(b)) => a.stable_cmp(b, db),
            (Self::Impl(a), Self::Impl(b)) => a.stable_cmp(b, db),
            (Self::NegImpl(a), Self::NegImpl(b)) => a.stable_cmp(b, db),
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}

impl<'db> StableOrd<'db> for TypeId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use TypeLongId::*;
        let kind = |ty: &TypeLongId<'db>| match ty {
            Concrete(_) => 0,
            Tuple(_) => 1,
            Snapshot(_) => 2,
            GenericParameter(_) => 3,
            Var(_) => 4,
            NumericLiteral(_) => 5,
            Coupon(_) => 6,
            FixedSizeArray { .. } => 7,
            ImplType(_) => 8,
            Closure(_) => 9,
            Missing(_) => 10,
        };
        match (self.long(db), other.long(db)) {
            (Concrete(a), Concrete(b)) => {
                cmp_elements(db, &a.generic_type(db), &b.generic_type(db))
                    .then_with(|| cmp_slices(db, &a.generic_args(db), &b.generic_args(db)))
            }
            (Tuple(a), Tuple(b)) => cmp_slices(db, a, b),
            (Snapshot(a), Snapshot(b)) => a.stable_cmp(b, db),
            (GenericParameter(a), GenericParameter(b)) => cmp_elements(db, a, b),
            (Var(a), Var(b)) | (NumericLiteral(a), NumericLiteral(b)) => a.id.0.cmp(&b.id.0),
            (Coupon(a), Coupon(b)) => a.stable_cmp(b, db),
            (
                FixedSizeArray { type_id: a, size: a_size },
                FixedSizeArray { type_id: b, size: b_size },
            ) => a.stable_cmp(b, db).then_with(|| a_size.stable_cmp(b_size, db)),
            (ImplType(a), ImplType(b)) => a
                .impl_id()
                .stable_cmp(&b.impl_id(), db)
                .then_with(|| cmp_elements(db, &a.ty(), &b.ty())),
            (Closure(a), Closure(b)) => {
                cmp_nodes(db, a.params_location.stable_ptr(), b.params_location.stable_ptr())
                    .then_with(|| a.parent_function.stable_cmp(&b.parent_function, db))
            }
            (Missing(_), Missing(_)) => Ordering::Equal,
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}

impl<'db> StableOrd<'db> for ConstValueId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use ConstValue::*;
        let kind = |value: &ConstValue<'db>| match value {
            Int(..) => 0,
            Struct(..) => 1,
            Enum(..) => 2,
            NonZero(_) => 3,
            Generic(_) => 4,
            ImplConstant(_) => 5,
            Var(..) => 6,
            Missing(_) => 7,
        };
        match (self.long(db), other.long(db)) {
            (Int(a, a_ty), Int(b, b_ty)) => a.cmp(b).then_with(|| a_ty.stable_cmp(b_ty, db)),
            (Struct(a, a_ty), Struct(b, b_ty)) => {
                a_ty.stable_cmp(b_ty, db).then_with(|| cmp_slices(db, a, b))
            }
            (Enum(a, a_payload), Enum(b, b_payload)) => {
                a.stable_cmp(b, db).then_with(|| a_payload.stable_cmp(b_payload, db))
            }
            (NonZero(a), NonZero(b)) => a.stable_cmp(b, db),
            (Generic(a), Generic(b)) => cmp_elements(db, a, b),
            (ImplConstant(a), ImplConstant(b)) => a
                .impl_id()
                .stable_cmp(&b.impl_id(), db)
                .then_with(|| cmp_elements(db, &a.trait_constant_id(), &b.trait_constant_id())),
            (Var(a, a_ty), Var(b, b_ty)) => {
                a.id.0.cmp(&b.id.0).then_with(|| a_ty.stable_cmp(b_ty, db))
            }
            (Missing(_), Missing(_)) => Ordering::Equal,
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}

impl<'db> StableOrd<'db> for ConcreteVariant<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        cmp_concrete(
            db,
            (&self.concrete_enum_id.enum_id(db), &self.concrete_enum_id.long(db).generic_args),
            (&other.concrete_enum_id.enum_id(db), &other.concrete_enum_id.long(db).generic_args),
        )
        .then_with(|| cmp_elements(db, &self.id, &other.id))
    }
}

impl<'db> StableOrd<'db> for ImplId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use ImplLongId::*;
        let kind = |imp: &ImplLongId<'db>| match imp {
            Concrete(_) => 0,
            GenericParameter(_) => 1,
            ImplVar(_) => 2,
            ImplImpl(_) => 3,
            SelfImpl(_) => 4,
            GeneratedImpl(_) => 5,
        };
        match (self.long(db), other.long(db)) {
            (Concrete(a), Concrete(b)) => {
                let (a, b) = (a.long(db), b.long(db));
                cmp_concrete(
                    db,
                    (&a.impl_def_id, &a.generic_args),
                    (&b.impl_def_id, &b.generic_args),
                )
            }
            (GenericParameter(a), GenericParameter(b)) => cmp_elements(db, a, b),
            (ImplVar(a), ImplVar(b)) => a.long(db).id.0.cmp(&b.long(db).id.0),
            (ImplImpl(a), ImplImpl(b)) => a
                .impl_id()
                .stable_cmp(&b.impl_id(), db)
                .then_with(|| cmp_elements(db, &a.trait_impl_id(), &b.trait_impl_id())),
            (SelfImpl(a), SelfImpl(b)) => a.stable_cmp(b, db),
            (GeneratedImpl(a), GeneratedImpl(b)) => {
                let (a, b) = (a.long(db), b.long(db));
                a.concrete_trait
                    .stable_cmp(&b.concrete_trait, db)
                    .then_with(|| a.impl_items.stable_cmp(&b.impl_items, db))
            }
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}

impl<'db> StableOrd<'db> for NegativeImplId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use NegativeImplLongId::*;
        let kind = |imp: &NegativeImplLongId<'db>| match imp {
            Solved(_) => 0,
            GenericParameter(_) => 1,
            NegativeImplVar(_) => 2,
        };
        match (self.long(db), other.long(db)) {
            (Solved(a), Solved(b)) => a.stable_cmp(b, db),
            (GenericParameter(a), GenericParameter(b)) => cmp_elements(db, a, b),
            (NegativeImplVar(a), NegativeImplVar(b)) => a.long(db).id.0.cmp(&b.long(db).id.0),
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}

/// Compared item by item, as a sequence of trait type and value pairs.
impl<'db> StableOrd<'db> for GeneratedImplItems<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        self.0
            .iter()
            .zip(other.0.iter())
            .map(|((a_ty, a), (b_ty, b))| {
                cmp_elements(db, a_ty, b_ty).then_with(|| a.stable_cmp(b, db))
            })
            .find(|ordering| ordering.is_ne())
            .unwrap_or(self.0.len().cmp(&other.0.len()))
    }
}

impl<'db> StableOrd<'db> for ConcreteTraitId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        let (a, b) = (self.long(db), other.long(db));
        cmp_concrete(db, (&a.trait_id, &a.generic_args), (&b.trait_id, &b.generic_args))
    }
}

/// Functions are ordered by the position of their definition first (the trait function for impl
/// functions), so that the earliest defined function of a group comes first regardless of kind.
impl<'db> StableOrd<'db> for FunctionId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use GenericFunctionId::*;
        let (a, b) = (&self.long(db).function, &other.long(db).function);
        let title = |function: &GenericFunctionId<'db>| match function {
            Free(id) => (FunctionTitleId::Free(*id), 0),
            Extern(id) => (FunctionTitleId::Extern(*id), 1),
            Impl(id) => (FunctionTitleId::Trait(id.function), 2),
        };
        let ((a_title, a_kind), (b_title, b_kind)) =
            (title(&a.generic_function), title(&b.generic_function));
        cmp_elements(db, &a_title, &b_title)
            .then_with(|| a_kind.cmp(&b_kind))
            .then_with(|| match (&a.generic_function, &b.generic_function) {
                (Impl(a), Impl(b)) => a.impl_id.stable_cmp(&b.impl_id, db),
                _ => Ordering::Equal,
            })
            .then_with(|| cmp_slices(db, &a.generic_args, &b.generic_args))
    }
}

/// Ordered like `FunctionId`, by the impl function's definition where it has one.
impl<'db> StableOrd<'db> for ConcreteFunctionWithBodyId<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use GenericFunctionWithBodyId::*;
        let (a, b) = (self.long(db), other.long(db));
        let kind = |function: &GenericFunctionWithBodyId<'db>| match function {
            Free(_) => 0,
            Impl(_) => 1,
            Trait(_) => 2,
        };
        cmp_elements(db, &a.function_with_body_id(db), &b.function_with_body_id(db))
            .then_with(|| kind(&a.generic_function).cmp(&kind(&b.generic_function)))
            .then_with(|| match (&a.generic_function, &b.generic_function) {
                (Impl(a), Impl(b)) => {
                    let (a, b) = (a.concrete_impl_id.long(db), b.concrete_impl_id.long(db));
                    cmp_concrete(
                        db,
                        (&a.impl_def_id, &a.generic_args),
                        (&b.impl_def_id, &b.generic_args),
                    )
                }
                (Trait(a), Trait(b)) => a.concrete_trait(db).stable_cmp(&b.concrete_trait(db), db),
                _ => Ordering::Equal,
            })
            .then_with(|| cmp_slices(db, &a.generic_args, &b.generic_args))
    }
}

impl<'db> StableOrd<'db> for UninferredImpl<'db> {
    fn stable_cmp(&self, other: &Self, db: &'db dyn Database) -> Ordering {
        use UninferredImpl::*;
        let kind = |imp: &Self| match imp {
            Def(_) => 0,
            ImplAlias(_) => 1,
            GenericParam(_) => 2,
            ImplImpl(_) => 3,
            GeneratedImpl(_) => 4,
        };
        match (self, other) {
            (Def(a), Def(b)) => cmp_elements(db, a, b),
            (ImplAlias(a), ImplAlias(b)) => cmp_elements(db, a, b),
            (GenericParam(a), GenericParam(b)) => cmp_elements(db, a, b),
            (ImplImpl(a), ImplImpl(b)) => a
                .impl_id()
                .stable_cmp(&b.impl_id(), db)
                .then_with(|| cmp_elements(db, &a.trait_impl_id(), &b.trait_impl_id())),
            (GeneratedImpl(a), GeneratedImpl(b)) => {
                let (a, b) = (a.long(db), b.long(db));
                a.concrete_trait
                    .stable_cmp(&b.concrete_trait, db)
                    .then_with(|| a.impl_items.stable_cmp(&b.impl_items, db))
            }
            (a, b) => kind(a).cmp(&kind(b)),
        }
    }
}
