use std::ops::Shl;

use cairo_lang_utils::casts::IntoOrPanic;
use itertools::{chain, repeat_n};
use num_bigint::BigInt;
use num_traits::One;
use starknet_types_core::felt::CAIRO_PRIME_BIGINT;

use super::bounded_int::BoundedIntType;
use super::boxing::box_ty;
use super::bytes31::Bytes31Type;
use super::int::signed::{Sint8Type, Sint16Type, Sint32Type, Sint64Type};
use super::int::signed128::Sint128Type;
use super::int::unsigned::{Uint8Type, Uint16Type, Uint32Type, Uint64Type};
use super::int::unsigned128::Uint128Type;
use super::snapshot::{SnapshotType, snapshot_ty};
use super::structure::StructType;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type,
};
use crate::ids::{ConcreteTypeId, UserTypeId};
use crate::program::GenericArg;

/// Returns a libfunc signature that casts from one type to another, without changing the internal
/// representation.
/// The implementation must be the identity function (no CASM output).
pub fn reinterpret_cast_signature(
    from_ty: ConcreteTypeId,
    to_ty: ConcreteTypeId,
) -> LibfuncSignature {
    LibfuncSignature::new_non_branch_ex(
        vec![ParamSignature::new(from_ty).with_allow_all()],
        vec![OutputVarInfo {
            ty: to_ty,
            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
        }],
        SierraApChange::Known { new_vars_only: true },
    )
}

/// A range of integers (`[lower, upper)`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Range {
    /// The lower bound (Inclusive).
    pub lower: BigInt,
    /// The upper bound (Exclusive).
    pub upper: BigInt,
}
impl Range {
    /// Creates a closed range i.e. `[lower, upper]`.
    pub fn closed(lower: impl Into<BigInt>, upper: impl Into<BigInt>) -> Self {
        Self::half_open(lower, upper.into() as BigInt + 1)
    }
    /// Creates a half-closed range i.e. `[lower, upper)`.
    pub fn half_open(lower: impl Into<BigInt>, upper: impl Into<BigInt>) -> Self {
        let result = Self { lower: lower.into(), upper: upper.into() };
        assert!(result.lower < result.upper, "Invalid range: {result:?}");
        result
    }
    /// Returns the [Range] bounds from the given type info.
    pub fn from_type_info(ty_info: &TypeInfo) -> Result<Self, SpecializationError> {
        Ok(match (&ty_info.long_id.generic_id, &ty_info.long_id.generic_args[..]) {
            (id, []) if *id == Felt252Type::id() => {
                let prime: BigInt = CAIRO_PRIME_BIGINT.clone();
                Self::half_open(1 - &prime, prime)
            }
            (id, []) if *id == Uint8Type::id() => Self::closed(u8::MIN, u8::MAX),
            (id, []) if *id == Uint16Type::id() => Self::closed(u16::MIN, u16::MAX),
            (id, []) if *id == Uint32Type::id() => Self::closed(u32::MIN, u32::MAX),
            (id, []) if *id == Uint64Type::id() => Self::closed(u64::MIN, u64::MAX),
            (id, []) if *id == Uint128Type::id() => Self::closed(u128::MIN, u128::MAX),
            (id, []) if *id == Sint8Type::id() => Self::closed(i8::MIN, i8::MAX),
            (id, []) if *id == Sint16Type::id() => Self::closed(i16::MIN, i16::MAX),
            (id, []) if *id == Sint32Type::id() => Self::closed(i32::MIN, i32::MAX),
            (id, []) if *id == Sint64Type::id() => Self::closed(i64::MIN, i64::MAX),
            (id, []) if *id == Sint128Type::id() => Self::closed(i128::MIN, i128::MAX),
            (id, []) if *id == Bytes31Type::id() => Self::half_open(0, BigInt::one().shl(248)),
            (id, [GenericArg::Value(min), GenericArg::Value(max)])
                if *id == BoundedIntType::id() =>
            {
                Self::closed(min.clone(), max.clone())
            }
            _ => return Err(SpecializationError::UnsupportedGenericArg),
        })
    }
    /// Returns the Range bounds from the given type.
    pub fn from_type(
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<Self, SpecializationError> {
        Self::from_type_info(&context.get_type_info(ty)?)
    }
    /// Returns true if this range is smaller than the RangeCheck range.
    pub fn is_small_range(&self) -> bool {
        self.size() <= BigInt::one().shl(128)
    }
    /// Returns true if this range can contain all possible values of a CASM cell.
    pub fn is_full_felt252_range(&self) -> bool {
        self.size() >= *CAIRO_PRIME_BIGINT
    }
    /// Returns the size of the range.
    pub fn size(&self) -> BigInt {
        &self.upper - &self.lower
    }
    /// Returns the intersection of `self` and `other`.
    ///
    /// If the intersection is empty, returns `None`.
    pub fn intersection(&self, other: &Self) -> Option<Self> {
        let lower = std::cmp::max(&self.lower, &other.lower).clone();
        let upper = std::cmp::min(&self.upper, &other.upper).clone();
        if lower < upper { Some(Self::half_open(lower, upper)) } else { None }
    }
}

/// Returns a fixed type array of the given type and size.
pub fn fixed_size_array_ty(
    context: &dyn SignatureSpecializationContext,
    ty: ConcreteTypeId,
    size: i16,
) -> Result<ConcreteTypeId, SpecializationError> {
    let args: Vec<GenericArg> = chain!(
        [GenericArg::UserType(UserTypeId::from_string("Tuple"))],
        repeat_n(GenericArg::Type(ty), size.into_or_panic())
    )
    .collect();
    context.get_concrete_type(StructType::id(), &args)
}

/// Trait for implementing a library function that unpacks a boxed composite type
/// (struct or enum) into boxed components (members or variants).
pub trait BoxUnpackLibfunc: Default {
    const STR_ID: &'static str;
    type Concrete: SignatureBasedConcreteLibfunc;

    /// Extracts the component types from an unwrapped composite type.
    fn extract_components(
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<Vec<ConcreteTypeId>, SpecializationError>;

    /// Creates the libfunc signature for the boxed unpack operation.
    fn create_signature(
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
        components: Vec<ConcreteTypeId>,
        is_snapshot: bool,
    ) -> Result<LibfuncSignature, SpecializationError>;

    /// Creates the concrete libfunc instance.
    fn create_concrete(
        components: Vec<ConcreteTypeId>,
        signature: LibfuncSignature,
    ) -> Self::Concrete;

    /// Analyzes the type to extract components and snapshot status.
    fn analyze_type(
        context: &dyn SignatureSpecializationContext,
        ty: ConcreteTypeId,
    ) -> Result<(Vec<ConcreteTypeId>, bool), SpecializationError> {
        // Unwrap snapshot if present
        let type_info = context.get_type_info(ty.clone())?;
        let is_snapshot = type_info.long_id.generic_id == SnapshotType::id();
        let unwrapped_ty = if is_snapshot {
            match &type_info.long_id.generic_args[0] {
                GenericArg::Type(ty) => ty.clone(),
                _ => return Err(SpecializationError::UnsupportedGenericArg),
            }
        } else {
            ty
        };

        let components = Self::extract_components(context, unwrapped_ty)?;
        Ok((components, is_snapshot))
    }
}

/// Helper to create a boxed output variable for unpack operations.
pub fn create_boxed_output(
    context: &dyn SignatureSpecializationContext,
    component_ty: ConcreteTypeId,
    is_snapshot: bool,
) -> Result<OutputVarInfo, SpecializationError> {
    let inner_type = if is_snapshot { snapshot_ty(context, component_ty)? } else { component_ty };
    Ok(OutputVarInfo {
        ty: box_ty(context, inner_type)?,
        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
    })
}

/// Wrapper to prevent implementation collisions for `NamedLibfunc`.
#[derive(Default)]
pub struct WrapBoxUnpackLibfunc<T: BoxUnpackLibfunc>(T);

impl<T: BoxUnpackLibfunc> NamedLibfunc for WrapBoxUnpackLibfunc<T> {
    type Concrete = T::Concrete;
    const STR_ID: &'static str = <T as BoxUnpackLibfunc>::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let (components, is_snapshot) = T::analyze_type(context, ty.clone())?;
        T::create_signature(context, ty, components, is_snapshot)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let (components, is_snapshot) = T::analyze_type(context, ty.clone())?;
        let signature = T::create_signature(context, ty, components.clone(), is_snapshot)?;
        Ok(T::create_concrete(components, signature))
    }
}
