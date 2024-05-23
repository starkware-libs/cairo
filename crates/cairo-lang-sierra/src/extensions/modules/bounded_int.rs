use std::ops::Shl;

use cairo_felt::Felt252;
use itertools::Itertools;
use num_bigint::{BigInt, ToBigInt};
use num_traits::{One, Signed};

use super::range_check::RangeCheckType;
use super::utils::Range;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureOnlyGenericLibfunc, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    args_as_two_types, ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};
use crate::program::GenericArg;

/// Type for BoundedInt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct BoundedIntType {}
impl NamedType for BoundedIntType {
    type Concrete = BoundedIntConcreteType;

    const ID: GenericTypeId = GenericTypeId::new_inline("BoundedInt");
    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (min, max) = match args {
            [GenericArg::Value(min), GenericArg::Value(max)] => (min.clone(), max.clone()),
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };

        let prime: BigInt = Felt252::prime().into();
        if !(-&prime < min && min <= max && max < prime && &max - &min < prime) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let long_id = Self::concrete_type_long_id(args);
        let ty_info = TypeInfo {
            long_id,
            zero_sized: false,
            storable: true,
            droppable: true,
            duplicatable: true,
        };

        Ok(Self::Concrete { info: ty_info, range: Range::closed(min, max) })
    }
}

pub struct BoundedIntConcreteType {
    pub info: TypeInfo,
    /// The range bounds for a value of this type.
    pub range: Range,
}
impl ConcreteType for BoundedIntConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum BoundedIntLibfunc {
        Add(BoundedIntAddLibfunc),
        Sub(BoundedIntSubLibfunc),
        Mul(BoundedIntMulLibfunc),
        DivRem(BoundedIntDivRemLibfunc),
    }, BoundedIntConcreteLibfunc
}

/// Libfunc for adding two BoundedInts.
/// The result is a BoundedInt.
#[derive(Default)]
pub struct BoundedIntAddLibfunc {}
impl SignatureOnlyGenericLibfunc for BoundedIntAddLibfunc {
    const STR_ID: &'static str = "bounded_int_add";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        specialize_helper(context, args, |lhs_range, rhs_range| {
            (lhs_range.lower + rhs_range.lower, (lhs_range.upper - 1) + (rhs_range.upper - 1))
        })
    }
}

/// Libfunc for subtracting two BoundedInts.
/// The result is a BoundedInt.
#[derive(Default)]
pub struct BoundedIntSubLibfunc {}
impl SignatureOnlyGenericLibfunc for BoundedIntSubLibfunc {
    const STR_ID: &'static str = "bounded_int_sub";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        specialize_helper(context, args, |lhs_range, rhs_range| {
            (lhs_range.lower - (rhs_range.upper - 1), (lhs_range.upper - 1) - rhs_range.lower)
        })
    }
}

/// Libfunc for multiplying two BoundedInts.
/// The result is a BoundedInt.
#[derive(Default)]
pub struct BoundedIntMulLibfunc {}
impl SignatureOnlyGenericLibfunc for BoundedIntMulLibfunc {
    const STR_ID: &'static str = "bounded_int_mul";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        specialize_helper(context, args, |lhs_range, rhs_range| {
            // The result is the minimum and maximum of the four possible extremes.
            // Done to properly handle multiplication by negative values.
            let extremes = [
                &lhs_range.lower * &rhs_range.lower,
                lhs_range.lower * (&rhs_range.upper - 1),
                (&lhs_range.upper - 1) * rhs_range.lower,
                (lhs_range.upper - 1) * (rhs_range.upper - 1),
            ];
            extremes.into_iter().minmax().into_option().unwrap()
        })
    }
}

/// Libfunc for multiplying two BoundedInts.
/// The result is a BoundedInt.
#[derive(Default)]
pub struct BoundedIntDivRemLibfunc {}
impl NamedLibfunc for BoundedIntDivRemLibfunc {
    type Concrete = BoundedIntDivRemConcreteLibfunc;

    const STR_ID: &'static str = "bounded_int_div_rem";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (lhs, rhs) = args_as_two_types(args)?;
        let lhs_range = Range::from_type(context, lhs.clone())?;
        let rhs_range = Range::from_type(context, rhs.clone())?;
        // Supporting only division of a non-negative number by a positive number.
        // TODO(orizi): Consider relaxing the constraint, and defining the div_rem of negatives.
        if lhs_range.lower.is_negative() || !rhs_range.lower.is_positive() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        // The divisor upped bound multiplied by the range check bound must not wraparound the
        // prime.
        if (&rhs_range.upper).shl(128) >= Felt252::prime().to_bigint().unwrap() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let min_quotient = lhs_range.lower / (&rhs_range.upper - 1);
        let max_quotient = (&lhs_range.upper - 1) / rhs_range.lower;
        // The quotient bound must be less than the range check bound.
        if max_quotient >= BigInt::one().shl(128) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(lhs.clone()),
                ParamSignature::new(rhs.clone()),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type.clone(), 0),
                OutputVarInfo {
                    ty: bounded_int_ty(context, min_quotient, max_quotient)?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
                OutputVarInfo {
                    ty: bounded_int_ty(context, 0.into(), rhs_range.upper - 2)?,
                    ref_info: OutputVarReferenceInfo::SimpleDerefs,
                },
            ],
            SierraApChange::Known { new_vars_only: false },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (_lhs, rhs) = args_as_two_types(args)?;
        Ok(Self::Concrete {
            dividend_bound: Range::from_type(context.upcast(), rhs.clone())?.upper,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct BoundedIntDivRemConcreteLibfunc {
    /// The upper bound of the dividend.
    pub dividend_bound: BigInt,
    signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for BoundedIntDivRemConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Helper function for specializing the signature of a simple operation bounded int libfunc.
fn specialize_helper(
    context: &dyn SignatureSpecializationContext,
    args: &[GenericArg],
    result_range: impl Fn(Range, Range) -> (BigInt, BigInt),
) -> Result<LibfuncSignature, SpecializationError> {
    let (lhs, rhs) = args_as_two_types(args)?;

    let (min_result, max_result) = result_range(
        Range::from_type(context, lhs.clone())?,
        Range::from_type(context, rhs.clone())?,
    );
    Ok(LibfuncSignature::new_non_branch(
        vec![lhs, rhs],
        vec![OutputVarInfo {
            ty: bounded_int_ty(context, min_result, max_result)?,
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        }],
        SierraApChange::Known { new_vars_only: false },
    ))
}

/// Returns the concrete type for a BoundedInt with the given range.
fn bounded_int_ty(
    context: &dyn SignatureSpecializationContext,
    min: BigInt,
    max: BigInt,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(BoundedIntType::ID, &[GenericArg::Value(min), GenericArg::Value(max)])
}
