use std::ops::Shl;

use cairo_lang_utils::require;
use itertools::Itertools;
use num_bigint::{BigInt, ToBigInt};
use num_traits::{One, Signed, Zero};
use starknet_types_core::felt::Felt as Felt252;

use super::non_zero::{NonZeroType, nonzero_ty};
use super::range_check::RangeCheckType;
use super::utils::{Range, reinterpret_cast_signature};
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureOnlyGenericLibfunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SignatureBasedConcreteLibfunc,
    SpecializationError, args_as_single_type, args_as_two_types,
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
        Constrain(BoundedIntConstrainLibfunc),
        TrimMin(BoundedIntTrimLibfunc<false>),
        TrimMax(BoundedIntTrimLibfunc<true>),
        IsZero(BoundedIntIsZeroLibfunc),
        WrapNonZero(BoundedIntWrapNonZeroLibfunc),
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
        let (lhs, rhs) = args_as_two_types(args)?;
        let lhs_info = context.get_type_info(lhs.clone())?;
        let rhs_info = context.get_type_info(rhs.clone())?;
        let is_nz = {
            let lhs_is_nz = lhs_info.long_id.generic_id == NonZeroType::ID;
            let rhs_is_nz = rhs_info.long_id.generic_id == NonZeroType::ID;
            if lhs_is_nz != rhs_is_nz {
                return Err(SpecializationError::UnsupportedGenericArg);
            }
            lhs_is_nz
        };

        let [lhs_range, rhs_range] = if is_nz {
            [lhs_info, rhs_info].map(|info| {
                let inner_ty = args_as_single_type(&info.long_id.generic_args)?;
                Range::from_type(context, inner_ty)
            })
        } else {
            [lhs_info, rhs_info].map(|info| Range::from_type_info(&info))
        };
        let (lhs_range, rhs_range) = (lhs_range?, rhs_range?);
        // The result is the minimum and maximum of the four possible extremes.
        // Done to properly handle multiplication by negative values.
        let extremes = [
            &lhs_range.lower * &rhs_range.lower,
            lhs_range.lower * (&rhs_range.upper - 1),
            (&lhs_range.upper - 1) * rhs_range.lower,
            (lhs_range.upper - 1) * (rhs_range.upper - 1),
        ];
        let (min_result, max_result) = extremes.into_iter().minmax().into_option().unwrap();
        let res_ty = bounded_int_ty(context, min_result, max_result)?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![ParamSignature::new(lhs), ParamSignature::new(rhs).with_allow_const()],
            vec![OutputVarInfo {
                ty: if is_nz { nonzero_ty(context, &res_ty)? } else { res_ty },
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}

/// Libfunc for dividing two non negative BoundedInts and getting the quotient and remainder.
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
        // Supporting only division of a non-negative number by a positive number (non zero and non
        // negative).
        // TODO(orizi): Consider relaxing the constraint, and defining the
        // div_rem of negatives.
        require(!lhs_range.lower.is_negative() && !rhs_range.lower.is_negative())
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        // Making sure the algorithm is runnable.
        BoundedIntDivRemAlgorithm::try_new(&lhs_range, &rhs_range)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        require(rhs_range.upper >= BigInt::from(2))
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let quotient_min = lhs_range.lower / (&rhs_range.upper - 1);
        let quotient_max = (&lhs_range.upper - 1) / std::cmp::max(rhs_range.lower, BigInt::one());
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature::new_non_branch_ex(
            vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(lhs.clone()),
                ParamSignature::new(nonzero_ty(context, &rhs)?),
            ],
            vec![
                OutputVarInfo::new_builtin(range_check_type.clone(), 0),
                OutputVarInfo {
                    ty: bounded_int_ty(context, quotient_min, quotient_max)?,
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
        let (lhs, rhs) = args_as_two_types(args)?;
        let context = context.upcast();
        Ok(Self::Concrete {
            lhs: Range::from_type(context, lhs.clone())?,
            rhs: Range::from_type(context, rhs.clone())?,
            signature: self.specialize_signature(context, args)?,
        })
    }
}

pub struct BoundedIntDivRemConcreteLibfunc {
    pub lhs: Range,
    pub rhs: Range,
    signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for BoundedIntDivRemConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// The algorithm to use for division and remainder of bounded integers.
pub enum BoundedIntDivRemAlgorithm {
    /// The rhs is small enough to be multiplied by `2**128` without wraparound.
    KnownSmallRhs,
    /// The quotient is small enough to be multiplied by `2**128` without wraparound.
    KnownSmallQuotient { q_upper_bound: BigInt },
    /// The lhs is small enough so that its square root plus 1 can be multiplied by `2**128`
    /// without wraparound.
    /// `lhs_upper_sqrt` is the square root of the upper bound of the lhs, rounded up.
    KnownSmallLhs { lhs_upper_sqrt: BigInt },
}
impl BoundedIntDivRemAlgorithm {
    /// Returns the algorithm to use for division and remainder of bounded integers.
    /// Fails if the div_rem of the ranges is not supported yet.
    ///
    /// Assumption: `lhs` is non-negative and `rhs` is positive.
    pub fn try_new(lhs: &Range, rhs: &Range) -> Option<Self> {
        let prime = Felt252::prime().to_bigint().unwrap();
        // Note that `rhs.lower` may be 0 - but since it is a non-zero type, it is guaranteed to be
        // at least 1.
        let q_max = (&lhs.upper - 1) / std::cmp::max(&rhs.lower, &BigInt::one());
        let u128_limit = BigInt::one().shl(128);
        // `q` is range checked in all algorithm variants, so `q_max` must be smaller than `2**128`.
        require(q_max < u128_limit)?;
        // `r` is range checked in all algorithm variants, so `rhs.upper` must be at most
        // `2**128 + 1`.
        require(rhs.upper <= &u128_limit + 1)?;
        if &rhs.upper * &u128_limit < prime {
            return Some(Self::KnownSmallRhs);
        }
        let q_upper_bound = q_max + 1;
        if &q_upper_bound * &u128_limit < prime {
            return Some(Self::KnownSmallQuotient { q_upper_bound });
        }
        let mut lhs_upper_sqrt = lhs.upper.sqrt();
        // Round lhs_upper_sqrt up.
        if lhs_upper_sqrt.pow(2) != lhs.upper {
            lhs_upper_sqrt += 1;
        }
        if &lhs_upper_sqrt * &u128_limit < prime {
            // Make sure `lhs_upper_sqrt < 2**128`, since the value bounded by root is range
            // checked.
            require(lhs_upper_sqrt < u128_limit)?;
            return Some(Self::KnownSmallLhs { lhs_upper_sqrt });
        }
        // No algorithm found.
        None
    }
}

/// Libfunc for constraining a BoundedInt<Min, Max> to one of two non-empty ranges: [Min, Boundary)
/// or [Boundary, Max]. The libfunc is also applicable for standard types such as u* and i*.
#[derive(Default)]
pub struct BoundedIntConstrainLibfunc {}
impl NamedLibfunc for BoundedIntConstrainLibfunc {
    type Concrete = BoundedIntConstrainConcreteLibfunc;

    const STR_ID: &'static str = "bounded_int_constrain";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let (ty, boundary) = match args {
            [GenericArg::Type(ty), GenericArg::Value(boundary)] => Ok((ty, boundary)),
            [_, _] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }?;
        let ty_info = context.get_type_info(ty.clone())?;
        let is_nz = ty_info.long_id.generic_id == NonZeroType::ID;
        let range = if is_nz {
            let inner_ty = args_as_single_type(&ty_info.long_id.generic_args)?;
            Range::from_type(context, inner_ty)?
        } else {
            Range::from_type_info(&ty_info)?
        };
        require(&range.lower < boundary && boundary < &range.upper)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let low_range = Range::half_open(range.lower, boundary.clone());
        let high_range = Range::half_open(boundary.clone(), range.upper);
        require(low_range.is_small_range() && high_range.is_small_range())
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        let branch_signature = |rng: Range| {
            let inner_res_ty = bounded_int_ty(context, rng.lower, rng.upper - 1)?;
            let res_ty = if is_nz { nonzero_ty(context, &inner_res_ty)? } else { inner_res_ty };
            Ok(BranchSignature {
                vars: vec![
                    OutputVarInfo::new_builtin(range_check_type.clone(), 0),
                    OutputVarInfo {
                        ty: res_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: false },
            })
        };
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                ParamSignature::new(ty.clone()),
            ],
            branch_signatures: vec![branch_signature(low_range)?, branch_signature(high_range)?],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let boundary = match args {
            [GenericArg::Type(_), GenericArg::Value(boundary)] => Ok(boundary.clone()),
            [_, _] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }?;
        let context = context.upcast();
        Ok(Self::Concrete { boundary, signature: self.specialize_signature(context, args)? })
    }
}

pub struct BoundedIntConstrainConcreteLibfunc {
    pub boundary: BigInt,
    signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for BoundedIntConstrainConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for trimming a BoundedInt<Min, Max> by removing `Min` or `Max` from the range.
/// The libfunc is also applicable for standard types such as u* and i*.
#[derive(Default)]
pub struct BoundedIntTrimLibfunc<const IS_MAX: bool> {}
impl<const IS_MAX: bool> NamedLibfunc for BoundedIntTrimLibfunc<IS_MAX> {
    type Concrete = BoundedIntTrimConcreteLibfunc;

    const STR_ID: &'static str =
        if IS_MAX { "bounded_int_trim_max" } else { "bounded_int_trim_min" };

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(Self::Concrete::new::<IS_MAX>(context, args)?.signature)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        Self::Concrete::new::<IS_MAX>(context.upcast(), args)
    }
}

pub struct BoundedIntTrimConcreteLibfunc {
    pub trimmed_value: BigInt,
    signature: LibfuncSignature,
}
impl BoundedIntTrimConcreteLibfunc {
    fn new<const IS_MAX: bool>(
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let ty_info = context.get_type_info(ty.clone())?;
        let range = Range::from_type_info(&ty_info)?;
        let (res_ty, trimmed_value) = if IS_MAX {
            (
                bounded_int_ty(context, range.lower.clone(), range.upper.clone() - 2)?,
                range.upper - 1,
            )
        } else {
            (
                bounded_int_ty(context, range.lower.clone() + 1, range.upper.clone() - 1)?,
                range.lower,
            )
        };
        let ap_change = SierraApChange::Known { new_vars_only: trimmed_value.is_zero() };
        let signature = LibfuncSignature {
            param_signatures: vec![ParamSignature::new(ty.clone())],
            branch_signatures: vec![
                BranchSignature { vars: vec![], ap_change: ap_change.clone() },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: res_ty,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change,
                },
            ],
            fallthrough: Some(0),
        };
        Ok(Self { trimmed_value, signature })
    }
}
impl SignatureBasedConcreteLibfunc for BoundedIntTrimConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Helper function for specializing the signature of a simple bounded int operation libfunc.
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
    Ok(LibfuncSignature::new_non_branch_ex(
        vec![ParamSignature::new(lhs), ParamSignature::new(rhs).with_allow_const()],
        vec![OutputVarInfo {
            ty: bounded_int_ty(context, min_result, max_result)?,
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        }],
        SierraApChange::Known { new_vars_only: true },
    ))
}

/// Libfunc for checking whether the given bounded int is zero or not, and returning a non-zero
/// wrapped value in case of success.
#[derive(Default)]
pub struct BoundedIntIsZeroLibfunc;
impl SignatureOnlyGenericLibfunc for BoundedIntIsZeroLibfunc {
    const STR_ID: &'static str = "bounded_int_is_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let range = Range::from_type(context, ty.clone())?;
        // Make sure 0 is actually in the given range.
        require(!range.lower.is_positive() && range.upper.is_positive())
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(ty.clone())],
            branch_signatures: vec![
                // Zero.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
                // NonZero.
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: nonzero_ty(context, &ty)?,
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: true },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for wrapping a given bounded int with non-zero, given 0 is not in the range.
#[derive(Default)]
pub struct BoundedIntWrapNonZeroLibfunc {}
impl SignatureOnlyGenericLibfunc for BoundedIntWrapNonZeroLibfunc {
    const STR_ID: &'static str = "bounded_int_wrap_non_zero";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = args_as_single_type(args)?;
        let range = Range::from_type(context, ty.clone())?;
        // Make sure 0 is not in the given range.
        require(range.lower.is_positive() || !range.upper.is_positive())
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let prime: BigInt = Felt252::prime().to_bigint().unwrap();
        require(range.upper <= prime && range.lower > -prime)
            .ok_or(SpecializationError::UnsupportedGenericArg)?;
        let nz_ty = nonzero_ty(context, &ty)?;
        Ok(reinterpret_cast_signature(ty, nz_ty))
    }
}

/// Returns the concrete type for a BoundedInt<min, max>.
pub fn bounded_int_ty(
    context: &dyn SignatureSpecializationContext,
    min: BigInt,
    max: BigInt,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(BoundedIntType::ID, &[GenericArg::Value(min), GenericArg::Value(max)])
}
