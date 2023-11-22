use cairo_felt::Felt252;
use num_bigint::BigInt;
use num_traits::Zero;

use super::felt252::Felt252Type;
use super::range_check::RangeCheckType;
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type for Felt252Bounded.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct Felt252BoundedType {}
impl NamedType for Felt252BoundedType {
    type Concrete = Felt252BoundedConcreteType;

    const ID: GenericTypeId = GenericTypeId::new_inline("Felt252Bounded");
    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let (lower_bound, upper_bound) = match args {
            [GenericArg::Value(lower_bound), GenericArg::Value(upper_bound)] => {
                (lower_bound.clone(), upper_bound.clone())
            }
            [_, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        if !lower_bound.is_zero() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if lower_bound >= upper_bound {
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

        Ok(Self::Concrete { info: ty_info, lower_bound, upper_bound })
    }
}

//
pub struct Felt252BoundedConcreteType {
    pub info: TypeInfo,
    /// The lower bound for a value of this type (inclusive).
    pub lower_bound: BigInt,
    /// The upper bound for a value of this type (exclusive).
    pub upper_bound: BigInt,
}
impl ConcreteType for Felt252BoundedConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum Felt252BoundedLibfunc {
        FromFelt(Felt252BoundedFromFelt252Libfunc),
        ConstrainRange(Felt252BoundedConstrainRangeLibfunc),
    }, Felt252BoundedConcreteLibfunc
}

#[derive(Default)]
pub struct Felt252BoundedFromFelt252Libfunc {}

impl NoGenericArgsGenericLibfunc for Felt252BoundedFromFelt252Libfunc {
    const STR_ID: &'static str = "felt252_bounded_from_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let ty = context.get_concrete_type(
            Felt252BoundedType::id(),
            &[GenericArg::Value(0.into()), GenericArg::Value(Felt252::prime().into())],
        )?;
        Ok(reinterpret_cast_signature(context.get_concrete_type(Felt252Type::id(), &[])?, ty))
    }
}

pub struct Felt252BoundedConstrainRange {
    pub signature: LibfuncSignature,
    /// The lower bound for a value of the input (inclusive).
    pub lower_bound_in: BigInt,
    /// The upper bound for a value of the input (exclusive).
    pub upper_bound_in: BigInt,
    /// The lower bound for a value of the output (inclusive).
    pub lower_bound_out: BigInt,
    /// The upper bound for a value of the output (exclusive).
    pub upper_bound_out: BigInt,
}
impl SignatureBasedConcreteLibfunc for Felt252BoundedConstrainRange {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

#[derive(Default)]
pub struct Felt252BoundedConstrainRangeLibfunc {}
impl Felt252BoundedConstrainRangeLibfunc {
    /// Creates the specialization of the Felt252BoundedConstrainRange libfunc with the given
    /// template arguments.
    fn specialize_concrete_lib_func(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Felt252BoundedConstrainRange, SpecializationError> {
        let (lower_bound_in, upper_bound_in, lower_bound_out, upper_bound_out) = match args {
            [
                GenericArg::Value(lower_bound_in),
                GenericArg::Value(upper_bound_in),
                GenericArg::Value(lower_bound_out),
                GenericArg::Value(upper_bound_out),
            ] => (
                lower_bound_in.clone(),
                upper_bound_in.clone(),
                lower_bound_out.clone(),
                upper_bound_out.clone(),
            ),
            [_, _, _, _] => return Err(SpecializationError::UnsupportedGenericArg),
            _ => return Err(SpecializationError::WrongNumberOfGenericArgs),
        };
        if !lower_bound_in.is_zero() || !lower_bound_out.is_zero() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        if lower_bound_in >= upper_bound_in || lower_bound_out >= upper_bound_out {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        // Only allowing `upper_bound <= 2**128` for cheap checking for when in range.
        // TODO(tomerstarkware): support more ranges.
        if upper_bound_out > BigInt::from(u128::MAX) {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        if upper_bound_in <= upper_bound_out {
            return Err(SpecializationError::UnsupportedGenericArg);
        }
        let ty_in = context.get_concrete_type(Felt252BoundedType::id(), &args[..2])?;
        let ty_out = context.get_concrete_type(Felt252BoundedType::id(), &args[2..])?;

        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;

        Ok(Felt252BoundedConstrainRange {
            signature: LibfuncSignature {
                param_signatures: vec![
                    ParamSignature::new(range_check_type.clone()).with_allow_add_const(),
                    ParamSignature::new(ty_in),
                ],
                branch_signatures: vec![
                    BranchSignature {
                        vars: vec![
                            OutputVarInfo {
                                ty: range_check_type.clone(),
                                ref_info: OutputVarReferenceInfo::Deferred(
                                    DeferredOutputKind::AddConst { param_idx: 0 },
                                ),
                            },
                            OutputVarInfo {
                                ty: ty_out,
                                ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                            },
                        ],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                    BranchSignature {
                        vars: vec![OutputVarInfo {
                            ty: range_check_type,
                            ref_info: OutputVarReferenceInfo::Deferred(
                                DeferredOutputKind::AddConst { param_idx: 0 },
                            ),
                        }],
                        ap_change: SierraApChange::Known { new_vars_only: false },
                    },
                ],
                fallthrough: Some(0),
            },
            lower_bound_in,
            upper_bound_in,
            lower_bound_out,
            upper_bound_out,
        })
    }
}
impl NamedLibfunc for Felt252BoundedConstrainRangeLibfunc {
    type Concrete = Felt252BoundedConstrainRange;
    const STR_ID: &'static str = "felt252_bounded_constrain_range";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(self.specialize_concrete_lib_func(context, args)?.signature)
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        self.specialize_concrete_lib_func(context.upcast(), args)
    }
}
