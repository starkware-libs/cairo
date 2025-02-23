use super::function_call::SignatureAndFunctionConcreteLibfunc;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::type_specialization_context::TypeSpecializationContext;
use crate::extensions::types::TypeInfo;
use crate::extensions::{
    ConcreteType, NamedLibfunc, NamedType, OutputVarReferenceInfo, SpecializationError,
    args_as_single_type, args_as_single_user_func,
};
use crate::ids::{ConcreteTypeId, FunctionId, GenericTypeId};
use crate::program::GenericArg;

/// Coupon type `Coupon<function>` (`function::Coupon`) which represents that the cost of a
/// function was paid, without calling the function yet.
///
/// Using the coupon the function can be called without paying the cost.
#[derive(Default)]
pub struct CouponType {}

impl NamedType for CouponType {
    type Concrete = CouponConcreteType;
    const ID: GenericTypeId = GenericTypeId::new_inline("Coupon");

    fn specialize(
        &self,
        _context: &dyn TypeSpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let function_id = args_as_single_user_func(args)?;
        let long_id = Self::concrete_type_long_id(args);
        Ok(Self::Concrete {
            info: TypeInfo {
                long_id,
                duplicatable: false,
                droppable: true,
                storable: true,
                zero_sized: true,
            },
            function_id,
        })
    }
}

/// Returns the type `Coupon<func>`.
pub fn coupon_ty(
    context: &dyn SignatureSpecializationContext,
    function_id: FunctionId,
) -> Result<ConcreteTypeId, SpecializationError> {
    context.get_concrete_type(CouponType::id(), &[GenericArg::UserFunc(function_id)])
}

/// Concrete type information for `Coupon<function>`.
pub struct CouponConcreteType {
    pub info: TypeInfo,
    pub function_id: FunctionId,
}
impl ConcreteType for CouponConcreteType {
    fn info(&self) -> &TypeInfo {
        &self.info
    }
}

define_libfunc_hierarchy! {
    pub enum CouponLibfunc {
        Buy(CouponBuyLibfunc),
        Refund(CouponRefundLibfunc),
    }, CouponConcreteLibfunc
}

/// Libfunc for buying a coupon for a function.
///
/// The cost of the coupon is the cost of running the function (not including the `call` and `ret`
/// instructions). The coupon can be used to pay in advance for running the function, and run it
/// later for free (paying only for the `call` and `ret` instructions) using `coupon_call`.
#[derive(Default)]
pub struct CouponBuyLibfunc {}
impl NamedLibfunc for CouponBuyLibfunc {
    type Concrete = SignatureAndFunctionConcreteLibfunc;
    const STR_ID: &'static str = "coupon_buy";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let coupon_ty = args_as_single_type(args)?;
        if context.get_type_info(coupon_ty.clone())?.long_id.generic_id != CouponType::id() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo { ty: coupon_ty, ref_info: OutputVarReferenceInfo::ZeroSized }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let coupon_ty = args_as_single_type(args)?;
        let long_id = context.get_type_info(coupon_ty.clone())?.long_id;
        if long_id.generic_id != CouponType::id() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let function_id = args_as_single_user_func(&long_id.generic_args)?;

        Ok(SignatureAndFunctionConcreteLibfunc {
            function: context.get_function(&function_id)?,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

/// Libfunc for getting a refund for an unused coupon. The refund is the cost of the function
/// and it is added back to the gas wallet.
#[derive(Default)]
pub struct CouponRefundLibfunc {}
impl NamedLibfunc for CouponRefundLibfunc {
    type Concrete = SignatureAndFunctionConcreteLibfunc;
    const STR_ID: &'static str = "coupon_refund";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let coupon_ty = args_as_single_type(args)?;
        if context.get_type_info(coupon_ty.clone())?.long_id.generic_id != CouponType::id() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        Ok(LibfuncSignature::new_non_branch(
            vec![coupon_ty],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn crate::extensions::lib_func::SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let coupon_ty = args_as_single_type(args)?;
        let long_id = context.get_type_info(coupon_ty.clone())?.long_id;
        if long_id.generic_id != CouponType::id() {
            return Err(SpecializationError::UnsupportedGenericArg);
        }

        let function_id = args_as_single_user_func(&long_id.generic_args)?;

        Ok(SignatureAndFunctionConcreteLibfunc {
            function: context.get_function(&function_id)?,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}
