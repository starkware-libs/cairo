use itertools::chain;

use super::coupon::CouponType;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SignatureBasedConcreteLibfunc, SignatureSpecializationContext,
    SpecializationContext,
};
use crate::extensions::{NamedLibfunc, NamedType, OutputVarReferenceInfo, SpecializationError};
use crate::program::{Function, FunctionSignature, GenericArg};

/// Returns the [OutputVarInfo] instances for the return types of the given function signature.
fn get_output_var_infos(
    context: &dyn SignatureSpecializationContext,
    signature: FunctionSignature,
) -> Result<Vec<OutputVarInfo>, SpecializationError> {
    let mut curr_stack_idx = 0;
    let mut get_stack_idx = || {
        let idx = curr_stack_idx;
        curr_stack_idx += 1;
        idx
    };

    signature
        .ret_types
        .iter()
        .map(|ty| {
            Ok(OutputVarInfo {
                ty: ty.clone(),
                ref_info: if context.get_type_info(ty.clone())?.zero_sized {
                    OutputVarReferenceInfo::ZeroSized
                } else {
                    OutputVarReferenceInfo::NewTempVar { idx: get_stack_idx() }
                },
            })
        })
        .collect::<Result<Vec<_>, _>>()
}

/// Libfunc used to call user functions.
#[derive(Default)]
pub struct FunctionCallLibfunc {}
impl NamedLibfunc for FunctionCallLibfunc {
    type Concrete = FunctionCallConcreteLibfunc;
    const STR_ID: &'static str = "function_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let [GenericArg::UserFunc(function_id)] = args else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        let signature = context.get_function_signature(function_id)?;
        let ap_change = context.get_function_ap_change(function_id)?;
        Ok(LibfuncSignature::new_non_branch(
            signature.param_types.clone(),
            get_output_var_infos(context, signature)?,
            ap_change,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let [GenericArg::UserFunc(function_id)] = args else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        Ok(Self::Concrete {
            function: context.get_function(function_id)?,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}

pub struct FunctionCallConcreteLibfunc {
    pub function: Function,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for FunctionCallConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc used to call user functions.
#[derive(Default)]
pub struct CouponCallLibfunc {}
impl NamedLibfunc for CouponCallLibfunc {
    type Concrete = FunctionCallConcreteLibfunc;
    const STR_ID: &'static str = "coupon_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let [GenericArg::UserFunc(function_id)] = args else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        let signature = context.get_function_signature(function_id)?;
        let ap_change = context.get_function_ap_change(function_id)?;

        let coupon_ty = context
            .get_concrete_type(CouponType::id(), &[GenericArg::UserFunc(function_id.clone())])?;
        Ok(LibfuncSignature::new_non_branch(
            chain!(signature.param_types.iter().cloned(), [coupon_ty]).collect(),
            get_output_var_infos(context, signature)?,
            ap_change,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let [GenericArg::UserFunc(function_id)] = args else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        Ok(Self::Concrete {
            function: context.get_function(function_id)?,
            signature: self.specialize_signature(context.upcast(), args)?,
        })
    }
}
