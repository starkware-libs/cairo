use cairo_lang_utils::extract_matches;
use itertools::{Itertools, chain};
use num_traits::Zero;

use super::coupon::coupon_ty;
use crate::extensions::lib_func::{
    LibfuncSignature, OutputVarInfo, SierraApChange, SignatureBasedConcreteLibfunc,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, OutputVarReferenceInfo, SpecializationError, args_as_single_user_func,
};
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
    type Concrete = SignatureAndFunctionConcreteLibfunc;
    const STR_ID: &'static str = "function_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let function_id = args_as_single_user_func(args)?;

        let signature = context.get_function_signature(&function_id)?;
        let ap_change = context.get_function_ap_change(&function_id)?;
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
        let function_id = args_as_single_user_func(args)?;

        Ok(Self::Concrete {
            function: context.get_function(&function_id)?,
            signature: self.specialize_signature(context, args)?,
        })
    }
}

pub struct SignatureAndFunctionConcreteLibfunc {
    pub function: Function,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for SignatureAndFunctionConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc used to call user functions.
#[derive(Default)]
pub struct CouponCallLibfunc {}
impl NamedLibfunc for CouponCallLibfunc {
    type Concrete = SignatureAndFunctionConcreteLibfunc;
    const STR_ID: &'static str = "coupon_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let function_id = args_as_single_user_func(args)?;

        let signature = context.get_function_signature(&function_id)?;
        let ap_change = context.get_function_ap_change(&function_id)?;

        let coupon_ty = coupon_ty(context, function_id)?;
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
        let function_id = args_as_single_user_func(args)?;

        Ok(Self::Concrete {
            function: context.get_function(&function_id)?,
            signature: self.specialize_signature(context, args)?,
        })
    }
}

/// Libfunc to replace function calls in size estiamtion.
/// The libfunc consumes the function arguments and returns the function return values.
/// The ap_change and signature are encoded in the generic arguments, see
/// `try_extract_dummy_func_info` for details.
#[derive(Default)]
pub struct DummyFunctionCallLibfunc {}
impl NamedLibfunc for DummyFunctionCallLibfunc {
    type Concrete = SignatureAndFunctionConcreteLibfunc;
    const STR_ID: &'static str = "dummy_function_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        let Some((signature, ap_change)) = try_extract_dummy_func_info(args.iter()) else {
            return Err(SpecializationError::UnsupportedGenericArg);
        };

        Ok(LibfuncSignature::new_non_branch(
            signature.param_types.clone(),
            get_output_var_infos(context, signature).unwrap(),
            ap_change,
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        let function_id = args_as_single_user_func(&args[0..1])?;

        Ok(Self::Concrete {
            function: context.get_function(&function_id)?,
            signature: self.specialize_signature(context, args)?,
        })
    }
}

/// Given the generic arguments of a dummy function call, returns the function signature and and
/// ap_change.
fn try_extract_dummy_func_info<'a>(
    mut args: impl Iterator<Item = &'a GenericArg>,
) -> Option<(FunctionSignature, SierraApChange)> {
    let _user_func_id = args.next()?;

    let GenericArg::Value(ap_change) = args.next()? else {
        return None;
    };
    let ap_change = if ap_change.is_zero() {
        SierraApChange::Known { new_vars_only: false }
    } else {
        SierraApChange::Unknown
    };
    let GenericArg::Value(n_params) = args.next()? else {
        return None;
    };
    let param_types = args
        .by_ref()
        .take(n_params.try_into().ok()?)
        .map(|garg| extract_matches!(garg, GenericArg::Type).clone())
        .collect_vec();

    let GenericArg::Value(n_ret) = args.next()? else {
        return None;
    };
    let ret_types = args
        .by_ref()
        .take(n_ret.try_into().ok()?)
        .map(|garg| extract_matches!(garg, GenericArg::Type).clone())
        .collect_vec();
    Some((FunctionSignature { param_types, ret_types }, ap_change))
}
