use super::array::ArrayType;
use super::felt::FeltType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};

define_libfunc_hierarchy! {
    pub enum CheatcodesLibFunc {
        Roll(RollLibFunc),
        Warp(WarpLibFunc),
        Declare(DeclareLibFunc),
        StartPrank(StartPrankLibFunc),
        Invoke(InvokeLibFunc),
        MockCall(MockCallLibFunc),
        Deploy(DeployLibFunc),
    }, CheatcodesConcreteLibFunc
}

#[derive(Default)]
pub struct DeclareLibFunc {}
impl NoGenericArgsGenericLibfunc for DeclareLibFunc {
    const STR_ID: &'static str = "declare";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Contract
                ParamSignature::new(felt_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        // ty: context.get_concrete_type(ClassHashType::id(), &[])?,
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct RollLibFunc {}
impl NoGenericArgsGenericLibfunc for RollLibFunc {
    const STR_ID: &'static str = "roll";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Address
                ParamSignature::new(felt_ty.clone()),
                // Value
                ParamSignature::new(felt_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct WarpLibFunc {}
impl NoGenericArgsGenericLibfunc for WarpLibFunc {
    const STR_ID: &'static str = "warp";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Address
                ParamSignature::new(felt_ty.clone()),
                // Value
                ParamSignature::new(felt_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct StartPrankLibFunc {}
impl NoGenericArgsGenericLibfunc for StartPrankLibFunc {
    const STR_ID: &'static str = "start_prank";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // caller_address
                ParamSignature::new(felt_ty.clone()),
                // target_contract_address
                ParamSignature::new(felt_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure branch
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct InvokeLibFunc {}
impl NoGenericArgsGenericLibfunc for InvokeLibFunc {
    const STR_ID: &'static str = "invoke";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // contract_address
                ParamSignature::new(felt_ty.clone()),
                // function_name
                ParamSignature::new(felt_ty.clone()),
                // calldata
                ParamSignature::new(arr_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure branch
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for creating a new array.
#[derive(Default)]
pub struct MockCallLibFunc {}
impl NoGenericArgsGenericLibfunc for MockCallLibFunc {
    const STR_ID: &'static str = "mock_call";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // contract_address
                ParamSignature::new(felt_ty.clone()),
                // function_name
                ParamSignature::new(felt_ty.clone()),
                // response
                ParamSignature::new(arr_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure branch
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct DeployLibFunc {}
impl NoGenericArgsGenericLibfunc for DeployLibFunc {
    const STR_ID: &'static str = "deploy";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // prepared_contract_address
                ParamSignature::new(felt_ty.clone()),
                // prepared_class_hash
                ParamSignature::new(felt_ty.clone()),
                // prepared_constructor_calldata
                ParamSignature::new(arr_ty.clone()),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
