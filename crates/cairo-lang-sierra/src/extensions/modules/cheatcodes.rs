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
        DeclareCairo0(DeclareCairo0LibFunc),
        StartPrank(StartPrankLibFunc),
        StopPrank(StopPrankLibFunc),
        Invoke(InvokeLibFunc),
        MockCall(MockCallLibFunc),
        Deploy(DeployLibFunc),
        DeployCairo0(DeployCairo0LibFunc),
        Prepare(PrepareLibFunc),
        PrepareCairo0(PrepareCairo0LibFunc),
        Call(CallLibFunc),
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
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
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

#[derive(Default)]
pub struct DeclareCairo0LibFunc {}
impl NoGenericArgsGenericLibfunc for DeclareCairo0LibFunc {
    const STR_ID: &'static str = "declare_cairo0";

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

/// LibFunc for stopping a prank
#[derive(Default)]
pub struct StopPrankLibFunc {}
impl NoGenericArgsGenericLibfunc for StopPrankLibFunc {
    const STR_ID: &'static str = "stop_prank";

    // noinspection DuplicatedCode
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;

        Ok(LibfuncSignature {
            param_signatures: vec![
                // Target address
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
    const STR_ID: &'static str = "deploy_tp";

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

#[derive(Default)]
pub struct DeployCairo0LibFunc {}
impl NoGenericArgsGenericLibfunc for DeployCairo0LibFunc {
    const STR_ID: &'static str = "deploy_tp_cairo0";

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

#[derive(Default)]
pub struct PrepareLibFunc {}
impl NoGenericArgsGenericLibfunc for PrepareLibFunc {
    const STR_ID: &'static str = "prepare_tp";
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(arr_ty.clone()),
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        // Constructor Calldata
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                        // Contract Address
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                        // Class Hash
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct PrepareCairo0LibFunc {}
impl NoGenericArgsGenericLibfunc for PrepareCairo0LibFunc {
    const STR_ID: &'static str = "prepare_tp_cairo0";
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(FeltType::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(arr_ty.clone()),
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        // Constructor Calldata
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                        // Contract Address
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                        // Class Hash
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct CallLibFunc {}
impl NoGenericArgsGenericLibfunc for CallLibFunc {
    const STR_ID: &'static str = "call";

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
                    vars: vec![
                        // Return Data
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    // Error reason
                    vars: vec![OutputVarInfo {
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: Some(0) },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
