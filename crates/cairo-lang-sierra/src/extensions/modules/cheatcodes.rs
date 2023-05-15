use super::array::ArrayType;
use super::felt252::Felt252Type;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext,
};
use crate::extensions::snapshot::snapshot_ty;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};

define_libfunc_hierarchy! {
    pub enum CheatcodesLibFunc {
        StartRoll(StartRollLibFunc),
        StopRoll(StopRollLibFunc),
        StartWarp(StartWarpLibFunc),
        StopWarp(StopWarpLibFunc),
        Declare(DeclareLibFunc),
        DeclareCairo0(DeclareCairo0LibFunc),
        StartPrank(StartPrankLibFunc),
        StopPrank(StopPrankLibFunc),
        Invoke(InvokeLibFunc),
        MockCall(MockCallLibFunc),
        Deploy(DeployLibFunc),
        Prepare(PrepareLibFunc),
        Call(CallLibFunc),
        Print(PrintLibFunc),
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
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
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
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![
                        // Error reason
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for starting roll
#[derive(Default)]
pub struct StartRollLibFunc {}
impl NoGenericArgsGenericLibfunc for StartRollLibFunc {
    const STR_ID: &'static str = "start_roll";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Block number
                ParamSignature::new(felt_ty.clone()),
                // Target contract address
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for stopping roll
#[derive(Default)]
pub struct StopRollLibFunc {}
impl NoGenericArgsGenericLibfunc for StopRollLibFunc {
    const STR_ID: &'static str = "stop_roll";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Target contract address
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for starting warp
#[derive(Default)]
pub struct StartWarpLibFunc {}
impl NoGenericArgsGenericLibfunc for StartWarpLibFunc {
    const STR_ID: &'static str = "start_warp";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Block number
                ParamSignature::new(felt_ty.clone()),
                // Target Address
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for starting warp
#[derive(Default)]
pub struct StopWarpLibFunc {}
impl NoGenericArgsGenericLibfunc for StopWarpLibFunc {
    const STR_ID: &'static str = "stop_warp";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // Target Address
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// LibFunc for starting a prank
#[derive(Default)]
pub struct StartPrankLibFunc {}
impl NoGenericArgsGenericLibfunc for StartPrankLibFunc {
    const STR_ID: &'static str = "start_prank";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;

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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
    const STR_ID: &'static str = "invoke_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // contract_address
                ParamSignature::new(felt_ty.clone()),
                // function_name
                ParamSignature::new(felt_ty.clone()),
                // calldata
                ParamSignature::new(snapshot_ty(context, arr_ty.clone())?),
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
                        // Panic data
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // contract_address
                ParamSignature::new(felt_ty.clone()),
                // function_name
                ParamSignature::new(felt_ty.clone()),
                // response
                ParamSignature::new(snapshot_ty(context, arr_ty.clone())?),
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
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
    const STR_ID: &'static str = "deploy_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // prepared_contract_address
                ParamSignature::new(felt_ty.clone()),
                // prepared_class_hash
                ParamSignature::new(felt_ty.clone()),
                // prepared_constructor_calldata
                ParamSignature::new(snapshot_ty(context, arr_ty.clone())?),
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
                        // Panic data
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
    const STR_ID: &'static str = "prepare_impl";
    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature::new(felt_ty.clone()),
                ParamSignature::new(snapshot_ty(context, arr_ty.clone())?),
            ],
            branch_signatures: vec![
                BranchSignature {
                    vars: vec![
                        // Constructor Calldata
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                        // Contract Address
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                        // Class Hash
                        OutputVarInfo {
                            ty: felt_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: felt_ty.clone(),
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
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
    const STR_ID: &'static str = "call_impl";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let felt_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt_ty.clone())?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // contract_address
                ParamSignature::new(felt_ty.clone()),
                // function_name
                ParamSignature::new(felt_ty.clone()),
                // calldata
                ParamSignature::new(snapshot_ty(context, arr_ty.clone())?),
            ],
            branch_signatures: vec![
                // Success branch
                BranchSignature {
                    vars: vec![
                        // Return Data
                        OutputVarInfo {
                            ty: arr_ty.clone(),
                            ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    // Panic data
                    vars: vec![OutputVarInfo {
                        ty: arr_ty.clone(),
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

#[derive(Default)]
pub struct PrintLibFunc {}
impl NoGenericArgsGenericLibfunc for PrintLibFunc {
    const STR_ID: &'static str = "print";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        // TODO(spapini): We should get a StringView, which is something like
        // (Span<StringLimb>, len), or something like that.
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let arr_type = context.get_wrapped_concrete_type(ArrayType::id(), felt252_ty)?;
        Ok(LibfuncSignature::new_non_branch(
            vec![arr_type],
            vec![],
            SierraApChange::Known { new_vars_only: true },
        ))
    }
}
