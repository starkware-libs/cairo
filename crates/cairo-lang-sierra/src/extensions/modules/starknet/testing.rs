use num_bigint::BigInt;

use super::felt252_span_ty;
use super::interoperability::ContractAddressType;
use crate::define_libfunc_hierarchy;
use crate::extensions::lib_func::{
    BranchSignature, LibfuncSignature, OutputVarInfo, ParamSignature, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo,
    SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::program::GenericArg;

#[derive(Default)]
pub struct PopLogLibfunc {}

impl NoGenericArgsGenericLibfunc for PopLogLibfunc {
    const STR_ID: &'static str = "pop_log";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let contract_address_ty = context.get_concrete_type(ContractAddressType::id(), &[])?;
        let span_ty = felt252_span_ty(context)?;

        Ok(LibfuncSignature {
            param_signatures: vec![ParamSignature::new(contract_address_ty)],
            branch_signatures: vec![
                // Some variant branch.
                BranchSignature {
                    vars: vec![
                        // keys
                        OutputVarInfo {
                            ty: span_ty.clone(),
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                        // data
                        OutputVarInfo {
                            ty: span_ty,
                            ref_info: OutputVarReferenceInfo::SimpleDerefs,
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // None variant branch.
                BranchSignature {
                    vars: vec![],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for creating a general cheatcode.
#[derive(Default)]
pub struct CheatcodeLibfunc {}
impl NamedLibfunc for CheatcodeLibfunc {
    type Concrete = CheatcodeConcreteLibfunc;
    const STR_ID: &'static str = "cheatcode";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        if args.len() != 1 {
            return Err(SpecializationError::WrongNumberOfGenericArgs);
        }

        let span_ty = felt252_span_ty(context)?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                // input
                ParamSignature::new(span_ty.clone()),
            ],
            branch_signatures: vec![BranchSignature {
                vars: vec![
                    // output
                    OutputVarInfo {
                        ty: span_ty,
                        ref_info: OutputVarReferenceInfo::NewTempVar { idx: 0 },
                    },
                ],
                ap_change: SierraApChange::Known { new_vars_only: true },
            }],
            fallthrough: Some(0),
        })
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(selector)] => Ok(CheatcodeConcreteLibfunc {
                selector: selector.clone(),
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            [_] => Err(SpecializationError::UnsupportedGenericArg),
            _ => Err(SpecializationError::WrongNumberOfGenericArgs),
        }
    }
}

pub struct CheatcodeConcreteLibfunc {
    pub selector: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for CheatcodeConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

define_libfunc_hierarchy! {
    pub enum TestingLibfunc {
         PopLog(PopLogLibfunc),
         Cheatcode(CheatcodeLibfunc),
    }, TestingConcreteLibfunc
}
