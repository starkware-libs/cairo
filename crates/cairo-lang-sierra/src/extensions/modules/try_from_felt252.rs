use std::marker::PhantomData;

use super::felt252::Felt252Type;
use super::range_check::RangeCheckType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, OutputVarReferenceInfo, SpecializationError,
};
use crate::ids::GenericTypeId;

/// Trait for implementing try_{ty}_from_felt252.
pub trait TryFromFelt252: Default {
    /// The try_{ty}_from_felt252 library function id.
    const STR_ID: &'static str;
    /// The id of the generic type to implement the library functions for.
    const GENERIC_TYPE_ID: GenericTypeId;
}
/// Libfunc for attempting to convert a felt252 into a uint.
#[derive(Default)]
pub struct TryFromFelt252Libfunc<TTryFromFelt252: TryFromFelt252> {
    _phantom: PhantomData<TTryFromFelt252>,
}
impl<TTryFromFelt252: TryFromFelt252> NoGenericArgsGenericLibfunc
    for TryFromFelt252Libfunc<TTryFromFelt252>
{
    const STR_ID: &'static str = TTryFromFelt252::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let range_check_type = context.get_concrete_type(RangeCheckType::id(), &[])?;
        Ok(LibfuncSignature {
            param_signatures: vec![
                ParamSignature {
                    ty: range_check_type.clone(),
                    allow_deferred: false,
                    allow_add_const: true,
                    allow_const: false,
                },
                ParamSignature::new(context.get_concrete_type(Felt252Type::id(), &[])?),
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
                            ty: context.get_concrete_type(TTryFromFelt252::GENERIC_TYPE_ID, &[])?,
                            ref_info: OutputVarReferenceInfo::SameAsParam { param_idx: 1 },
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                BranchSignature {
                    vars: vec![OutputVarInfo {
                        ty: range_check_type,
                        ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::AddConst {
                            param_idx: 0,
                        }),
                    }],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}
