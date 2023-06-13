use itertools::chain;

use super::interoperability::ClassHashType;
use super::u64_span_ty;
use crate::extensions::array::ArrayType;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::gas::GasBuiltinType;
use crate::extensions::lib_func::{
    BranchSignature, DeferredOutputKind, LibfuncSignature, OutputVarInfo, ParamSignature,
    SierraApChange, SignatureSpecializationContext,
};
use crate::extensions::modules::get_u256_type;
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, OutputVarReferenceInfo,
    SpecializationError,
};
use crate::ids::{ConcreteTypeId, GenericTypeId};

/// Type for Starknet system object.
/// Used to make system calls.
#[derive(Default)]
pub struct SystemType {}
impl NoGenericArgsGenericType for SystemType {
    const ID: GenericTypeId = GenericTypeId::new_inline("System");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = false;
    const DROPPABLE: bool = false;
    const ZERO_SIZED: bool = false;
}

/// Trait for implementing a library function for syscalls.
pub trait SyscallGenericLibfunc: Default {
    /// The library function id.
    const STR_ID: &'static str;
    /// The non implicits inputs for the libfunc.
    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<ConcreteTypeId>, SpecializationError>;
    /// The success case non implicits outputs of the libfunc.
    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<ConcreteTypeId>, SpecializationError>;
}

impl<T: SyscallGenericLibfunc> NoGenericArgsGenericLibfunc for T {
    const STR_ID: &'static str = T::STR_ID;

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        let gas_builtin_ty = context.get_concrete_type(GasBuiltinType::id(), &[])?;
        let system_ty = context.get_concrete_type(SystemType::id(), &[])?;
        let felt252_ty = context.get_concrete_type(Felt252Type::id(), &[])?;
        let felt252_array_ty = context.get_wrapped_concrete_type(ArrayType::id(), felt252_ty)?;

        let gb_output_info = OutputVarInfo {
            ty: gas_builtin_ty.clone(),
            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
        };
        let system_output_info = OutputVarInfo::new_builtin(system_ty.clone(), 1);
        Ok(LibfuncSignature {
            param_signatures: chain!(
                [
                    // Gas builtin
                    ParamSignature::new(gas_builtin_ty),
                    // System
                    ParamSignature::new(system_ty).with_allow_add_const(),
                ],
                T::input_tys(context)?.into_iter().map(ParamSignature::new)
            )
            .collect(),
            branch_signatures: vec![
                // Success branch.
                BranchSignature {
                    vars: chain!(
                        [
                            // Gas builtin
                            gb_output_info.clone(),
                            // System
                            system_output_info.clone()
                        ],
                        T::success_output_tys(context)?.into_iter().map(|ty| OutputVarInfo {
                            ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        })
                    )
                    .collect(),
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
                // Failure branch.
                BranchSignature {
                    vars: vec![
                        // Gas builtin
                        gb_output_info,
                        // System
                        system_output_info,
                        // Revert reason
                        OutputVarInfo {
                            ty: felt252_array_ty,
                            ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Generic),
                        },
                    ],
                    ap_change: SierraApChange::Known { new_vars_only: false },
                },
            ],
            fallthrough: Some(0),
        })
    }
}

/// Libfunc for the replace_class system call.
#[derive(Default)]
pub struct ReplaceClassLibfunc {}
impl SyscallGenericLibfunc for ReplaceClassLibfunc {
    const STR_ID: &'static str = "replace_class_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let class_hash_ty = context.get_concrete_type(ClassHashType::id(), &[])?;
        Ok(vec![
            // class_hash
            class_hash_ty,
        ])
    }

    fn success_output_tys(
        _context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![])
    }
}

/// Libfunc for the keccak system call.
/// The libfunc does not add any padding and the input needs to be a multiple of 1088 bits
/// (== 17 u64 word).
#[derive(Default)]
pub struct KeccakLibfunc {}
impl SyscallGenericLibfunc for KeccakLibfunc {
    const STR_ID: &'static str = "keccak_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // input
            u64_span_ty(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![get_u256_type(context)?])
    }
}
