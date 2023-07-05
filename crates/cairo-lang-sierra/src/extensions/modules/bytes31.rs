use num_bigint::BigInt;

use super::try_from_felt252::{TryFromFelt252, TryFromFelt252Libfunc};
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{
    DeferredOutputKind, LibfuncSignature, OutputVarInfo, SierraApChange,
    SignatureSpecializationContext, SpecializationContext,
};
use crate::extensions::{
    NamedLibfunc, NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType,
    OutputVarReferenceInfo, SignatureBasedConcreteLibfunc, SpecializationError,
};
use crate::ids::GenericTypeId;
use crate::program::GenericArg;

/// Type for bytes31.
#[derive(Default)]
pub struct Bytes31Type {}
impl NoGenericArgsGenericType for Bytes31Type {
    const ID: GenericTypeId = GenericTypeId::new_inline("bytes31");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

define_libfunc_hierarchy! {
    pub enum Bytes31Libfunc {
        Const(Bytes31ConstLibfunc),
        ToFelt252(Bytes31ToFelt252Libfunc),
        TryFromFelt252(Bytes31FromFelt252Libfunc),
    }, Bytes31ConcreteLibfunc
}

/// Libfunc for creating a constant bytes31.
#[derive(Default)]
pub struct Bytes31ConstLibfunc {}
impl NamedLibfunc for Bytes31ConstLibfunc {
    type Concrete = Bytes31ConstConcreteLibfunc;
    const STR_ID: &'static str = "bytes31_const";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
        _args: &[GenericArg],
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(LibfuncSignature::new_non_branch(
            vec![],
            vec![OutputVarInfo {
                ty: context.get_concrete_type(Bytes31Type::id(), &[])?,
                ref_info: OutputVarReferenceInfo::Deferred(DeferredOutputKind::Const),
            }],
            SierraApChange::Known { new_vars_only: true },
        ))
    }

    fn specialize(
        &self,
        context: &dyn SpecializationContext,
        args: &[GenericArg],
    ) -> Result<Self::Concrete, SpecializationError> {
        match args {
            [GenericArg::Value(c)] => Ok(Bytes31ConstConcreteLibfunc {
                c: c.clone(),
                signature: <Self as NamedLibfunc>::specialize_signature(
                    self,
                    context.upcast(),
                    args,
                )?,
            }),
            _ => Err(SpecializationError::UnsupportedGenericArg),
        }
    }
}
pub struct Bytes31ConstConcreteLibfunc {
    pub c: BigInt,
    pub signature: LibfuncSignature,
}
impl SignatureBasedConcreteLibfunc for Bytes31ConstConcreteLibfunc {
    fn signature(&self) -> &LibfuncSignature {
        &self.signature
    }
}

/// Libfunc for converting a bytes31 into a felt252.
#[derive(Default)]
pub struct Bytes31ToFelt252Libfunc {}
impl NoGenericArgsGenericLibfunc for Bytes31ToFelt252Libfunc {
    const STR_ID: &'static str = "bytes31_to_felt252";

    fn specialize_signature(
        &self,
        context: &dyn SignatureSpecializationContext,
    ) -> Result<LibfuncSignature, SpecializationError> {
        Ok(reinterpret_cast_signature(
            context.get_concrete_type(Bytes31Type::id(), &[])?,
            context.get_concrete_type(Felt252Type::id(), &[])?,
        ))
    }
}

/// Libfunc for attempting to convert a felt252 into a bytes31.
#[derive(Default)]
pub struct Bytes31FromFelt252Trait;
impl TryFromFelt252 for Bytes31FromFelt252Trait {
    const STR_ID: &'static str = "bytes31_try_from_felt252";
    const GENERIC_TYPE_ID: GenericTypeId = <Bytes31Type as NoGenericArgsGenericType>::ID;
}

type Bytes31FromFelt252Libfunc = TryFromFelt252Libfunc<Bytes31FromFelt252Trait>;
