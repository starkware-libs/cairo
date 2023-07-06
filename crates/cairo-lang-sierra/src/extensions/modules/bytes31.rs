use num_bigint::BigInt;

use super::consts::{ConstGenLibfunc, WrapConstGenLibfunc};
use super::try_from_felt252::{TryFromFelt252, TryFromFelt252Libfunc};
use super::utils::reinterpret_cast_signature;
use crate::define_libfunc_hierarchy;
use crate::extensions::felt252::Felt252Type;
use crate::extensions::lib_func::{LibfuncSignature, SignatureSpecializationContext};
use crate::extensions::{
    NamedType, NoGenericArgsGenericLibfunc, NoGenericArgsGenericType, SpecializationError,
};
use crate::ids::GenericTypeId;

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
pub struct Bytes31ConstLibfuncWrapped {}
impl ConstGenLibfunc for Bytes31ConstLibfuncWrapped {
    const STR_ID: &'static str = "bytes31_const";
    const GENERIC_TYPE_ID: GenericTypeId = <Bytes31Type as NoGenericArgsGenericType>::ID;

    fn bound() -> BigInt {
        BigInt::from(2).pow(248)
    }
}
pub type Bytes31ConstLibfunc = WrapConstGenLibfunc<Bytes31ConstLibfuncWrapped>;

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
