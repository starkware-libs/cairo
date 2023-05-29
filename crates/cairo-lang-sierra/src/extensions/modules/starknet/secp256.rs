use std::marker::PhantomData;

use super::secp256k1::{Secp256K1, Secp256k1EcPointType};
use super::secp256r1::Secp256r1EcPointType;
use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::modules::get_u256_type;
use crate::extensions::{NamedType, NoGenericArgsGenericType, SpecializationError};
use crate::ids::GenericTypeId;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum Secp256EcPointType {
        K1(Secp256k1EcPointType),
        R1(Secp256r1EcPointType),
    }, Secp256EcPointTypeConcrete
}

pub trait Secp256Trait: Default {
    const STR_ID_ADD: &'static str;
    const STR_ID_MUL: &'static str;
    const TYPE_ID: GenericTypeId;
}
define_libfunc_hierarchy! {
    pub enum Secp256EcLibfunc {
        K1(Secp256EcOpLibfunc<Secp256K1>),
        // TODO(yg): add r1
        // R1(Secp256EcLibfunc<Secp256R1>),
    }, Secp256EcConcreteLibfunc
}
define_libfunc_hierarchy! {
    pub enum Secp256EcOpLibfunc<T: Secp256Trait> {
        Add(Secp256EcAddLibfunc<T>),
        Mul(Secp256EcMulLibfunc<T>),
        // TODO(yg): add the rest
    }, Secp256EcOpConcreteLibfunc
}

/// Libfunc for a secp256 elliptic curve addition system call.
#[derive(Default)]
pub struct Secp256EcAddLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256EcAddLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_ADD;

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let secp256_ec_point_type = context.get_concrete_type(T::TYPE_ID, &[])?;

        // Point `p0`, point `p1`
        Ok(vec![secp256_ec_point_type.clone(), secp256_ec_point_type])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(T::TYPE_ID, &[])?])
    }
}

/// Libfunc for a secp256 elliptic curve multiplication system call.
#[derive(Default)]
pub struct Secp256EcMulLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256EcMulLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_MUL;

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(T::TYPE_ID, &[])?,
            // Scalar `m`.
            get_u256_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(T::TYPE_ID, &[])?])
    }
}

// TODO(yg): add the rest
