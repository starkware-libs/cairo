use super::syscalls::SyscallGenericLibfunc;
use crate::define_libfunc_hierarchy;
use crate::extensions::enm::EnumType;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::modules::{get_bool_type, get_u256_type, get_unit_type};
use crate::extensions::{NamedType, NoGenericArgsGenericType, SpecializationError};
use crate::ids::{GenericTypeId, UserTypeId};
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum Secp256K1EcLibfunc {
         Add(Secp256K1EcAddLibfunc),
         Mul(Secp256K1EcMulLibfunc),
         GetPointFromX(Secp256K1EcGetPointFromXLibfunc),
    }, Secp256K1EcConcreteLibfunc
}

#[derive(Default)]
pub struct Secp256K1EcPointType {}
impl NoGenericArgsGenericType for Secp256K1EcPointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Secp256K1EcPoint");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

/// Libfunc for a secp256k1 elliptic curve addition system call.
#[derive(Default)]
pub struct Secp256K1EcAddLibfunc {}
impl SyscallGenericLibfunc for Secp256K1EcAddLibfunc {
    const STR_ID: &'static str = "secp256k1_ec_add_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let secp256k1_ec_point_type = context.get_concrete_type(Secp256K1EcPointType::id(), &[])?;

        // Point `p0`, point `p1`
        Ok(vec![secp256k1_ec_point_type.clone(), secp256k1_ec_point_type])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(Secp256K1EcPointType::id(), &[])?])
    }
}

/// Libfunc for a secp256k1 elliptic curve multiplication system call.
#[derive(Default)]
pub struct Secp256K1EcMulLibfunc {}
impl SyscallGenericLibfunc for Secp256K1EcMulLibfunc {
    const STR_ID: &'static str = "secp256k1_ec_mul_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(Secp256K1EcPointType::id(), &[])?,
            // Scalar `m`.
            get_u256_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(Secp256K1EcPointType::id(), &[])?])
    }
}

/// System call libfunc for getting a point on the secp256k1 elliptic curve, according to the given
/// `x` coordinate and the parity of the relevant y coordinate.
#[derive(Default)]
pub struct Secp256K1EcGetPointFromXLibfunc {}
impl SyscallGenericLibfunc for Secp256K1EcGetPointFromXLibfunc {
    const STR_ID: &'static str = "secp256k1_ec_get_point_from_x_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // `x` coordinate.
            get_u256_type(context)?,
            // `y_parity` - parity of the relevant y coordinate.
            get_bool_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let secp256k1_ec_point_type = context.get_concrete_type(Secp256K1EcPointType::id(), &[])?;

        let unit_type = get_unit_type(context)?;
        // TODO(yuval): add get_option_type to mod.rs and use it here.
        let option_secp256k1_ec_point_type = context.get_concrete_type(
            EnumType::id(),
            &[
                GenericArg::UserType(UserTypeId::from_string(
                    "core::option::Option::<core::starknet::secp256k1::Secp256K1EcPoint>",
                )),
                GenericArg::Type(secp256k1_ec_point_type),
                GenericArg::Type(unit_type),
            ],
        )?;
        Ok(vec![option_secp256k1_ec_point_type])
    }
}
