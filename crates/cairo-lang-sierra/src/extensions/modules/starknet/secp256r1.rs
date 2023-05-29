use super::secp256::Secp256EcPointType;
use super::syscalls::SyscallGenericLibfunc;
use crate::define_libfunc_hierarchy;
use crate::extensions::enm::EnumType;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::modules::{get_bool_type, get_u256_type, get_unit_type};
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::UserTypeId;
use crate::program::GenericArg;

define_libfunc_hierarchy! {
    pub enum Secp256R1EcLibfunc {
        New(Secp256R1EcNewLibfunc),
        Add(Secp256R1EcAddLibfunc),
        Mul(Secp256R1EcMulLibfunc),
        GetPointFromX(Secp256R1EcGetPointFromXLibfunc),
        GetCoordinates(Secp256R1EcGetCoordinatesLibfunc),
    }, Secp256R1EcConcreteLibfunc
}

/// System call libfunc for creating a point on the secp256r1 elliptic curve.
#[derive(Default)]
pub struct Secp256R1EcNewLibfunc {}
impl SyscallGenericLibfunc for Secp256R1EcNewLibfunc {
    const STR_ID: &'static str = "secp256r1_ec_new_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let u256_ty = get_u256_type(context)?;
        // `x` coordinate, `y` coordinate.
        Ok(vec![u256_ty.clone(), u256_ty])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        optional_secp256r1_ec_point_return_type(context)
    }
}

/// Libfunc for a secp256r1 elliptic curve addition system call.
#[derive(Default)]
pub struct Secp256R1EcAddLibfunc {}
impl SyscallGenericLibfunc for Secp256R1EcAddLibfunc {
    const STR_ID: &'static str = "secp256r1_ec_add_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let secp256r1_ec_point_type = context.get_concrete_type(Secp256EcPointType::id(), &[])?;

        // Point `p0`, point `p1`
        Ok(vec![secp256r1_ec_point_type.clone(), secp256r1_ec_point_type])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(Secp256EcPointType::id(), &[])?])
    }
}

/// Libfunc for a secp256r1 elliptic curve multiplication system call.
#[derive(Default)]
pub struct Secp256R1EcMulLibfunc {}
impl SyscallGenericLibfunc for Secp256R1EcMulLibfunc {
    const STR_ID: &'static str = "secp256r1_ec_mul_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(Secp256EcPointType::id(), &[])?,
            // Scalar `m`.
            get_u256_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(Secp256EcPointType::id(), &[])?])
    }
}

/// System call libfunc for getting a point on the secp256r1 elliptic curve, according to the given
/// `x` coordinate and the parity of the relevant y coordinate.
#[derive(Default)]
pub struct Secp256R1EcGetPointFromXLibfunc {}
impl SyscallGenericLibfunc for Secp256R1EcGetPointFromXLibfunc {
    const STR_ID: &'static str = "secp256r1_ec_get_point_from_x_syscall";

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
        optional_secp256r1_ec_point_return_type(context)
    }
}

/// System call libfunc for getting the coordinates of a point on the secp256r1 elliptic curve.
#[derive(Default)]
pub struct Secp256R1EcGetCoordinatesLibfunc {}
impl SyscallGenericLibfunc for Secp256R1EcGetCoordinatesLibfunc {
    const STR_ID: &'static str = "secp256r1_ec_get_coordinates_syscall";

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(Secp256EcPointType::id(), &[])?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // `x` coordinate.
            get_u256_type(context)?,
            // `y` coordinate.
            get_u256_type(context)?,
        ])
    }
}

/// Returns a single return type of `Option<Secp256EcPoint>`.
fn optional_secp256r1_ec_point_return_type(
    context: &dyn SignatureSpecializationContext,
) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
    let secp256r1_ec_point_type = context.get_concrete_type(Secp256EcPointType::id(), &[])?;

    let unit_type = get_unit_type(context)?;
    // TODO(yuval): add get_option_type to mod.rs and use it here.
    let option_secp256r1_ec_point_type = context.get_concrete_type(
        EnumType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string(
                "core::option::Option::<core::starknet::secp256::Secp256EcPoint>",
            )),
            GenericArg::Type(secp256r1_ec_point_type),
            GenericArg::Type(unit_type),
        ],
    )?;
    Ok(vec![option_secp256r1_ec_point_type])
}
