use std::marker::PhantomData;

use super::secp256k1::{Secp256k1, Secp256k1PointType};
use super::secp256r1::{Secp256r1, Secp256r1PointType};
use super::syscalls::SyscallGenericLibfunc;
use crate::extensions::enm::EnumType;
use crate::extensions::lib_func::SignatureSpecializationContext;
use crate::extensions::modules::{get_bool_type, get_u256_type, get_unit_type};
use crate::extensions::{NamedType, SpecializationError};
use crate::ids::{GenericTypeId, UserTypeId};
use crate::program::GenericArg;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum Secp256PointType {
        K1(Secp256k1PointType),
        R1(Secp256r1PointType),
    }, Secp256PointTypeConcrete
}

pub trait Secp256Trait: Default {
    const STR_ID_NEW: &'static str;
    const STR_ID_ADD: &'static str;
    const STR_ID_MUL: &'static str;
    const STR_ID_GET_POINT_FROM_X: &'static str;
    const STR_ID_GET_XY: &'static str;
    const TYPE_ID: GenericTypeId;
    const TYPE_ID_STR: &'static str;
}
define_libfunc_hierarchy! {
    pub enum Secp256Libfunc {
        K1(Secp256OpLibfunc<Secp256k1>),
        R1(Secp256OpLibfunc<Secp256r1>),
    }, Secp256ConcreteLibfunc
}
define_libfunc_hierarchy! {
    pub enum Secp256OpLibfunc<T: Secp256Trait> {
        New(Secp256NewLibfunc<T>),
        Add(Secp256AddLibfunc<T>),
        Mul(Secp256MulLibfunc<T>),
        GetPointFromX(Secp256GetPointFromXLibfunc<T>),
        GetXy(Secp256GetXyLibfunc<T>),
    }, Secp256OpConcreteLibfunc
}

/// System call libfunc for creating a point on the secp256 elliptic curve.
#[derive(Default)]
pub struct Secp256NewLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256NewLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_NEW;

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
        optional_secp256_ec_point_return_type::<T>(context)
    }
}

/// Libfunc for a secp256 elliptic curve addition system call.
#[derive(Default)]
pub struct Secp256AddLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256AddLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_ADD;

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        let secp256_ec_point_type = context.get_concrete_type(T::TYPE_ID, &[])?;

        // Point `p0`, point `p1`.
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
pub struct Secp256MulLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256MulLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_MUL;

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(T::TYPE_ID, &[])?,
            // Scalar `scalar`.
            get_u256_type(context)?,
        ])
    }

    fn success_output_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![context.get_concrete_type(T::TYPE_ID, &[])?])
    }
}

/// System call libfunc for getting a point on the secp256 elliptic curve, according to the given
/// `x` coordinate and the parity of the relevant y coordinate.
#[derive(Default)]
pub struct Secp256GetPointFromXLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256GetPointFromXLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_GET_POINT_FROM_X;

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
        optional_secp256_ec_point_return_type::<T>(context)
    }
}

/// System call libfunc for getting the coordinates of a point on the secp256 elliptic curve.
#[derive(Default)]

pub struct Secp256GetXyLibfunc<T: Secp256Trait> {
    _phantom: PhantomData<T>,
}
impl<T: Secp256Trait> SyscallGenericLibfunc for Secp256GetXyLibfunc<T> {
    const STR_ID: &'static str = T::STR_ID_GET_XY;

    fn input_tys(
        context: &dyn SignatureSpecializationContext,
    ) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
        Ok(vec![
            // Point `p`.
            context.get_concrete_type(T::TYPE_ID, &[])?,
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

/// Returns a single return type of `Option<Secp256Point>`.
fn optional_secp256_ec_point_return_type<T: Secp256Trait>(
    context: &dyn SignatureSpecializationContext,
) -> Result<Vec<crate::ids::ConcreteTypeId>, SpecializationError> {
    let secp256_ec_point_type = context.get_concrete_type(T::TYPE_ID, &[])?;

    let unit_type = get_unit_type(context)?;
    // TODO(yuval): add get_option_type to mod.rs and use it here.
    let option_secp256_ec_point_type = context.get_concrete_type(
        EnumType::id(),
        &[
            GenericArg::UserType(UserTypeId::from_string(format!(
                "core::option::Option::<{}>",
                T::TYPE_ID_STR
            ))),
            GenericArg::Type(secp256_ec_point_type),
            GenericArg::Type(unit_type),
        ],
    )?;
    Ok(vec![option_secp256_ec_point_type])
}
