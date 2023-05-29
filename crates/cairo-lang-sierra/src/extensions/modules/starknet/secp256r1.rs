use super::secp256::Secp256Trait;
use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

#[derive(Default)]
pub struct Secp256r1EcPointType {}
impl NoGenericArgsGenericType for Secp256r1EcPointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Secp256r1EcPoint");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const SIZE: i16 = 1;
}

#[derive(Default)]
pub struct Secp256r1;
impl Secp256Trait for Secp256r1 {
    const STR_ID_NEW: &'static str = "secp256r1_ec_new_syscall";
    const STR_ID_ADD: &'static str = "secp256r1_ec_add_syscall";
    const STR_ID_MUL: &'static str = "secp256r1_ec_mul_syscall";
    const STR_ID_GET_POINT_FROM_X: &'static str = "secp256r1_ec_get_point_from_x_syscall";
    const STR_ID_GET_COORDINATES: &'static str = "secp256r1_ec_get_coordinates_syscall";
    const TYPE_ID: GenericTypeId = GenericTypeId::new_inline("Secp256r1EcPoint");
    const TYPE_ID_STR: &'static str = "core::starknet::secp256r1::Secp256r1EcPoint";
}
