use super::secp256::Secp256Trait;
use crate::extensions::NoGenericArgsGenericType;
use crate::ids::GenericTypeId;

#[derive(Default)]
pub struct Secp256r1PointType {}
impl NoGenericArgsGenericType for Secp256r1PointType {
    const ID: GenericTypeId = GenericTypeId::new_inline("Secp256r1Point");
    const STORABLE: bool = true;
    const DUPLICATABLE: bool = true;
    const DROPPABLE: bool = true;
    const ZERO_SIZED: bool = false;
}

#[derive(Default)]
pub struct Secp256r1;
impl Secp256Trait for Secp256r1 {
    const STR_ID_NEW: &'static str = "secp256r1_new_syscall";
    const STR_ID_ADD: &'static str = "secp256r1_add_syscall";
    const STR_ID_MUL: &'static str = "secp256r1_mul_syscall";
    const STR_ID_GET_POINT_FROM_X: &'static str = "secp256r1_get_point_from_x_syscall";
    const STR_ID_GET_XY: &'static str = "secp256r1_get_xy_syscall";
    const TYPE_ID: GenericTypeId = Secp256r1PointType::ID;
    const TYPE_ID_STR: &'static str = "core::starknet::secp256r1::Secp256r1Point";
}
