use self::gas::GasLibcall;
use self::integer::IntegerLibcall;
use self::mem::MemLibcall;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericLibcall;
use crate::define_libcall_hierarchy;

pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_libcall_hierarchy! {
    pub enum CoreLibcall {
        Gas(GasLibcall),
        Integer(IntegerLibcall),
        Mem(MemLibcall),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
