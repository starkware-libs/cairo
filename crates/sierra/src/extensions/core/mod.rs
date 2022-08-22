use self::gas::GasLibFunc;
use self::integer::IntegerLibFunc;
use self::mem::MemLibFunc;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericLibFunc;
use crate::define_libfunc_hierarchy;

pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
