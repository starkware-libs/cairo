use self::gas::{GasBuiltinGeneric, GasLibFunc};
use self::integer::{IntegerGenericType, IntegerLibFunc};
use self::mem::MemLibFunc;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericLibFunc;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_type_hierarchy! {
    pub enum CoreType {
        GasBuiltin(GasBuiltinGeneric),
        Integer(IntegerGenericType),
    }, CoreConcreteType
}

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
