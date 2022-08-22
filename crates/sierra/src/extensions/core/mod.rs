use self::function_call::FunctionCallGeneric;
use self::gas::{GasBuiltinGeneric, GasLibFunc};
use self::integer::{IntegerGenericType, IntegerLibFunc};
use self::mem::MemLibFunc;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericLibFunc;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod function_call;
pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_type_hierarchy! {
    pub enum CoreType {
        GasBuiltin(GasBuiltinGeneric),
        Integer(IntegerGenericType),
    }, CoreTypeConcrete
}

// TODO(orizi): Improve naming of this hierarchy. e.g. CoreConcrete should have LibFunc in its name.
define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        FunctionCall(FunctionCallGeneric),
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
