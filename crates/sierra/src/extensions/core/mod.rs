use self::function_call::FunctionCallLibFunc;
use self::gas::{GasBuiltinType, GasLibFunc};
use self::integer::{IntegerLibFunc, IntegerType};
use self::mem::{DeferredType, MemLibFunc};
use self::unconditional_jump::UnconditionalJumpLibFunc;
use super::GenericLibFunc;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

pub mod function_call;
pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_type_hierarchy! {
    pub enum CoreType {
        GasBuiltin(GasBuiltinType),
        Integer(IntegerType),
        Deferred(DeferredType),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibFunc {
        FunctionCall(FunctionCallLibFunc),
        Gas(GasLibFunc),
        Integer(IntegerLibFunc),
        Mem(MemLibFunc),
        UnconditionalJump(UnconditionalJumpLibFunc),
    }, CoreConcreteLibFunc
}
