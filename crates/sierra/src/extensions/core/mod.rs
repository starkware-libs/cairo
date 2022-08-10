use self::gas::{GasBuiltinType, GasExtension};
use self::integer::{IntegerExtension, IntegerType};
use self::mem::MemExtension;
use self::unconditional_jump::UnconditionalJumpGeneric;
use crate::{define_extension_hierarchy, define_type_hierarchy};

pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_type_hierarchy! {
    pub enum CoreType {
        Gas(GasBuiltinType),
        Integer(IntegerType)
    }
}

define_extension_hierarchy! {
    pub enum CoreExtension {
        Gas(GasExtension),
        Integer(IntegerExtension),
        Mem(MemExtension),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
