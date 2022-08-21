use self::gas::GasExtension;
use self::integer::IntegerExtension;
use self::mem::MemExtension;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericExtension;
use crate::define_extension_hierarchy;

pub mod gas;
pub mod integer;
pub mod mem;
pub mod unconditional_jump;

define_extension_hierarchy! {
    pub enum CoreExtension {
        Gas(GasExtension),
        Integer(IntegerExtension),
        Mem(MemExtension),
        UnconditionalJump(UnconditionalJumpGeneric),
    }, CoreConcrete
}
