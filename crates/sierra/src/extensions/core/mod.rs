use self::gas::GasExtension;
use self::integer::IntegerExtension;
use self::mem::MemExtension;
use self::unconditional_jump::UnconditionalJumpGeneric;
use super::GenericExtension;
use crate::super_extension;

mod gas;
mod integer;
mod mem;
mod unconditional_jump;

super_extension! {
    pub enum CoreExtension {
        Gas(GasExtension),
        Integer(IntegerExtension),
        Mem(MemExtension),
        UnconditionalJump(UnconditionalJumpGeneric)
    }, CoreConcrete
}
