use crate::ids::GenericTypeId;
use crate::program::ConcreteTypeLongId;

pub fn no_args_long_id(name: &'static str) -> ConcreteTypeLongId {
    ConcreteTypeLongId { generic_id: GenericTypeId::new_inline(name), generic_args: Vec::new() }
}
