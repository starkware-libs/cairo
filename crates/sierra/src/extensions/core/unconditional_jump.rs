use crate::extensions::{
    ConcreteExtension, ConcreteExtensionBox, GenericExtensionBox, NoGenericArgsAbstractExtension,
};
use crate::program::GenericExtensionId;

struct UnconditionalJumpGeneric {}
impl NoGenericArgsAbstractExtension for UnconditionalJumpGeneric {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnconditionalJumpConcrete {})
    }
}

struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {}

pub(super) fn extensions() -> [(GenericExtensionId, GenericExtensionBox); 1] {
    [("jump".into(), Box::new(UnconditionalJumpGeneric {}))]
}
