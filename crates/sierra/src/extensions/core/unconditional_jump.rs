use crate::extensions::{ConcreteExtension, ConcreteExtensionBox, ExtensionBox, NoArgsExtension};
use crate::program::ExtensionId;

struct UnconditionalJumpExtension {}
impl NoArgsExtension for UnconditionalJumpExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnconditionalJumpConcrete {})
    }
}

struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 1] {
    [("jump".into(), Box::new(UnconditionalJumpExtension {}))]
}
