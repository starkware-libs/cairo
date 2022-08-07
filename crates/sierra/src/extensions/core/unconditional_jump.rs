use crate::extensions::{ConcreteExtension, ConcreteExtensionBox, ExtensionBox, NoArgsExtension};
use crate::program::ExtensionId;

struct UnconditionalJumpExtension {}
impl NoArgsExtension for UnconditionalJumpExtension {
    fn specialize(&self) -> ConcreteExtensionBox {
        Box::new(UnconditionalJump {})
    }
}

struct UnconditionalJump {}
impl ConcreteExtension for UnconditionalJump {}

pub(super) fn extensions() -> [(ExtensionId, ExtensionBox); 1] {
    [(ExtensionId::Name("jump".into()), Box::new(UnconditionalJumpExtension {}))]
}
