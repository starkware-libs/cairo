use crate::extensions::{ConcreteExtension, NoGenericArgsGenericExtension};
use crate::ids::GenericExtensionId;

pub struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericExtension for UnconditionalJumpGeneric {
    type Concrete = UnconditionalJumpConcrete;
    fn id() -> Option<GenericExtensionId> {
        Some("jump".into())
    }
    fn new() -> Option<Self> {
        Some(Self {})
    }
    fn specialize(&self) -> Self::Concrete {
        UnconditionalJumpConcrete {}
    }
}

pub struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {}
