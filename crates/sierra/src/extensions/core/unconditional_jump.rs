use crate::extensions::{ConcreteExtension, NoGenericArgsGenericExtension};

#[derive(Default)]
pub struct UnconditionalJumpGeneric {}
impl NoGenericArgsGenericExtension for UnconditionalJumpGeneric {
    type Concrete = UnconditionalJumpConcrete;
    const NAME: &'static str = "jump";
    fn specialize(&self) -> Self::Concrete {
        UnconditionalJumpConcrete {}
    }
}

pub struct UnconditionalJumpConcrete {}
impl ConcreteExtension for UnconditionalJumpConcrete {}
