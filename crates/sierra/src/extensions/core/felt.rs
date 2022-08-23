use crate::extensions::{ConcreteType, NoGenericArgsGenericType};


/// Type for felt.
/// The native type of the Cairo architecture.
#[derive(Default)]
pub struct FeltGenericType {}
impl NoGenericArgsGenericType for FeltGenericType {
    type Concrete = FeltConcreteType;
    const NAME: &'static str = "felt";
}
#[derive(Default)]
pub struct FeltConcreteType {}
impl ConcreteType for FeltConcreteType {}
