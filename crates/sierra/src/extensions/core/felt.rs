use crate::extensions::{ConcreteType, NoGenericArgsGenericType};

#[derive(Default)]
pub struct FeltGenericType {}
impl NoGenericArgsGenericType for FeltGenericType {
    type Concrete = FeltConcreteType;
    const NAME: &'static str = "felt";
}
#[derive(Default)]
pub struct FeltConcreteType {}
impl ConcreteType for FeltConcreteType {}
