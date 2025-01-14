pub trait WithAssociated<T> {
    type S;
    fn foo<+Drop<T>, +Drop<Self::S>>(self: T, s: Self::S) -> Self::S {
        s
    }
}

impl Impl<T> of WithAssociated<T> {
    type S = u8;
}

#[test]
fn test_associated_type_usage() {
    WithAssociated::foo(4, 5_u8);
    4.foo(5_u8);
}
