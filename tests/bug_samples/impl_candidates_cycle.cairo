trait Introspect<T> {
}

#[derive(Drop)]
struct Generic<T, +Introspect<T>> {
    value: T,
}

#[test]
#[available_gas(2000000)]
fn test_generic_introspect() {
    let generic = Generic { value: 123 };
}

impl GenericStructIntrospect<T, impl IntrospectTImp: Introspect<T>> of Introspect<Generic<T, IntrospectTImp>> {}
impl Felt252Introspect of Introspect<felt252> {}
