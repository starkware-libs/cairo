#[derive(Drop)]
struct ZeroSized {}

#[inline(never)]
fn use_zero_sized(ref value: ZeroSized) -> felt252 {
    0
}

fn foo(ref value: ZeroSized, array_with_data: Array<felt252>) {
    if 1 == 0 {
        use_zero_sized(ref value);
    }
    // Unknown ap change tracking, while not using `value`.
    let mut data_span = array_with_data.span();
    let _a: Option<Array<Array<felt252>>> = Serde::deserialize(ref data_span);
}

#[test]
fn call_foo() {
    let mut value = ZeroSized {};
    foo(ref value, array![0]);
}
