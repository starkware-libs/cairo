// The decision whether to specialize bar triggers a cycle, the current workaround is not to
// specialize in this case.
pub fn foo(a: felt252) -> felt252 {
    bar(a, 3) + bar(3, a)
}


pub fn bar(a: felt252, b: felt252) -> felt252 {
    foo(a) + 1
}


#[test]
#[available_gas(10000)]
#[should_panic(expected: 'Out of gas')]
fn call_foo() {
    foo(8);
}
