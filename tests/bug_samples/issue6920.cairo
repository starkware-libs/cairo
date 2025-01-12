#[test]
fn uninlined_closure_test() {
    let uninlined_closure = || {
        let a: felt252 = 256;
        format!("wow such amazing calcs {a} wow such amazing calcs {a} wow such amazing calcs {a}");
    };
    let _ = uninlined_closure();
}
