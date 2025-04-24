#[test]
fn stack_overflow() {
    let outer_var = 42;
    let closure_annotated = |i: u32| -> u32 {
        i + outer_var
    };
    let _v = closure_annotated(1);
}
