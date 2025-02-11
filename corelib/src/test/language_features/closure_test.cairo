#[test]
fn array_map_test() {
    let outer_var = 42;
    let _closure_annotated = |i: u32| -> u32 {
        i + outer_var
    };
    let _a = _closure_annotated(1);
}

