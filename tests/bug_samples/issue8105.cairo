#[test]
fn use_const_popped_value_test() -> bool {
    let stack: Array<u32> = array![];
    let mut span = stack.span();
    let _ = span.pop_back();
    let actual = span.into();
    let expected = array![];
    expected == actual
}
