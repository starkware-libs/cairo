#[derive(Drop)]
struct Node {
    value: felt252,
    left: Option<Box<Node>>,
}

#[test]
fn simple_test() {
    let bst = Node { value: 12, left: None };

    assert_eq!(bst.value, 12);
    assert!(bst.left.is_none(), "left should be none");
}

#[derive(Copy, Drop)]
pub enum RecursiveType {
    Simple: felt252,
    Direct: Span<RecursiveType>,
    ExtraHop: Span<(felt252, RecursiveType)>,
}

#[inline(never)]
fn use_value(_value: RecursiveType) {}

#[test]
fn other_test() {
    use_value(RecursiveType::Simple(12));
}
