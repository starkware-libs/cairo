#[derive(Drop)]
struct Node {
    value: felt252,
    left: Option::<Box<Node>>,
}

#[test]
fn simple_test() {
    let bst = Node { value: 12, left: Option::None, };

    assert_eq!(bst.value, 12);
    assert!(bst.left.is_none(), "left should be none");
}
