struct Node {
    value: felt252,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn traverse(node: Node) {
    let Node{value, left, right } = node;
    match left {
        Option::Some(x) => traverse(unbox(x)),
        Option::None(_) => {},
    }
    match right {
        Option::Some(x) => traverse(unbox(x)),
        Option::None(_) => {},
    }
}
