#[derive(Drop)]
struct Node {
    value: felt252,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn traverse(node: Node) {
    let Node { value: _, left, right } = node;
    match left {
        Option::Some(x) => traverse(x.unbox()),
        Option::None => {},
    }
    match right {
        Option::Some(x) => traverse(x.unbox()),
        Option::None => {},
    }
}
