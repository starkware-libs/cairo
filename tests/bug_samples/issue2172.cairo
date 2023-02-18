struct Node {
    value: felt,
    left: Option::<Box::<Node>>,
    right: Option::<Box::<Node>>,
}
