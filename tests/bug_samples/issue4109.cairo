#[derive(Drop)]
struct Tag {
    children: Option<Array<Tag>>,
}

#[inline]
fn to_string(self: Tag) {
    build(self);
}

fn build(self: Tag) {
    let Tag { children: opt_children } = self;

    let mut children = opt_children.unwrap();

    to_string(children.pop_front().unwrap());
}


#[test]
#[available_gas(1000000)]
#[should_panic(expected: ('Option::unwrap failed.',))]
fn test_build_empty() {
    build(Tag { children: None });
}
