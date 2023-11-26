#[derive(Drop)]
struct Attribute {
    name: ByteArray,
    value: ByteArray
}

#[derive(Drop)]
struct Tag {
    attrs: Option<Array<Attribute>>,
    children: Option<Array<Tag>>,
    content: Option<ByteArray>
}

fn build(tag: Tag) -> ByteArray {
    let mut s = "<";

    let mut _attrs = tag.attrs.unwrap();
    // let no_content = tag.children.is_none() && tag.content.is_none();
    let no_content = tag.children.is_none();
    let no_content = no_content && tag.content.is_none();

    if no_content {
        return s + " />";
    } else {
        s += ">";
    }
    s
}
