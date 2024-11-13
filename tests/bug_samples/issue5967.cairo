#[derive(Drop)]
pub enum Pattern {
    Leaf,
    Boxed: Box<Pattern>,
}

pub fn boxed(pat: Pattern) -> Pattern {
    return Pattern::Boxed(BoxTrait::new(pat));
}

pub fn boxed_leaf() -> Pattern {
    boxed(Pattern::Leaf)
}

#[inline(never)]
pub fn boxed_boxed_leaf() -> Pattern {
    boxed(boxed_leaf())
}

#[test]
fn test() {
    let _ = boxed_boxed_leaf();
}
