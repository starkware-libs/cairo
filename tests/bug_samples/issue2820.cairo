fn foo(ref item_index: usize, index: usize) -> usize {
    if 5_usize < 7 {
        item_index = index;
    }
    index
}

#[test]
fn main() {
    let mut x = 0;
    foo(ref x, 0);
}
