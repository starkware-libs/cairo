fn foo(x: @u32) {
    *x + 1;
    if true {
        *x + 2;
    }
}

#[test]
fn main() {
    foo(@4);
}
