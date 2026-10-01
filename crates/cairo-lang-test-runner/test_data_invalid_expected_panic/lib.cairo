#[test]
#[should_panic(expected: "café")]
fn non_ascii() {
    panic!("cafe");
}

#[test]
#[should_panic(expected: ('a', "bad\q"))]
fn bad_escape() {
    panic!("bad");
}
