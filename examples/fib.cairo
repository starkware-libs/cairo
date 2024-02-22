fn foo(ref a: Array<u32>) -> u32 {
    match a.pop_front() {
        Option::Some(x) => x,
        Option::None => 0
    }   
}
