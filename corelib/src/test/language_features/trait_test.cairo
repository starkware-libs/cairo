trait MyTrait<T> {
    fn foo(t: T) -> T {
        t
    }
}
impl MyImplU64 of MyTrait<u64>;
impl MyImplU32 of MyTrait<u32> {
    fn foo(t: u32) -> u32 {
        t + 1
    }
}

#[test]
fn test_default_implementation_and_override() {
    assert!(MyTrait::foo(5_u64) == 5);
    assert!(MyTrait::foo(10_u32) == 11);
}
