#[test]
fn main() {
    let mut a = ArrayTrait::<felt252>::new();
    assert_eq!(a.len(), 0_usize);
    assert_eq!(a.is_empty(), true);
}
