use array::ArrayTrait;
fn main() {
    let mut a = ArrayTrait::<felt252>::new();
    assert_eq(a.len(), 0_usize, 'Array length is not 0');
    assert_eq(a.is_empty(), true, 'Array is not empty');
}
