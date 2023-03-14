use array::ArrayTrait;
fn main() {
    let mut a = ArrayTrait::<felt252>::new();
    assert(a.len() == 0_usize, 'Array length is not 0');
    assert(a.is_empty() == true, 'Array is not empty');
}
