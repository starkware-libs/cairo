use array::ArrayTrait;

#[derive(Drop)]
struct MyStruct {
    value: felt252,
    arr: Array<felt252>
}

#[test]
fn main() {
    let mut my_struct = MyStruct { value: 0, arr: ArrayTrait::<felt252>::new() };
    let result = sub_three(my_struct.value);
    my_struct.value = result;
}

fn sub_three(value: felt252) -> felt252 {
    value - 3
}
