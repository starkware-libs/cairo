use array::ArrayTrait;

extern fn print(message: Array::<felt>) nopanic;

fn print_felt(message: felt) {
    let mut arr = ArrayTrait::new();
    arr.append(message);
    print(arr);
}
