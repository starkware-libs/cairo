extern fn print(message: Array::<felt>) nopanic;

fn print_felt(message: felt) {
    let mut arr = array_new::<felt>();
    array_append::<felt>(ref arr, message);
    print(arr);
}
