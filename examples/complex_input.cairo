#[derive(Drop)]
struct A {
    val: u256,
    arr: Array<u256>,
}

fn complex_input(
    felt_input: felt252,
    mut felt_arr_input: Array<felt252>,
    mut a_input: A,
    mut a_arr_input: Array<A>,
) -> u256 {
    let mut r: u256 = felt_input.into();
    while let Some(x) = felt_arr_input.pop_front() {
        r += x.into();
    }
    r += a_input.val;
    while let Some(x) = a_input.arr.pop_front() {
        r += x;
    }
    while let Some(mut a) = a_arr_input.pop_front() {
        r += a.val;
        while let Some(x) = a.arr.pop_front() {
            r += x;
        }
    }
    r
}
