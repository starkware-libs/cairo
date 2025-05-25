fn find_value_iterative(mut arr: Span<felt252>, value: felt252) -> Option<usize> {
    let mut result = None;
    let mut index = 0;

    while let Some(array_value) = arr.pop_front() {
        if (*array_value == value) {
            return Some(index);
        }

        index += 1;
    }

    result
}

#[test]
fn call_early_return() {
    let _ = find_value_iterative([].span(), 0);
}
