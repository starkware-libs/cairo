fn contains<T, impl TPartialEq: PartialEq<T>, +Drop<T>, +Copy<T>>(
    ref self: Array<T>, item: T,
) -> bool {
    let mut index = 0_usize;
    loop {
        if index >= self.len() {
            break false;
        } else if *self[index] == item {
            break true;
        } else {
            index = index + 1_usize;
        }
    }
}

#[test]
fn test_contains() {
    let mut arr: Array<felt252> = array![1, 2, 3, 4];
    assert!(contains(ref arr, 1));
    assert!(contains(ref arr, 2));
    assert!(contains(ref arr, 3));
    assert!(contains(ref arr, 4));
    assert!(!contains(ref arr, 5));
    assert!(!contains(ref arr, 6));
    assert!(!contains(ref arr, 7));
    assert!(!contains(ref arr, 8));
}
