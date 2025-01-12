extern fn array_snapshot_pop_front<T>(ref arr: @Array<T>) -> Option<Box<@T>> nopanic;

fn fn1() -> (@Array<felt252>, felt252) {
    let mut data: @Array<felt252> = @array![0, 0x8c4fd0147, 0];
    let _ = array_snapshot_pop_front(ref data);
    let elem = array_snapshot_pop_front(ref data);

    (data, *(elem.unwrap().deref()))
}

fn fn2() -> (@Array<felt252>, felt252) {
    let mut data: @Array<felt252> = @array![0, 0x8c4fd0147, 0];
    let _elem = array_snapshot_pop_front(ref data);
    let elem = array_snapshot_pop_front(ref data);

    (data, *(elem.unwrap().deref()))
}

#[test]
fn check_same() {
    assert_eq!(fn1(), fn2());
}
