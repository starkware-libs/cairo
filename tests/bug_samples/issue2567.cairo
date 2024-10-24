fn throw(err_code: felt252) -> core::never {
    let mut data = array![];
    data.append(err_code);
    panic(data)
}

#[test]
#[should_panic]
fn main() -> felt252 {
    match throw(1) {}
}
