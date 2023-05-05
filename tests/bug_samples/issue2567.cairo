use array::ArrayTrait;

fn throw(err_code: felt252) -> never {
    let mut data = Default::default();
    data.append(err_code);
    panic(data)
}

#[test]
#[should_panic]
fn main() -> felt252 {
    match throw(1) {}
}
