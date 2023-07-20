use core::array::ArrayTrait;

#[available_gas(2000000)]
#[test]
fn main() {
    let mut a: Array<felt252> = array!['a', 'b'];

    let a = loop {
        let v: felt252 = match a.pop_front() {
            Option::Some(v) => {
                v
            },
            Option::None => {
                break 'hi';
            }
        };
    };
}
