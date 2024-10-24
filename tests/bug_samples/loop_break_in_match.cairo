#[test]
fn main() {
    let mut a: Array<felt252> = array!['a', 'b'];

    let _a = loop {
        let _v: felt252 = match a.pop_front() {
            Option::Some(v) => { v },
            Option::None => { break 'hi'; }
        };
    };
}
