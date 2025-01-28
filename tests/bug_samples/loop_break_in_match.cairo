#[test]
fn main() {
    let mut a: Array<felt252> = array!['a', 'b'];

    let _a = loop {
        let _v: felt252 = match a.pop_front() {
            Some(v) => { v },
            None => { break 'hi'; },
        };
    };
}
