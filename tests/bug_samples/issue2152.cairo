use integer::u256_from_felt252;

#[test]
#[available_gas(100000)]
fn reproduce_bug() {
    match gas::withdraw_gas_all(get_builtin_costs()) {
        Option::Some(_) => {},
        Option::None => {
            panic(array!['OOG']);
        }
    }
    let a = 1;
    let b = 2;
    let mut c = 0;
    if u256_from_felt252(a) < u256_from_felt252(b) {
        c = pedersen::pedersen(a, b);
    } else {
        c = pedersen::pedersen(b, a);
    }
}
