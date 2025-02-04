use core::integer::u256_from_felt252;

#[test]
fn reproduce_bug() {
    match core::gas::withdraw_gas_all(core::gas::get_builtin_costs()) {
        Some(_) => {},
        None => { panic(array!['OOG']); },
    }
    let a = 1;
    let b = 2;
    let mut c = 0;
    if u256_from_felt252(a) < u256_from_felt252(b) {
        c = core::pedersen::pedersen(a, b);
    } else {
        c = core::pedersen::pedersen(b, a);
    }
}
