use array::ArrayTrait;
use hash::LegacyHash;
use integer::u256_from_felt252;
use option::OptionTrait;

fn reproduce_bug() {
    match gas::withdraw_gas_all(get_builtin_costs()) {
        Option::Some(_) => {},
        Option::None(_) => {
            let mut data = ArrayTrait::new();
            data.append('OOG');
            panic(data);
        }
    }
    let a = 1;
    let b = 2;
    let mut c = 0;
    if u256_from_felt252(
        a
    ) < u256_from_felt252(b) {
        c = LegacyHash::hash(a, b);
    } else {
        c = LegacyHash::hash(b, a);
    }
}
