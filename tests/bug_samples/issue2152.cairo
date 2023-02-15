use hash::LegacyHash;
use array::ArrayTrait;
use option::OptionTrait;

fn reproduce_bug() {
    match get_gas_all(get_builtin_costs()) {
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
    if a < b {
        c = LegacyHash::hash(a, b);
    } else {
        c = LegacyHash::hash(b, a);
    }
}
