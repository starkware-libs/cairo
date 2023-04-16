use option::OptionTrait;

fn foo(ref item_index: usize, index: usize) {
    gas::withdraw_gas_all(get_builtin_costs()).expect('Out of gas');

    if 5_usize < 7 {
        item_index = index;
    };
    foo(ref item_index, index);
}

fn main() {
    let mut x = 0;
    foo(ref x, 0);
}
