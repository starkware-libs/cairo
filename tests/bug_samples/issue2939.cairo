fn foo() -> usize {
    let mut x = 0_usize;
    let mut y = 0_usize;
    loop {
        core::gas::withdraw_gas_all(core::gas::get_builtin_costs()).expect('Out of gas');
        if x == 10_usize {
            break y;
        }

        x += 1_usize;

        if x % 2_usize == 0_usize {
            continue;
        }

        y += 1_usize;
    }
}

#[test]
fn main() {
    assert_eq!(foo(), 5_usize);
}
