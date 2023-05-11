use option::OptionTrait;
use test::test_utils::{assert_eq, assert_ne};

fn foo() -> usize {
    let mut x = 0_usize;
    let mut y = 0_usize;
    loop {
        gas::withdraw_gas_all(get_builtin_costs()).expect('Out of gas');
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
#[available_gas(1000000)]
fn main() {
    assert_eq(foo(), 5_usize, 'issue2939');
}
