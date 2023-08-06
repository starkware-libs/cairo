#[test]
#[available_gas(10000000)]
fn test_egcd() {
    let (g, s, t, sub_direction) = math::egcd(68_u8.try_into().unwrap(), 16_u8.try_into().unwrap());
    assert(g == 4, 'g != 4');
    assert(s == 1, 's != 1');
    assert(t == 4, 't != 4');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 68 - 4 * 16 == 4, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(
        240_u256.try_into().unwrap(), 46_u256.try_into().unwrap()
    );
    assert(g == 2, 'g != 2');
    assert(s == 9, 's != 9');
    assert(t == 47, 't != 47');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(47 * 46 - 9 * 240 == 2, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(
        50_u128.try_into().unwrap(), 17_u128.try_into().unwrap()
    );
    assert(g == 1, 'g != 1');
    assert(s == 1, 's != 1');
    assert(t == 3, 't != 3');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(3 * 17 - 1 * 50 == 1, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(
        5_u128.try_into().unwrap(), 15_u128.try_into().unwrap()
    );
    assert(g == 5, 'g != 5');
    assert(s == 1, 's != 1');
    assert(t == 0, 't != 0');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 5 - 0 * 15 == 5, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(
        1_u128.try_into().unwrap(), 1_u128.try_into().unwrap()
    );
    assert(g == 1, 'g != 1');
    assert(s == 0, 's != 0');
    assert(t == 1, 't != 1');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(1 * 1 - 0 * 1 == 1, 'Sanity check failed');
}

#[test]
#[available_gas(10000000)]
fn test_inv_mod() {
    let inv = math::inv_mod(5_u256.try_into().unwrap(), 24_u256.try_into().unwrap()).unwrap();
    assert(inv == 5, 'inv != 5');

    let inv = math::inv_mod(29_u128.try_into().unwrap(), 24_u128.try_into().unwrap()).unwrap();
    assert(inv == 5, 'inv != 5');

    let inv = math::inv_mod(1_u16.try_into().unwrap(), 24_u16.try_into().unwrap()).unwrap();
    assert(inv == 1, 'inv != 1');

    let inv = math::inv_mod(1_u32.try_into().unwrap(), 5_u32.try_into().unwrap()).unwrap();
    assert(inv == 1, 'inv != 1');

    let inv = math::inv_mod(8_usize.try_into().unwrap(), 24_usize.try_into().unwrap());
    assert(inv.is_none(), 'inv should be None');

    let inv = math::inv_mod(1_usize.try_into().unwrap(), 1_usize.try_into().unwrap()).unwrap();
    assert(inv == 0, 'inv != 0');

    let inv = math::inv_mod(7_usize.try_into().unwrap(), 1_usize.try_into().unwrap()).unwrap();
    assert(inv == 0, 'inv != 0');
}

#[test]
#[available_gas(10000000)]
fn test_u256_div_mod_n() {
    let q = math::u256_div_mod_n(6_u256, 2_u256.try_into().unwrap(), 7_u256.try_into().unwrap())
        .unwrap();
    assert(q == 3, '6 / 2 != 3 (7)');

    let q = math::u256_div_mod_n(5_u256, 1_u256.try_into().unwrap(), 7_u256.try_into().unwrap())
        .unwrap();
    assert(q == 5, '5 / 1 != 5 (7)');

    let q = math::u256_div_mod_n(1_u256, 1_u256.try_into().unwrap(), 7_u256.try_into().unwrap())
        .unwrap();
    assert(q == 1, '1 / 1 != 1 (7)');

    let q = math::u256_div_mod_n(7_u256, 2_u256.try_into().unwrap(), 13_u256.try_into().unwrap())
        .unwrap();
    assert(q == 10, '7 / 2 != 10 (13)');

    let q = math::u256_div_mod_n(0_u256, 3_u256.try_into().unwrap(), 13_u256.try_into().unwrap())
        .unwrap();
    assert(q == 0, '0 / 3 != 0 (13)');
}
