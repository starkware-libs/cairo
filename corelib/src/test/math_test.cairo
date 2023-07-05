/// Helper for making a non-zero value.
fn nz<N, impl TryIntoNonZeroN: TryInto<N, NonZero<N>>>(n: N) -> NonZero<N> {
    n.try_into().unwrap()
}

#[test]
#[available_gas(10000000)]
fn test_egcd() {
    let (g, s, t, sub_direction) = math::egcd(nz(68_u8), nz(16_u8));
    assert(g == 4, 'g != 4');
    assert(s == 1, 's != 1');
    assert(t == 4, 't != 4');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 68 - 4 * 16 == 4, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(nz(240_u256), nz(46_u256));
    assert(g == 2, 'g != 2');
    assert(s == 9, 's != 9');
    assert(t == 47, 't != 47');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(47 * 46 - 9 * 240 == 2, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(nz(50_u128), nz(17_u128));
    assert(g == 1, 'g != 1');
    assert(s == 1, 's != 1');
    assert(t == 3, 't != 3');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(3 * 17 - 1 * 50 == 1, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(nz(5_u128), nz(15_u128));
    assert(g == 5, 'g != 5');
    assert(s == 1, 's != 1');
    assert(t == 0, 't != 0');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 5 - 0 * 15 == 5, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd(nz(1_u128), nz(1_u128));
    assert(g == 1, 'g != 1');
    assert(s == 0, 's != 0');
    assert(t == 1, 't != 1');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(1 * 1 - 0 * 1 == 1, 'Sanity check failed');
}

#[test]
#[available_gas(10000000)]
fn test_inv_mod() {
    assert(math::inv_mod(nz(5), nz(24)) == Option::Some(5_u256), 'inv_mov(5, 24) != 5');
    assert(math::inv_mod(nz(29), nz(24)) == Option::Some(5_u128), 'inv_mov(29, 24) != 5');
    assert(math::inv_mod(nz(1), nz(24)) == Option::Some(1_u16), 'inv_mov(1, 24) != 1');
    assert(math::inv_mod(nz(1), nz(5)) == Option::Some(1_u32), 'inv_mov(1, 5) != 1');
    assert(math::inv_mod(nz(8_usize), nz(24_usize)).is_none(), 'inv_mov(8, 24) != None');
    assert(math::inv_mod(nz(1), nz(1)) == Option::Some(0_usize), 'inv_mov(1, 1) != 0');
    assert(math::inv_mod(nz(7), nz(1)) == Option::Some(0_usize), 'inv_mov(7, 1) != 0');
}

#[test]
#[available_gas(10000000)]
fn test_u256_div_mod_n() {
    assert(math::u256_div_mod_n(6, nz(2), nz(7)) == Option::Some(3), '6 / 2 != 3 (7)');
    assert(math::u256_div_mod_n(5, nz(1), nz(7)) == Option::Some(5), '5 / 1 != 5 (7)');
    assert(math::u256_div_mod_n(1, nz(1), nz(7)) == Option::Some(1), '1 / 1 != 1 (7)');
    assert(math::u256_div_mod_n(7, nz(2), nz(13)) == Option::Some(10), '7 / 2 != 10 (13)');
    assert(math::u256_div_mod_n(0, nz(3), nz(13)) == Option::Some(0), '0 / 3 != 0 (13)');
    assert(math::u256_div_mod_n(4, nz(3), nz(6)).is_none(), '4 / 3 == None (6)');
    assert(math::u256_div_mod_n(2, nz(8), nz(4)).is_none(), '2 / 8 == None (4)');
    assert(
        math::u256_div_mod_n(
            0x2ab1f535168b19cef4bf517c5b010e089820273ac99e934bba57b1afb49856aa,
            nz(0xea9195982bd472e30e5146ad7cb0acd954cbc75032a298ac73234b6b05e28cc1),
            nz(0x4075f980fab77a3fde536dbaae600f5ea1540e01837dcec64c1f379613aa4d18)
        )
            .is_none(),
        'Random no inverse 1'
    );
    assert(
        math::u256_div_mod_n(
            0xe3f5c3c783073fdcf77c0459634bd8111698220fb18ef110d9a7b8b39de6289a,
            nz(0x85ef555d7a0aa34019c138defc40a1d3683dc1caa505bff286dd8069a28a2e4c),
            nz(0xd71e5a5f4a4d1af45e703f9e13d1305ce149313037956247ad5edfe3e81d6353)
        )
            .is_none(),
        'Random no inverse 2'
    );
    assert(
        math::u256_div_mod_n(
            0xfa855081cc80656250605b2ecd7958ba4f0aa6799053da0d68bf76f2484decc6,
            nz(0xe8e94a59a951af1b4c8cbd45fb8d01c1dd946de2533e3ad18845f9dbb6d12f4f),
            nz(0xa3db605888ac3cd19e70c5b52220ad693566b996ef078e907578fec7758dabc9)
        ) == Option::Some(0x8e70aea916ee4b782a0da9c18083ed9d867148a703615a2a88d0e7fddd4c900d),
        'Random large values 1'
    );
    assert(
        math::u256_div_mod_n(
            0x759426f1c0ba213b6378196b5091f5fa48f49f1d0cecfb00a7d59a51be35f609,
            nz(0x57ff2c2e0900fce82331e396a71787a837783cca8145538eb32cb4b52104a3be),
            nz(0xcb514e4d4672d8f1d952c0312afb5baae86121aa5817030d8439ce759295a029)
        ) == Option::Some(0x7c9d22b40f98075c0bfd674d546bc77d775dcf021d30b88afb099834dffa951b),
        'Random large values 2'
    );
}
