use core::option::OptionTrait;
use core::math;

#[test]
fn test_egcd() {
    let (g, s, t, sub_direction) = math::egcd::<u8>(68, 16);
    assert(g == 4, 'g != 4');
    assert(s == 1, 's != 1');
    assert(t == 4, 't != 4');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 68 - 4 * 16 == 4, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd::<u256>(240, 46);
    assert(g == 2, 'g != 2');
    assert(s == 9, 's != 9');
    assert(t == 47, 't != 47');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(47 * 46 - 9 * 240 == 2, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd::<u128>(50, 17);
    assert(g == 1, 'g != 1');
    assert(s == 1, 's != 1');
    assert(t == 3, 't != 3');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(3 * 17 - 1 * 50 == 1, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd::<u128>(5, 15);
    assert(g == 5, 'g != 5');
    assert(s == 1, 's != 1');
    assert(t == 0, 't != 0');
    assert(sub_direction, 'sub_direction is wrong');
    assert(1 * 5 - 0 * 15 == 5, 'Sanity check failed');

    let (g, s, t, sub_direction) = math::egcd::<u128>(1, 1);
    assert(g == 1, 'g != 1');
    assert(s == 0, 's != 0');
    assert(t == 1, 't != 1');
    assert(!sub_direction, 'sub_direction is wrong');
    assert(1 * 1 - 0 * 1 == 1, 'Sanity check failed');
}

#[test]
fn test_inv_mod() {
    assert(math::inv_mod(5, 24) == Option::Some(5_u256), 'inv_mov(5, 24) != 5');
    assert(math::inv_mod(29, 24) == Option::Some(5_u128), 'inv_mov(29, 24) != 5');
    assert(math::inv_mod(1, 24) == Option::Some(1_u16), 'inv_mov(1, 24) != 1');
    assert(math::inv_mod(1, 5) == Option::Some(1_u32), 'inv_mov(1, 5) != 1');
    assert(math::inv_mod(8, 24) == Option::<usize>::None, 'inv_mov(8, 24) != None');
    assert(math::inv_mod(1, 1) == Option::Some(0_usize), 'inv_mov(1, 1) != 0');
    assert(math::inv_mod(7, 1) == Option::Some(0_usize), 'inv_mov(7, 1) != 0');
}

#[test]
fn test_u256_div_mod_n() {
    assert(math::u256_div_mod_n(6, 2, 7) == Option::Some(3), '6 / 2 != 3 (7)');
    assert(math::u256_div_mod_n(5, 1, 7) == Option::Some(5), '5 / 1 != 5 (7)');
    assert(math::u256_div_mod_n(1, 1, 7) == Option::Some(1), '1 / 1 != 1 (7)');
    assert(math::u256_div_mod_n(7, 2, 13) == Option::Some(10), '7 / 2 != 10 (13)');
    assert(math::u256_div_mod_n(0, 3, 13) == Option::Some(0), '0 / 3 != 0 (13)');
    assert(math::u256_div_mod_n(4, 3, 6).is_none(), '4 / 3 == None (6)');
    assert(math::u256_div_mod_n(5, 4, 6).is_none(), '5 / 4 == None (6)');
    assert(math::u256_div_mod_n(2, 8, 4).is_none(), '2 / 8 == None (4)');
    assert(
        math::u256_div_mod_n(
            0xfa855081cc80656250605b2ecd7958ba4f0aa6799053da0d68bf76f2484decc6,
            0xe8e94a59a951af1b4c8cbd45fb8d01c1dd946de2533e3ad18845f9dbb6d12f4f,
            0xa3db605888ac3cd19e70c5b52220ad693566b996ef078e907578fec7758dabc9
        ) == Option::Some(0x8e70aea916ee4b782a0da9c18083ed9d867148a703615a2a88d0e7fddd4c900d),
        'Random large values 1'
    );
    assert(
        math::u256_div_mod_n(
            0x759426f1c0ba213b6378196b5091f5fa48f49f1d0cecfb00a7d59a51be35f609,
            0x57ff2c2e0900fce82331e396a71787a837783cca8145538eb32cb4b52104a3be,
            0xcb514e4d4672d8f1d952c0312afb5baae86121aa5817030d8439ce759295a029
        ) == Option::Some(0x7c9d22b40f98075c0bfd674d546bc77d775dcf021d30b88afb099834dffa951b),
        'Random large values 2'
    );
}

#[test]
fn test_u256_inv_mod() {
    assert(math::u256_inv_mod(5, 24).unwrap().into() == 5_u256, 'inv_mov(5, 24) != 5');
    assert(math::u256_inv_mod(29, 24).unwrap().into() == 5_u256, 'inv_mov(29, 24) != 5');
    assert(math::u256_inv_mod(1, 24).unwrap().into() == 1_u256, 'inv_mov(1, 24) != 1');
    assert(math::u256_inv_mod(1, 5).unwrap().into() == 1_u256, 'inv_mov(1, 5) != 1');
    assert(math::u256_inv_mod(8, 24).is_none(), 'inv_mov(8, 24) != None');
    assert(math::u256_inv_mod(1, 1).is_none(), 'inv_mov(1, 1) != None');
    assert(math::u256_inv_mod(7, 1).is_none(), 'inv_mov(7, 1) != None');
    assert(math::u256_inv_mod(0, 1).is_none(), 'inv_mov(0, 1) != None');
    assert(math::u256_inv_mod(0, 7).is_none(), 'inv_mov(0, 7) != None');
    assert(math::u256_inv_mod(3, 6).is_none(), 'inv_mod(3, 6) != None');
    assert(math::u256_inv_mod(4, 6).is_none(), 'inv_mod(4, 6) != None');
    assert(math::u256_inv_mod(8, 4).is_none(), 'inv_mod(8, 4) != None');
    assert(
        math::u256_inv_mod(
            0xea9195982bd472e30e5146ad7cb0acd954cbc75032a298ac73234b6b05e28cc1,
            0x4075f980fab77a3fde536dbaae600f5ea1540e01837dcec64c1f379613aa4d18
        )
            .is_none(),
        'Random 1'
    );
    assert(
        math::u256_inv_mod(
            0x85ef555d7a0aa34019c138defc40a1d3683dc1caa505bff286dd8069a28a2e4c,
            0xd71e5a5f4a4d1af45e703f9e13d1305ce149313037956247ad5edfe3e81d6353
        )
            .is_none(),
        'Random 2'
    );
    let large_gcd = 0x63f7a7326f84cfca7738923db2b6d6c1b1b;
    assert(
        math::u256_inv_mod(
            large_gcd * 0x51e28b744cc00edfb6bbbe6a9,
            (large_gcd * 0xc09497df4aa02be7fd25f10d3).try_into().unwrap()
        )
            .is_none(),
        'gcd ~ 2**140'
    );
    let very_large_gcd: NonZero<_> =
        0x74c5ef92be07ee4ad43ae8ca337390e4a5dfdbf4f1a5f09cdf412ab7ce343503;
    assert(math::u256_inv_mod(very_large_gcd.into(), very_large_gcd).is_none(), 'gcd ~ 2**256');
}
