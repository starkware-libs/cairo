use core::math;

/// Helper for making a non-zero value.
fn nz<N, +TryInto<N, NonZero<N>>>(n: N) -> NonZero<N> {
    n.try_into().unwrap()
}

#[test]
fn test_egcd() {
    let (g, s, t, sub_direction) = math::egcd(nz(68_u8), nz(16_u8));
    assert_eq!(g, 4);
    assert_eq!(s, 1);
    assert_eq!(t, 4);
    assert!(sub_direction);
    assert_eq!(1 * 68 - 4 * 16, 4);

    let (g, s, t, sub_direction) = math::egcd(nz(240_u256), nz(46_u256));
    assert_eq!(g, 2);
    assert_eq!(s, 9);
    assert_eq!(t, 47);
    assert!(!sub_direction);
    assert_eq!(47 * 46 - 9 * 240, 2);

    let (g, s, t, sub_direction) = math::egcd(nz(50_u128), nz(17_u128));
    assert_eq!(g, 1);
    assert_eq!(s, 1);
    assert_eq!(t, 3);
    assert!(!sub_direction);
    assert_eq!(3 * 17 - 1 * 50, 1);

    let (g, s, t, sub_direction) = math::egcd(nz(5_u128), nz(15_u128));
    assert_eq!(g, 5);
    assert_eq!(s, 1);
    assert_eq!(t, 0);
    assert!(sub_direction);
    assert_eq!(1 * 5 - 0 * 15, 5);

    let (g, s, t, sub_direction) = math::egcd(nz(1_u128), nz(1_u128));
    assert_eq!(g, 1);
    assert_eq!(s, 0);
    assert_eq!(t, 1);
    assert!(!sub_direction);
    assert_eq!(1 * 1 - 0 * 1, 1);
}

#[test]
fn test_inv_mod() {
    assert!(math::inv_mod(nz(5), nz(24)) == Option::Some(5_u256));
    assert!(math::inv_mod(nz(29), nz(24)) == Option::Some(5_u128));
    assert!(math::inv_mod(nz(1), nz(24)) == Option::Some(1_u16));
    assert!(math::inv_mod(nz(1), nz(5)) == Option::Some(1_u32));
    assert!(math::inv_mod(nz(8_usize), nz(24_usize)).is_none());
    assert!(math::inv_mod(nz(1), nz(1)) == Option::Some(0_usize));
    assert!(math::inv_mod(nz(7), nz(1)) == Option::Some(0_usize));
}

#[test]
fn test_u256_div_mod_n() {
    assert!(math::u256_div_mod_n(6, 2, nz(7)) == Option::Some(3));
    assert!(math::u256_div_mod_n(5, 1, nz(7)) == Option::Some(5));
    assert!(math::u256_div_mod_n(1, 1, nz(7)) == Option::Some(1));
    assert!(math::u256_div_mod_n(7, 2, nz(13)) == Option::Some(10));
    assert!(math::u256_div_mod_n(0, 3, nz(13)) == Option::Some(0));
    assert!(math::u256_div_mod_n(4, 3, nz(6)).is_none());
    assert!(math::u256_div_mod_n(5, 4, nz(6)).is_none());
    assert!(math::u256_div_mod_n(2, 8, nz(4)).is_none());
    assert!(
        math::u256_div_mod_n(
            0xfa855081cc80656250605b2ecd7958ba4f0aa6799053da0d68bf76f2484decc6,
            0xe8e94a59a951af1b4c8cbd45fb8d01c1dd946de2533e3ad18845f9dbb6d12f4f,
            nz(0xa3db605888ac3cd19e70c5b52220ad693566b996ef078e907578fec7758dabc9)
        ) == Option::Some(0x8e70aea916ee4b782a0da9c18083ed9d867148a703615a2a88d0e7fddd4c900d)
    );
    assert!(
        math::u256_div_mod_n(
            0x759426f1c0ba213b6378196b5091f5fa48f49f1d0cecfb00a7d59a51be35f609,
            0x57ff2c2e0900fce82331e396a71787a837783cca8145538eb32cb4b52104a3be,
            nz(0xcb514e4d4672d8f1d952c0312afb5baae86121aa5817030d8439ce759295a029)
        ) == Option::Some(0x7c9d22b40f98075c0bfd674d546bc77d775dcf021d30b88afb099834dffa951b)
    );
}

#[test]
fn test_u256_inv_mod_no_inverse() {
    assert!(math::u256_inv_mod(3, nz(6)).is_none());
    assert!(math::u256_inv_mod(4, nz(6)).is_none());
    assert!(math::u256_inv_mod(8, nz(4)).is_none());
    assert!(
        math::u256_inv_mod(
            0xea9195982bd472e30e5146ad7cb0acd954cbc75032a298ac73234b6b05e28cc1,
            nz(0x4075f980fab77a3fde536dbaae600f5ea1540e01837dcec64c1f379613aa4d18)
        )
            .is_none()
    );
    assert!(
        math::u256_inv_mod(
            0x85ef555d7a0aa34019c138defc40a1d3683dc1caa505bff286dd8069a28a2e4c,
            nz(0xd71e5a5f4a4d1af45e703f9e13d1305ce149313037956247ad5edfe3e81d6353)
        )
            .is_none()
    );
    let prime_140_bits = 0x63f7a7326f84cfca7738923db2b6d6c1b1b;
    assert!(
        math::u256_inv_mod(
            prime_140_bits * 0x51e28b744cc00edfb6bbbe6a9,
            nz(prime_140_bits * 0xc09497df4aa02be7fd25f10d3)
        )
            .is_none()
    );
    let prime_256_bits = 0x74c5ef92be07ee4ad43ae8ca337390e4a5dfdbf4f1a5f09cdf412ab7ce343503;
    assert!(math::u256_inv_mod(prime_256_bits, nz(prime_256_bits)).is_none());
}
