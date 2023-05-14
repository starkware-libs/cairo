use traits::TryInto;
use option::OptionTrait;

#[test]
#[available_gas(10000000)]
fn test_egcd() {
    let (g, s, sign_s, t, sign_t) = math::egcd(
        68_u8.try_into().unwrap(), 16_u8.try_into().unwrap()
    );
    assert(g == 4, 'g != 4');
    assert(s == 1, 's != 1');
    assert(sign_s, 's should be positive');
    assert(t == 4, 't != 4');
    assert(!sign_t, 't should be negative');
    assert(1 * 68 - 4 * 16 == 4, 'Sanity check failed');

    let (g, s, sign_s, t, sign_t) = integer::egcd(
        240_u256.try_into().unwrap(), 46_u256.try_into().unwrap()
    );
    assert(g == 2, 'g != 2');
    assert(s == 9, 's != 9');
    assert(!sign_s, 's should be negative');
    assert(t == 47, 't != 47');
    assert(sign_t, 't should be positive');
    assert(47 * 46 - 9 * 240 == 2, 'Sanity check failed');

    let (g, s, sign_s, t, sign_t) = integer::egcd(
        50_u128.try_into().unwrap(), 17_u128.try_into().unwrap()
    );
    assert(g == 1, 'g != 1');
    assert(s == 1, 's != 1');
    assert(!sign_s, 's should be negative');
    assert(t == 3, 't != 3');
    assert(sign_t, 't should be positive');
    assert(3 * 17 - 1 * 50 == 1, 'Sanity check failed');
}
