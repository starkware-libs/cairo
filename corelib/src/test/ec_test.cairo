use ec::{EcPointTrait, EcPointTryIntoNonZero, EcStateTrait};
use option::OptionTrait;
use test::test_utils::{assert_eq, assert_ne};
use traits::{Into, TryInto};

#[test]
#[should_panic]
fn test_ec_from_zero() {
    EcPointTrait::new_from_x(0).expect('Not on curve.');
}

#[test]
fn test_ec_operations() {
    // Beta + 2 is a square, and for x = 1 and alpha = 1, x^3 + alpha * x + beta = beta + 2.
    let beta_p2_root = 2487829544412206244690656897973144572467842667075005257202960243805141046681;
    let p = EcPointTrait::new_from_x(1).unwrap();
    let p_nz = p.try_into().unwrap();
    let (x, y) = p_nz.coordinates();
    assert_eq(@x, @1, 'x != 1');
    assert(y == beta_p2_root || y == -beta_p2_root, 'y is wrong');

    let mut state = EcStateTrait::init();
    state.add(p_nz);
    let q = state.finalize_nz().expect('zero point');
    let (qx, qy) = q.coordinates();
    assert_eq(@qx, @x, 'bad finalize x');
    assert_eq(@qy, @y, 'bad finalize y');

    // Try doing the same thing with the EC op builtin.
    let mut state = EcStateTrait::init();
    state.add_mul(1, p_nz);
    let q3 = state.finalize_nz().expect('zero point');
    let (qx, qy) = q3.coordinates();
    assert_eq(@qx, @x, 'bad EC op x');
    assert_eq(@qy, @y, 'bad EC op y');

    // Try computing `p + p` using the ec_mul function.
    let double_p = p.mul(2);
    let (double_x, double_y) = double_p.try_into().unwrap().coordinates();
    let expected_double_y =
        3572434102142093425782752266058856056057826477682467661647843687948039943621;
    assert_eq(
        @double_x,
        @75984168971785666410219869038140038216102669781812169677875295511117260233,
        'bad double x'
    );
    assert(double_y == expected_double_y || double_y == -expected_double_y, 'bad double y');

    // Compute `2p - p`.
    let (sub_x, sub_y) = (double_p - p).try_into().unwrap().coordinates();
    assert_eq(@sub_x, @x, 'bad x for 2p - p');
    assert_eq(@sub_y, @y, 'bad y for 2p - p');

    // Compute `p - p`.
    assert(EcPointTryIntoNonZero::try_into(p - p).is_none(), 'p - p did not return 0.');

    // Compute `(-p) - p`.
    let (sub2_x, sub2_y) = (-p - p).try_into().unwrap().coordinates();
    assert_eq(@sub2_x, @double_x, 'bad x for (-p) - p');
    assert_eq(@sub2_y, @-double_y, 'bad y for (-p) - p');
}

#[test]
#[should_panic]
fn test_bad_ec_point_creation() {
    EcPointTrait::new(0, 0).unwrap();
}

#[test]
fn test_ec_point_finalization_zero() {
    let state = EcStateTrait::init();
    let point_at_infinity = state.finalize_nz();
    assert(point_at_infinity.is_none(), 'Wrong point');
}

#[test]
fn test_ecdsa() {
    let message_hash = 0x503f4bea29baee10b22a7f10bdc82dda071c977c1f25b8f3973d34e6b03b2c;
    let public_key = 0x7b7454acbe7845da996377f85eb0892044d75ae95d04d3325a391951f35d2ec;
    let signature_r = 0xbe96d72eb4f94078192c2e84d5230cde2a70f4b45c8797e2c907acff5060bb;
    let signature_s = 0x677ae6bba6daf00d2631fab14c8acf24be6579f9d9e98f67aa7f2770e57a1f5;
    assert(
        ecdsa::check_ecdsa_signature(:message_hash, :public_key, :signature_r, :signature_s),
        'ecdsa returned false'
    );
    assert(
        !ecdsa::check_ecdsa_signature(
            message_hash: message_hash + 1, :public_key, :signature_r, :signature_s
        ),
        'ecdsa - wrong message'
    );
    assert(
        !ecdsa::check_ecdsa_signature(
            :message_hash, public_key: public_key + 1, :signature_r, :signature_s
        ),
        'ecdsa - wrong public_key'
    );
    assert(
        !ecdsa::check_ecdsa_signature(
            :message_hash, :public_key, signature_r: signature_r + 1, :signature_s
        ),
        'ecdsa - wrong r'
    );
    assert(
        !ecdsa::check_ecdsa_signature(
            :message_hash, :public_key, :signature_r, signature_s: signature_s + 1
        ),
        'ecdsa - wrong s'
    );
}

#[test]
#[available_gas(100000000)]
fn test_ecdsa_recover_public_key() {
    let message_hash = 0x503f4bea29baee10b22a7f10bdc82dda071c977c1f25b8f3973d34e6b03b2c;
    let signature_r = 0xbe96d72eb4f94078192c2e84d5230cde2a70f4b45c8797e2c907acff5060bb;
    let signature_s = 0x677ae6bba6daf00d2631fab14c8acf24be6579f9d9e98f67aa7f2770e57a1f5;
    assert_eq(
        @ecdsa::recover_public_key(:message_hash, :signature_r, :signature_s, y_parity: false)
            .unwrap(),
        @0x7b7454acbe7845da996377f85eb0892044d75ae95d04d3325a391951f35d2ec,
        'recover_ecdsa_public_key failed'
    );
    assert(
        ecdsa::check_ecdsa_signature(
            :message_hash,
            public_key: ecdsa::recover_public_key(
                :message_hash, :signature_r, :signature_s, y_parity: true
            )
                .unwrap(),
            :signature_r,
            :signature_s
        ),
        'ecdsa returned false'
    );
}

#[test]
fn test_ec_mul() {
    let p = EcPointTrait::new(
        x: 336742005567258698661916498343089167447076063081786685068305785816009957563,
        y: 1706004133033694959518200210163451614294041810778629639790706933324248611779,
    )
        .unwrap();
    let m = 2713877091499598330239944961141122840311015265600950719674787125185463975936;
    let (x, y) = p.mul(m).try_into().unwrap().coordinates();

    assert_eq(
        @x,
        @2881632108168892236043523177391659237686965655035240771134509747985978822780,
        'ec_mul failed (x).'
    );
    assert_eq(
        @y,
        @591135563672138037839394207500885413019058613584891498394077262936524140839,
        'ec_mul failed (y).'
    );
}
