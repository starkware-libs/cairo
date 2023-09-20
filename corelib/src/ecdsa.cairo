use ec::EcPointTrait;
use option::OptionTrait;
use traits::{Into, TryInto};
use zeroable::IsZeroResult;

// Checks if (`signature_r`, `signature_s`) is a valid ECDSA signature for the given `public_key`
// on the given `message`.
//
// Note: the verification algorithm implemented by this function slightly deviates from the
// standard ECDSA.
// While this does not allow to create valid signatures if one does not possess the private key,
// it means that the signature algorithm used should be modified accordingly.
// Namely, it should check that `r, s < stark_curve::ORDER`.
//
// Arguments:
// * `message_hash` - the signed message.
// * `public_key` - the public key corresponding to the key with which the message was signed.
// * `signature_r` - the `r` component of the ECDSA signature.
// * `signature_s` - the `s` component of the ECDSA signature.
//
// Returns:
//   `true` if the signature is valid and `false` otherwise.
// TODO(lior): Make this function nopanic once possible.
fn check_ecdsa_signature(
    message_hash: felt252, public_key: felt252, signature_r: felt252, signature_s: felt252
) -> bool {
    // TODO(orizi): Change to || once it does not prevent `a == 0` comparison optimization.
    // Check that s != 0 (mod stark_curve::ORDER).
    if signature_s == 0 {
        return false;
    }
    if signature_s == ec::stark_curve::ORDER {
        return false;
    }
    if signature_r == ec::stark_curve::ORDER {
        return false;
    }

    // Check that the public key is the x coordinate of a point on the curve and get such a point.
    let public_key_point = match ec::EcPointTrait::new_from_x(public_key) {
        Option::Some(point) => point,
        Option::None => { return false; },
    };

    // Check that `r` is the x coordinate of a point on the curve and get such a point.
    // Note that this ensures that `r != 0`.
    let signature_r_point = match EcPointTrait::new_from_x(signature_r) {
        Option::Some(point) => point,
        Option::None => { return false; },
    };

    // Retrieve the generator point.
    let gen_point = match EcPointTrait::new(ec::stark_curve::GEN_X, ec::stark_curve::GEN_Y) {
        Option::Some(point) => point,
        Option::None => { return false; },
    };

    // To verify ECDSA, obtain:
    //   zG = z * G, where z is the message and G is a generator of the EC.
    //   rQ = r * Q, where Q.x = public_key.
    //   sR = s * R, where R.x = r.
    // and check that:
    //   zG +/- rQ = +/- sR, or more efficiently that:
    //   (zG +/- rQ).x = sR.x.
    let sR: EcPoint = signature_r_point.mul(signature_s);
    let sR_x = match sR.try_into() {
        Option::Some(pt) => {
            let (x, _) = ec::ec_point_unwrap(pt);
            x
        },
        Option::None => { return false; },
    };

    let zG: EcPoint = gen_point.mul(message_hash);
    let rQ: EcPoint = public_key_point.mul(signature_r);
    match (zG + rQ).try_into() {
        Option::Some(pt) => {
            let (x, _) = ec::ec_point_unwrap(pt);
            if (x == sR_x) {
                return true;
            }
        },
        Option::None => {},
    };

    match (zG - rQ).try_into() {
        Option::Some(pt) => {
            let (x, _) = ec::ec_point_unwrap(pt);
            if (x == sR_x) {
                return true;
            }
        },
        Option::None => {},
    };

    return false;
}

/// Receives a signature and the signed message hash.
/// Returns the public key associated with the signer.
fn recover_public_key(
    message_hash: felt252, signature_r: felt252, signature_s: felt252, y_parity: bool
) -> Option<felt252> {
    let mut signature_r_point = EcPointTrait::new_from_x(signature_r)?;
    let (_, y) = signature_r_point.try_into()?.coordinates();
    let y: u256 = y.into();
    // If the actual the parity of the actual y is different than requested, flip the parity.
    if (y.low & 1 == 1) != y_parity {
        signature_r_point = -signature_r_point;
    }

    // Retrieve the generator point.
    let gen_point = EcPointTrait::new(ec::stark_curve::GEN_X, ec::stark_curve::GEN_Y)?;

    // a Valid signature should satisfy:
    // zG + rQ = sR.
    // Where:
    //   zG = z * G, z is the message and G is a generator of the EC.
    //   rQ = r * Q, Q.x = public_key.
    //   sR = s * R, where R.x = r and R.y is determined by y_parity.
    //
    // Hence:
    //   rQ = sR - zG.
    //   Q = (s/r)R - (z/r)G
    // and we can recover the public key using:
    //   Q.x = ((s/r)R - (z/r)G).x.
    let r_nz: u256 = signature_r.into();
    let r_nz = r_nz.try_into()?;
    let ord_nz: u256 = ec::stark_curve::ORDER.into();
    let ord_nz = ord_nz.try_into()?;
    let r_inv = math::inv_mod(r_nz, ord_nz)?;
    let s_div_r: felt252 = math::u256_mul_mod_n(signature_s.into(), r_inv, ord_nz).try_into()?;
    let z_div_r: felt252 = math::u256_mul_mod_n(message_hash.into(), r_inv, ord_nz).try_into()?;
    let s_div_rR: EcPoint = signature_r_point.mul(s_div_r);
    let z_div_rG: EcPoint = gen_point.mul(z_div_r);
    let (x, _) = (s_div_rR - z_div_rG).try_into()?.coordinates();
    Option::Some(x)
}
