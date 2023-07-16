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
    // Check that s != 0 (mod stark_curve::ORDER).
    if signature_s == 0
        || signature_s == ec::stark_curve::ORDER
        || signature_r == ec::stark_curve::ORDER {
        return false;
    }

    // Check that the public key is the x coordinate of a point on the curve and get such a point.
    let public_key_point = match ec::ec_point_from_x(public_key) {
        Option::Some(point) => point,
        Option::None => {
            return false;
        },
    };

    // Check that `r` is the x coordinate of a point on the curve and get such a point.
    // Note that this ensures that `r != 0`.
    let signature_r_point = match ec::ec_point_from_x(signature_r) {
        Option::Some(point) => point,
        Option::None => {
            return false;
        },
    };

    // Retrieve the generator point.
    let gen_point = match EcPointTrait::new(ec::stark_curve::GEN_X, ec::stark_curve::GEN_Y) {
        Option::Some(point) => point,
        Option::None => {
            return false;
        },
    };

    // To verify ECDSA, obtain:
    //   zG = z * G, where z is the message and G is a generator of the EC.
    //   rQ = r * Q, where Q.x = public_key.
    //   sR = s * R, where R.x = r.
    // and check that:
    //   zG +/- rQ = +/- sR, or more efficiently that:
    //   (zG +/- rQ).x = sR.x.
    let sR: EcPoint = signature_r_point.mul(signature_s);
    let sR_x = match sR.test_zero() {
        IsZeroResult::Zero => {
            return false;
        },
        IsZeroResult::NonZero(pt) => {
            let (x, y) = pt.coordinates();
            x
        },
    };

    let zG: EcPoint = gen_point.mul(message_hash);
    let rQ: EcPoint = public_key_point.mul(signature_r);
    match (zG + rQ).test_zero() {
        IsZeroResult::Zero => {},
        IsZeroResult::NonZero(pt) => {
            let (x, y) = pt.coordinates();
            if (x == sR_x) {
                return true;
            }
        },
    };

    match (zG - rQ).test_zero() {
        IsZeroResult::Zero => {},
        IsZeroResult::NonZero(pt) => {
            let (x, y) = pt.coordinates();
            if (x == sR_x) {
                return true;
            }
        },
    };

    return false;
}
