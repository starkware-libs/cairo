//! Elliptic Curve Digital Signature Algorithm (ECDSA) for the STARK curve.
//!
//! This module provides implementations for ECDSA signature verification and public key recovery
//! specifically tailored for the STARK curve.
//!
//! Curve information:
//! * Curve equation: y² ≡ x³ + α·x + β (mod p)
//! * α = 1
//! * β = 0x6f21413efbe40de150e596d72f7a8c5609ad26c15c915c1f4cdfcb99cee9e89
//! * p = 0x0800000000000011000000000000000000000000000000000000000000000001 = 2^251 + 17 * 2^192 +
//! 1
//! Generator point:
//! * x = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca
//! * y = 0x5668060aa49730b7be4801df46ec62de53ecd11abe43a32873000c36e8dc1f

use crate::ec::{EcPoint, EcPointTrait, EcStateTrait};
#[allow(unused_imports)]
use crate::option::OptionTrait;
#[allow(unused_imports)]
use crate::traits::{Into, TryInto};
#[allow(unused_imports)]
use crate::zeroable::IsZeroResult;
use crate::{ec, math};

/// Verifies an ECDSA signature against a message hash and public key.
///
/// Note: the verification algorithm implemented by this function slightly deviates from the
/// standard ECDSA.
/// While this does not allow to create valid signatures if one does not possess the private key,
/// it means that the signature algorithm used should be modified accordingly.
/// This function validates that `s` and `r` are not 0 or equal to the curve order,
/// but does not check that `r, s < stark_curve::ORDER`, which should be checked by the caller.
///
/// # Arguments
/// * `message_hash` - The hash of the signed message
/// * `public_key` - The x-coordinate of the signer's public key point on the STARK curve
/// * `signature_r` - The r component of the ECDSA signature (x-coordinate of point R)
/// * `signature_s` - The s component of the ECDSA signature
///
/// # Returns
/// Returns `true` if the signature is valid, `false` otherwise.
///
/// # Examples
///
/// ```
/// use core::ecdsa::check_ecdsa_signature;
///
/// let message_hash = 0x2d6479c0758efbb5aa07d35ed5454d728637fceab7ba544d3ea95403a5630a8;
/// let pubkey = 0x1ef15c18599971b7beced415a40f0c7deacfd9b0d1819e03d723d8bc943cfca;
/// let r = 0x6ff7b413a8457ef90f326b5280600a4473fef49b5b1dcdfcd7f42ca7aa59c69;
/// let s = 0x23a9747ed71abc5cb956c0df44ee8638b65b3e9407deade65de62247b8fd77;
/// assert!(check_ecdsa_signature(message_hash, pubkey, r, s));
/// ```
// TODO(lior): Make this function nopanic once possible.
pub fn check_ecdsa_signature(
    message_hash: felt252, public_key: felt252, signature_r: felt252, signature_s: felt252,
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
    let public_key_point = match EcPointTrait::new_nz_from_x(public_key) {
        Some(point) => point,
        None => { return false; },
    };

    // Check that `r` is the x coordinate of a point on the curve and get such a point.
    // Note that this ensures that `r != 0`.
    let signature_r_point = match EcPointTrait::new_nz_from_x(signature_r) {
        Some(point) => point,
        None => { return false; },
    };

    // Retrieve the generator point.
    let gen_point = match EcPointTrait::new_nz(ec::stark_curve::GEN_X, ec::stark_curve::GEN_Y) {
        Some(point) => point,
        None => { return false; },
    };

    // Initialize an EC state.
    let init_ec = EcStateTrait::init();

    // To verify ECDSA, obtain:
    //   zG = z * G, where z is the message and G is a generator of the EC.
    //   rQ = r * Q, where Q.x = public_key.
    //   sR = s * R, where R.x = r.
    // and check that:
    //   zG +/- rQ = +/- sR, or more efficiently that:
    //   (zG +/- rQ).x = sR.x.

    // Calculate `sR.x`.
    let mut sR_state = init_ec.clone();
    sR_state.add_mul(signature_s, signature_r_point);
    let sR_x = match sR_state.finalize_nz() {
        Some(pt) => pt.x(),
        None => { return false; },
    };

    // Calculate a state with `z * G`.
    let mut zG_state = init_ec.clone();
    zG_state.add_mul(message_hash, gen_point);

    // Calculate the point `r * Q`.
    let mut rQ_state = init_ec;
    rQ_state.add_mul(signature_r, public_key_point);
    let rQ = match rQ_state.finalize_nz() {
        Some(pt) => pt,
        // The zero case is not actually possible, as `signature_r` isn't 0.
        None => { return false; },
    };

    // Check the `(zG + rQ).x = sR.x` case.
    let mut zG_plus_eQ_state = zG_state.clone();
    zG_plus_eQ_state.add(rQ);
    if let Some(pt) = zG_plus_eQ_state.finalize_nz() {
        if pt.x() == sR_x {
            return true;
        }
    }

    // Check the `(zG - rQ).x = sR.x` case.
    let mut zG_minus_eQ_state = zG_state;
    zG_minus_eQ_state.sub(rQ);
    if let Some(pt) = zG_minus_eQ_state.finalize_nz() {
        if pt.x() == sR_x {
            return true;
        }
    }

    false
}

/// Recovers the public key from an ECDSA signature and message hash.
///
/// Given a valid ECDSA signature, the original message hash, and the y-coordinate parity of point
/// R, this function recovers the signer's public key. This is useful in scenarios where you need to
/// verify a message has been signed by a specific public key.
///
/// # Arguments
/// * `message_hash` - The hash of the signed message
/// * `signature_r` - The r component of the ECDSA signature (x-coordinate of point R)
/// * `signature_s` - The s component of the ECDSA signature
/// * `y_parity` - The parity of the y-coordinate of point R (`true` for odd, `false` for even)
///
/// # Returns
/// Returns `Some(public_key)` containing the x-coordinate of the recovered public key point if
/// the signature is valid, `None` otherwise.
///
/// # Examples
///
/// ```
/// use core::ecdsa::recover_public_key;
///
/// let message_hash = 0x503f4bea29baee10b22a7f10bdc82dda071c977c1f25b8f3973d34e6b03b2c;
/// let signature_r = 0xbe96d72eb4f94078192c2e84d5230cde2a70f4b45c8797e2c907acff5060bb;
/// let signature_s = 0x677ae6bba6daf00d2631fab14c8acf24be6579f9d9e98f67aa7f2770e57a1f5;
/// assert!(
///     recover_public_key(:message_hash, :signature_r, :signature_s, y_parity: false)
///         .unwrap() == 0x7b7454acbe7845da996377f85eb0892044d75ae95d04d3325a391951f35d2ec,
/// )
/// ```
pub fn recover_public_key(
    message_hash: felt252, signature_r: felt252, signature_s: felt252, y_parity: bool,
) -> Option<felt252> {
    let mut signature_r_point = EcPointTrait::new_from_x(signature_r)?;
    let y: u256 = signature_r_point.try_into()?.y().into();
    // If the actual parity of the actual y is different than requested, flip the parity.
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
    let r_inv = math::u256_inv_mod(r_nz, ord_nz)?.into();
    let s_div_r: felt252 = math::u256_mul_mod_n(signature_s.into(), r_inv, ord_nz).try_into()?;
    let z_div_r: felt252 = math::u256_mul_mod_n(message_hash.into(), r_inv, ord_nz).try_into()?;
    let s_div_rR: EcPoint = signature_r_point.mul(s_div_r);
    let z_div_rG: EcPoint = gen_point.mul(z_div_r);

    Some((s_div_rR - z_div_rG).try_into()?.x())
}
