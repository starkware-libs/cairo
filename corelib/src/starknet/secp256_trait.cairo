//! Elliptic Curve Digital Signature Algorithm (ECDSA) for Secp256k1 and Secp256r1 curves.
//!
//! This module provides traits and functions for working with ECDSA signatures
//! on the Secp256k1 and the Secp256r1 curves. It includes utilities for creating
//! and validating signatures, as well as recovering public keys from signatures.
//!
//! # Examples
//!
//! ```
//! use starknet::SyscallResultTrait;
/// use starknet::secp256r1::Secp256r1Point;
/// use starknet::secp256_trait::{Secp256Trait, Signature, is_signature_entry_valid,
/// is_valid_signature, recover_public_key};
///
/// assert!(
///     is_signature_entry_valid::<
///         Secp256r1Point,
///     >(0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac),
/// );
///
/// let msg_hash = 0x4cee90eb86eaa050036147a12d49004b6b9c72bd725d39d4785011fe190f0b4d;
/// let r = 0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac;
/// let s = 0x36dbcd03009df8c59286b162af3bd7fcc0450c9aa81be5d10d312af6c66b1d60;
/// let public_key = Secp256Trait::secp256_ec_new_syscall(
///     0x4aebd3099c618202fcfe16ae7770b0c49ab5eadf74b754204a3bb6060e44eff3,
///     0x7618b065f9832de4ca6ca971a7a1adc826d0f7c00181a5fb2ddf79ae00b4e10e,
/// )
///     .unwrap_syscall()
///     .unwrap();
///
/// assert!(is_valid_signature::<Secp256r1Point>(msg_hash, r, s, public_key));
///
/// let signature = Signature {
///     r: 0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac,
///     s: 0x36dbcd03009df8c59286b162af3bd7fcc0450c9aa81be5d10d312af6c66b1d60,
///     y_parity: true,
/// };
///
/// let public_key = recover_public_key::<Secp256r1Point>(msg_hash, signature);
/// ```

#[allow(unused_imports)]
use core::array::ArrayTrait;
#[allow(unused_imports)]
use core::integer::U256TryIntoNonZero;
use core::math::{u256_inv_mod, u256_mul_mod_n};
use core::option::OptionTrait;
use core::traits::{Into, TryInto};
#[allow(unused_imports)]
use starknet::eth_address::U256IntoEthAddress;
#[allow(unused_imports)]
use starknet::{EthAddress, SyscallResult, SyscallResultTrait};

/// Represents a Secp256{k/r}1 ECDSA signature.
///
/// This struct holds the components of an ECDSA signature: `r`, `s`, and `y_parity`.
#[derive(Copy, Drop, Debug, PartialEq, Serde, Hash)]
pub struct Signature {
    pub r: u256,
    pub s: u256,
    /// The parity of the y coordinate of the elliptic curve point whose x coordinate is `r`.
    /// `y_parity == true` means that the y coordinate is odd.
    /// Some places use non boolean `v` instead of `y_parity`.
    /// In that case, `signature_from_vrs` should be used.
    pub y_parity: bool,
}

impl SignatureStorePacking of starknet::StorePacking<Signature, (u256, u256, bool)> {
    fn pack(value: Signature) -> (u256, u256, bool) {
        (value.r, value.s, value.y_parity)
    }

    fn unpack(value: (u256, u256, bool)) -> Signature {
        let (r, s, y_parity) = value;
        Signature { r, s, y_parity }
    }
}

/// Creates an ECDSA signature from the `v`, `r`, and `s` values.
///
/// `v` is the sum of an odd number and the parity of the y coordinate of the ec point whose x
/// coordinate is `r`.
///
/// See https://eips.ethereum.org/EIPS/eip-155 for more details.
///
/// # Examples
///
/// ```
/// use starknet::secp256_trait::signature_from_vrs;
///
/// let signature = signature_from_vrs(0,
/// 0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac,
/// 0x36dbcd03009df8c59286b162af3bd7fcc0450c9aa81be5d10d312af6c66b1d60);
/// ```
pub fn signature_from_vrs(v: u32, r: u256, s: u256) -> Signature {
    Signature { r, s, y_parity: v % 2 == 0 }
}

/// A trait for interacting with Secp256{k/r}1 curves.
///
/// Provides operations needed to work with Secp256k1 and Secp256r1 elliptic curves.
/// It includes methods for accessing curve parameters and creating curve points.
///
/// # Examples
///
/// ```
/// use starknet::secp256k1::Secp256k1Point;
/// use starknet::secp256_trait::Secp256Trait;
/// use starknet::SyscallResultTrait;
///
/// assert!(
///     Secp256Trait::<
///         Secp256k1Point,
///     >::get_curve_size() == 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141,
/// );
///
/// let generator = Secp256Trait::<Secp256k1Point>::get_generator_point();
///
/// let generator = Secp256Trait::<
/// Secp256k1Point,
/// >::secp256_ec_new_syscall(
/// 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
/// 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8,
/// )
/// .unwrap_syscall();
///
/// let random_point = Secp256Trait::<
/// Secp256k1Point,
/// >::secp256_ec_get_point_from_x_syscall(
/// 0x4aebd3099c618202fcfe16ae7770b0c49ab5eadf74b754204a3bb6060e44eff3, true,
/// );
/// ```
pub trait Secp256Trait<Secp256Point> {
    /// Returns the order (size) of the curve's underlying field.
    ///
    /// This is the number of points on the curve, also known as the curve order.
    fn get_curve_size() -> u256;

    /// Returns the generator point (G) for the curve.
    ///
    /// The generator point is a standard base point on the curve from which other points
    /// can be generated through scalar multiplication.
    fn get_generator_point() -> Secp256Point;

    /// Creates a new curve point from its x and y coordinates.
    ///
    /// Returns `None` if the provided coordinates don't represent a valid point on the curve.
    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256Point>>;

    /// Creates a curve point on the curve given its x-coordinate and y-parity.
    ///
    /// # Arguments
    ///
    /// * `x` - The x coordinate of the point
    /// * `y_parity` - If true, choose the odd y value; if false, choose the even y value
    ///
    /// # Returns
    ///
    /// Returns `Some(point)` if a point exists with the given x coordinate,
    /// `None` otherwise.
    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool,
    ) -> SyscallResult<Option<Secp256Point>>;
}

/// A trait for performing operations on Secp256{k/r}1 curve points.
///
/// Provides operations needed for elliptic curve cryptography, including point addition
/// and scalar multiplication.
///
/// # Examples
///
/// ```
/// use starknet::SyscallResultTrait;
/// use starknet::secp256k1::Secp256k1Point;
/// use starknet::secp256_trait::Secp256PointTrait;
/// use starknet::secp256_trait::Secp256Trait;
///
/// let generator = Secp256Trait::<Secp256k1Point>::get_generator_point();
///
/// assert!(
///     Secp256PointTrait::get_coordinates(generator)
///         .unwrap_syscall() == (
///             0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798,
///             0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8,
///         ),
/// );
///
/// let point = Secp256PointTrait::add(generator, generator);
/// let other_point = Secp256PointTrait::mul(generator, 2);
/// ```
pub trait Secp256PointTrait<Secp256Point> {
    /// Returns the x and y coordinates of the curve point.
    fn get_coordinates(self: Secp256Point) -> SyscallResult<(u256, u256)>;

    /// Performs elliptic curve point addition.
    ///
    /// Adds `self` and `other` following the curve's addition law and returns
    /// the resulting point.
    fn add(self: Secp256Point, other: Secp256Point) -> SyscallResult<Secp256Point>;

    /// Performs scalar multiplication of a curve point.
    ///
    /// Multiplies `self` by the given scalar value.
    fn mul(self: Secp256Point, scalar: u256) -> SyscallResult<Secp256Point>;
}

/// Checks whether the given `value` is in the range [1, N), where N is the size of the curve.
///
/// For ECDSA signatures to be secure, both `r` and `s` components must be in the range [1, N),
/// where N is the order of the curve. Enforcing this range prevents signature malleability attacks
/// where an attacker could create multiple valid signatures for the same message by adding
/// multiples of N.
/// This function validates that a given value meets this requirement.
///
/// # Returns
///
/// Returns `true` if the value is in the valid range [1, N), `false` otherwise.
///
/// # Examples
///
/// ```
/// use starknet::secp256r1::Secp256r1Point;
/// use starknet::secp256_trait::is_signature_entry_valid;
///
/// assert!(!is_signature_entry_valid::<Secp256r1Point>(0));
/// ```
pub fn is_signature_entry_valid<
    Secp256Point, +Drop<Secp256Point>, impl Secp256Impl: Secp256Trait<Secp256Point>,
>(
    value: u256,
) -> bool {
    value != 0_u256 && value < Secp256Impl::get_curve_size()
}

/// Checks whether a signature is valid given a public key point and a message hash.
///
/// # Examples
///
/// ```
/// use starknet::SyscallResultTrait;
/// use starknet::secp256r1::Secp256r1Point;
/// use starknet::secp256_trait::{Secp256Trait, is_valid_signature};
///
/// let msg_hash = 0x4cee90eb86eaa050036147a12d49004b6b9c72bd725d39d4785011fe190f0b4d;
/// let r = 0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac;
/// let s = 0x36dbcd03009df8c59286b162af3bd7fcc0450c9aa81be5d10d312af6c66b1d60;
/// let public_key = Secp256Trait::secp256_ec_new_syscall(
///     0x4aebd3099c618202fcfe16ae7770b0c49ab5eadf74b754204a3bb6060e44eff3,
///     0x7618b065f9832de4ca6ca971a7a1adc826d0f7c00181a5fb2ddf79ae00b4e10e,
/// )
///     .unwrap_syscall()
///     .unwrap();
///
/// assert!(is_valid_signature::<Secp256r1Point>(msg_hash, r, s, public_key));
/// ```
pub fn is_valid_signature<
    Secp256Point,
    +Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>,
>(
    msg_hash: u256, r: u256, s: u256, public_key: Secp256Point,
) -> bool {
    if !is_signature_entry_valid::<Secp256Point>(r)
        || !is_signature_entry_valid::<Secp256Point>(s) {
        return false;
    }

    let n_nz = Secp256Impl::get_curve_size().try_into().unwrap();
    let s_inv = u256_inv_mod(s.try_into().unwrap(), n_nz).unwrap().into();
    let u1 = u256_mul_mod_n(msg_hash, s_inv, n_nz);
    let u2 = u256_mul_mod_n(r, s_inv, n_nz);

    let generator_point = Secp256Impl::get_generator_point();
    let point1 = generator_point.mul(u1).unwrap_syscall();
    let point2 = public_key.mul(u2).unwrap_syscall();
    let sum = point1.add(point2).unwrap_syscall();

    let (x, _y) = sum.get_coordinates().unwrap_syscall();
    x == r
}

/// Recovers the public key associated with a given signature and message hash.
///
/// Returns the public key as a point on the curve.
///
/// # Examples
///
/// ```
/// use starknet::secp256r1::Secp256r1Point;
/// use starknet::secp256_trait::{Signature, recover_public_key};
///
/// let msg_hash = 0x4cee90eb86eaa050036147a12d49004b6b9c72bd725d39d4785011fe190f0b4d;
///
/// let signature = Signature {
///     r: 0xa73bd4903f0ce3b639bbbf6e8e80d16931ff4bcf5993d58468e8fb19086e8cac,
///     s: 0x36dbcd03009df8c59286b162af3bd7fcc0450c9aa81be5d10d312af6c66b1d60,
///     y_parity: true,
/// };
///
/// let public_key = recover_public_key::<Secp256r1Point>(msg_hash, signature);
/// ```
pub fn recover_public_key<
    Secp256Point,
    +Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>,
>(
    msg_hash: u256, signature: Signature,
) -> Option<Secp256Point> {
    let Signature { r, s, y_parity } = signature;
    let r_point = Secp256Impl::secp256_ec_get_point_from_x_syscall(x: r, :y_parity)
        .unwrap_syscall()?;
    let generator_point = Secp256Impl::get_generator_point();

    // The result is given by
    //   -(msg_hash / r) * gen + (s / r) * r_point
    // where the divisions by `r` are modulo `N` (the size of the curve).

    let n_nz = Secp256Impl::get_curve_size().try_into().unwrap();
    let r_inv = u256_inv_mod(r.try_into().unwrap(), n_nz).unwrap().into();

    let u1 = u256_mul_mod_n(msg_hash, r_inv, n_nz);
    let minus_u1 = secp256_ec_negate_scalar::<Secp256Point>(u1);
    let u2 = u256_mul_mod_n(s, r_inv, n_nz);

    let minus_point1 = generator_point.mul(minus_u1).unwrap_syscall();

    let point2 = r_point.mul(u2).unwrap_syscall();

    Some(minus_point1.add(point2).unwrap_syscall())
}

/// Computes the negation of a scalar modulo N (the size of the curve).
fn secp256_ec_negate_scalar<
    Secp256Point, +Drop<Secp256Point>, impl Secp256Impl: Secp256Trait<Secp256Point>,
>(
    c: u256,
) -> u256 {
    Secp256Impl::get_curve_size() - c
}
