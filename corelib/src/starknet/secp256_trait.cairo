use array::ArrayTrait;
use math::{u256_mul_mod_n, inv_mod};
use option::OptionTrait;
use starknet::{eth_address::U256IntoEthAddress, EthAddress, SyscallResult, SyscallResultTrait};
use traits::{Into, TryInto};
use integer::U256TryIntoNonZero;

/// Secp256{k/r}1 ECDSA signature.
#[derive(Copy, Drop, PartialEq, Serde, starknet::Store, Hash)]
struct Signature {
    r: u256,
    s: u256,
    // The parity of the y coordinate of the ec point whose x coordinate is `r`.
    // `y_parity` == true means that the y coordinate is odd.
    // Some places use non boolean v instead of y_parity.
    // In that case, `signature_from_vrs` should be used.
    y_parity: bool,
}


/// Creates an ECDSA signature from the `v`, `r` and `s` values.
/// `v` is the sum of an odd number and the parity of the y coordinate of the ec point whose x
/// coordinate is `r`.
/// See https://eips.ethereum.org/EIPS/eip-155 for more details.
fn signature_from_vrs(v: u32, r: u256, s: u256) -> Signature {
    Signature { r, s, y_parity: v % 2 == 0 }
}

trait Secp256Trait<Secp256Point> {
    fn get_curve_size() -> u256;
    fn get_generator_point() -> Secp256Point;

    fn secp256_ec_new_syscall(x: u256, y: u256) -> SyscallResult<Option<Secp256Point>>;
    fn secp256_ec_get_point_from_x_syscall(
        x: u256, y_parity: bool
    ) -> SyscallResult<Option<Secp256Point>>;
}

trait Secp256PointTrait<Secp256Point> {
    fn get_coordinates(self: Secp256Point) -> SyscallResult<(u256, u256)>;
    fn add(self: Secp256Point, other: Secp256Point) -> SyscallResult<Secp256Point>;
    fn mul(self: Secp256Point, scalar: u256) -> SyscallResult<Secp256Point>;
}

/// Checks whether `value` is in the range [1, N), where N is the size of the curve.
fn is_signature_entry_valid<
    Secp256Point, +Drop<Secp256Point>, impl Secp256Impl: Secp256Trait<Secp256Point>
>(
    value: u256
) -> bool {
    value != 0_u256 && value < Secp256Impl::get_curve_size()
}

fn is_valid_signature<
    Secp256Point,
    +Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, r: u256, s: u256, public_key: Secp256Point
) -> bool {
    if !is_signature_entry_valid::<Secp256Point>(r)
        || !is_signature_entry_valid::<Secp256Point>(s) {
        return false;
    }

    let n_nz = Secp256Impl::get_curve_size().try_into().unwrap();
    let s_inv = inv_mod(s.try_into().unwrap(), n_nz).unwrap();
    let u1 = u256_mul_mod_n(msg_hash, s_inv, n_nz);
    let u2 = u256_mul_mod_n(r, s_inv, n_nz);

    let generator_point = Secp256Impl::get_generator_point();
    let point1 = generator_point.mul(u1).unwrap_syscall();
    let point2 = public_key.mul(u2).unwrap_syscall();
    let sum = point1.add(point2).unwrap_syscall();

    let (x, y) = sum.get_coordinates().unwrap_syscall();
    x == r
}

/// Receives a signature and the signed message hash.
/// Returns the public key associated with the signer, represented as a point on the curve.
fn recover_public_key<
    Secp256Point,
    +Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, signature: Signature
) -> Option<Secp256Point> {
    let Signature{r, s, y_parity } = signature;
    let r_point = Secp256Impl::secp256_ec_get_point_from_x_syscall(x: r, :y_parity)
        .unwrap_syscall()?;
    let generator_point = Secp256Impl::get_generator_point();

    // The result is given by
    //   -(msg_hash / r) * gen + (s / r) * r_point
    // where the divisions by `r` are modulo `N` (the size of the curve).

    let n_nz = Secp256Impl::get_curve_size().try_into().unwrap();
    let r_inv = inv_mod(r.try_into().unwrap(), n_nz).unwrap();

    let u1 = u256_mul_mod_n(msg_hash, r_inv, n_nz);
    let minus_u1 = secp256_ec_negate_scalar::<Secp256Point>(u1);
    let u2 = u256_mul_mod_n(s, r_inv, n_nz);

    let minus_point1 = generator_point.mul(minus_u1).unwrap_syscall();

    let point2 = r_point.mul(u2).unwrap_syscall();

    Option::Some(minus_point1.add(point2).unwrap_syscall())
}

/// Computes the negation of a scalar modulo N (the size of the curve).
fn secp256_ec_negate_scalar<
    Secp256Point, +Drop<Secp256Point>, impl Secp256Impl: Secp256Trait<Secp256Point>
>(
    c: u256
) -> u256 {
    Secp256Impl::get_curve_size() - c
}
