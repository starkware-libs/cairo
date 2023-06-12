use array::ArrayTrait;
use keccak::keccak_u256s_be_inputs;
use math::u256_div_mod_n;
use option::OptionTrait;
use starknet::{eth_address::U256IntoEthAddress, EthAddress, SyscallResult, SyscallResultTrait};
use traits::{Into, TryInto};
use integer::U256TryIntoNonZero;

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


/// Receives a signature and the signed message hash.
/// Returns the public key associated with the signer, represented as a point on the curve.
/// Note:
///   Some places use non boolean values for v.
///   In that case, `recover_public_key_u32` may be a better match.
fn recover_public_key<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    impl Secp256PointImpl: Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, r: u256, s: u256, y_parity: bool
) -> Option<Secp256Point> {
    let r_point = Secp256Impl::secp256_ec_get_point_from_x_syscall(x: r, :y_parity)
        .unwrap_syscall()?;
    let generator_point = Secp256Impl::get_generator_point();

    // The result is given by
    //   -(msg_hash / r) * gen + (s / r) * r_point
    // where the divisions by `r` are modulo `N` (the size of the curve).

    let n_nz = Secp256Impl::get_curve_size().try_into().unwrap();
    let r_nz = r.try_into().unwrap();
    let u1 = u256_div_mod_n(msg_hash, r_nz, n_nz).unwrap();
    let minus_u1 = secp256_ec_negate_scalar::<Secp256Point>(u1);
    let u2 = u256_div_mod_n(s, r_nz, n_nz).unwrap();

    let minus_point1 = generator_point.mul(minus_u1).unwrap_syscall();

    let point2 = r_point.mul(u2).unwrap_syscall();

    Option::Some(minus_point1.add(point2).unwrap_syscall())
}

/// Same as `recover_public_key` but receives `v` of type `u32` instead of `y_parity`.
/// Uses the parity of `v` as `y_parity`.
fn recover_public_key_u32<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    impl Secp256PointImpl: Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, r: u256, s: u256, v: u32
) -> Option<Secp256Point> {
    let y_parity = v % 2 == 0;
    recover_public_key(:msg_hash, :r, :s, :y_parity)
}

/// Computes the negation of a scalar modulo N (the size of the curve).
fn secp256_ec_negate_scalar<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>
>(
    c: u256
) -> u256 {
    Secp256Impl::get_curve_size() - c
}


/// Verifies a Secp256 ECDSA signature.
/// Also verifies that r and s are in the range (0, N), where N is the size of the curve.
fn verify_eth_signature<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    impl Secp256PointImpl: Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, r: u256, s: u256, y_parity: bool, eth_address: EthAddress
) {
    assert(is_signature_entry_valid::<Secp256Point>(r), 'Signature out of range');
    assert(is_signature_entry_valid::<Secp256Point>(s), 'Signature out of range');

    let public_key_point = recover_public_key::<Secp256Point>(:msg_hash, :r, :s, :y_parity)
        .unwrap();
    let calculated_eth_address = public_key_point_to_eth_address(:public_key_point);
    assert(eth_address == calculated_eth_address, 'Invalid signature');
}

/// Same as `verify_eth_signature` but receives `v` of type `u32` instead of `y_parity`.
/// Uses the parity of `v` as `y_parity`.
fn verify_eth_signature_u32<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    impl Secp256PointImpl: Secp256PointTrait<Secp256Point>
>(
    msg_hash: u256, r: u256, s: u256, v: u32, eth_address: EthAddress
) {
    let y_parity = v % 2 == 0;
    verify_eth_signature::<Secp256Point>(:msg_hash, :r, :s, :y_parity, :eth_address);
}

/// Checks whether `value` is in the range [1, N), where N is the size of the curve.
fn is_signature_entry_valid<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>
>(
    value: u256
) -> bool {
    value != 0_u256 && value < Secp256Impl::get_curve_size()
}

/// Converts a public key point to the corresponding Ethereum address.
fn public_key_point_to_eth_address<
    Secp256Point,
    impl Secp256PointDrop: Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    impl Secp256PointImpl: Secp256PointTrait<Secp256Point>
>(
    public_key_point: Secp256Point
) -> EthAddress {
    let (x, y) = public_key_point.get_coordinates().unwrap_syscall();

    let mut keccak_input = Default::default();
    keccak_input.append(x);
    keccak_input.append(y);
    // Keccak output is little endian.
    let point_hash_le = keccak_u256s_be_inputs(keccak_input.span());
    let point_hash = u256 {
        low: integer::u128_byte_reverse(point_hash_le.high),
        high: integer::u128_byte_reverse(point_hash_le.low)
    };

    point_hash.into()
}
