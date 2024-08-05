#[starknet::contract]
mod libfuncs_coverage {
    #[storage]
    struct Storage {}

    #[abi(per_item)]
    #[generate_trait]
    impl Impl of Trait {
        #[external(v0)]
        fn entry_point(ref self: ContractState) {
            super::all_libfuncs(super::Libfuncs::None)
        }
    }
}

enum Libfuncs {
    None,
    U8: UnsignedIntLibfuncs<u8>,
    U16: UnsignedIntLibfuncs<u16>,
    U32: UnsignedIntLibfuncs<u32>,
    U64: UnsignedIntLibfuncs<u64>,
    U128: UnsignedIntLibfuncs<u128>,
    U256: UnsignedIntLibfuncs<u256>,
    I8: IntLibfuncs<i8>,
    I16: IntLibfuncs<i16>,
    I32: IntLibfuncs<i32>,
    I64: IntLibfuncs<i64>,
    I128: IntLibfuncs<i128>,
    Felt252: NumericLibfuncs<felt252>,
    CheckECDSA: (felt252, felt252, felt252, felt252),
    Keccak: ByteArray,
    Sha256: ByteArray,
}

enum NumericLibfuncs<T> {
    Add: (T, T),
    Sub: (T, T),
    Mul: (T, T),
    Equal: (T, T),
}

enum IntLibfuncs<T> {
    Div: (T, T),
    Mod: (T, T),
    Numeric: NumericLibfuncs<T>,
}

enum UnsignedIntLibfuncs<T> {
    Sqrt: T,
    Int: IntLibfuncs<T>,
}

#[inline(never)]
fn all_libfuncs(libfuncs: Libfuncs) {
    match libfuncs {
        Libfuncs::None => {},
        Libfuncs::U8(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U16(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U32(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U64(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U128(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::U256(libfuncs) => unsigned_int_libfuncs(libfuncs),
        Libfuncs::I8(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I16(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I32(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I64(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::I128(libfuncs) => int_libfuncs(libfuncs),
        Libfuncs::Felt252(libfuncs) => numeric_libfuncs(libfuncs),
        Libfuncs::CheckECDSA((
            a, b, c, d
        )) => use_and_panic(core::ecdsa::check_ecdsa_signature(a, b, c, d)),
        Libfuncs::Keccak(input) => use_and_panic(core::keccak::compute_keccak_byte_array(@input)),
        Libfuncs::Sha256(input) => use_and_panic(core::sha256::compute_sha256_byte_array(@input)),
    }
}

use core::num::traits::Sqrt;

fn unsigned_int_libfuncs<
    T,
    impl TSqrt: Sqrt<T>,
    +Drop<TSqrt::Target>,
    +Div<T>,
    +Rem<T>,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +PartialEq<T>,
    +Drop<T>
>(
    libfuncs: UnsignedIntLibfuncs<T>
) {
    match libfuncs {
        UnsignedIntLibfuncs::Sqrt(a) => use_and_panic(Sqrt::sqrt(a)),
        UnsignedIntLibfuncs::Int(libfuncs) => int_libfuncs(libfuncs),
    }
}

fn int_libfuncs<T, +Div<T>, +Rem<T>, +Add<T>, +Sub<T>, +Mul<T>, +PartialEq<T>, +Drop<T>>(
    libfuncs: IntLibfuncs<T>
) {
    match libfuncs {
        IntLibfuncs::Div((a, b)) => use_and_panic(a / b),
        IntLibfuncs::Mod((a, b)) => use_and_panic(a % b),
        IntLibfuncs::Numeric(libfuncs) => numeric_libfuncs(libfuncs),
    }
}

fn numeric_libfuncs<T, +Add<T>, +Sub<T>, +Mul<T>, +PartialEq<T>, +Drop<T>>(
    libfuncs: NumericLibfuncs<T>
) {
    match libfuncs {
        NumericLibfuncs::Add((a, b)) => use_and_panic(a + b),
        NumericLibfuncs::Sub((a, b)) => use_and_panic(a - b),
        NumericLibfuncs::Mul((a, b)) => use_and_panic(a * b),
        NumericLibfuncs::Equal((a, b)) => use_and_panic(a == b),
    }
}

fn use_and_panic<T, +PanicDestruct<T>>(t: T) {
    panic!();
}
