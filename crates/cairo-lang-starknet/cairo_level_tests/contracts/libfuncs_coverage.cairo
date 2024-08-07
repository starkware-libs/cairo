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
    Bool: BitwiseLibfuncs<bool>,
    Felt252: NumericLibfuncs<felt252>,
    Conversions: ConversionsLibfuncs,
    CheckECDSA: (felt252, felt252, felt252, felt252),
    Keccak: ByteArray,
    Sha256: ByteArray,
    ArrayU128: ArrayLibfuncs<u128>,
    ArrayU256: ArrayLibfuncs<u256>,
    Circuit: (u384, u384, u384),
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
    Lt: (T, T),
    Numeric: NumericLibfuncs<T>,
}

enum UnsignedIntLibfuncs<T> {
    Sqrt: T,
    Bitwise: BitwiseLibfuncs<T>,
    Int: IntLibfuncs<T>,
}

enum BitwiseLibfuncs<T> {
    And: (T, T),
    Or: (T, T),
    Xor: (T, T),
}

enum ConversionsLibfuncs {
    Into: IntoLibfuncs,
    TryInto: TryIntoLibfuncs,
}

enum IntoLibfuncs {
    U8Felt252: u8,
    U16Felt252: u16,
    U32Felt252: u32,
    U64Felt252: u64,
    U128Felt252: u128,
    I8Felt252: i8,
    I16Felt252: i16,
    I32Felt252: i32,
    I64Felt252: i64,
    I128Felt252: i128,
    BoolFelt252: bool,
    Felt252U256: felt252,
    U256U384: u256,
}

enum TryIntoLibfuncs {
    Felt252U8: felt252,
    Felt252U16: felt252,
    Felt252U32: felt252,
    Felt252U64: felt252,
    Felt252U128: felt252,
    Felt252I8: felt252,
    Felt252I16: felt252,
    Felt252I32: felt252,
    Felt252I64: felt252,
    Felt252I128: felt252,
}

enum ArrayLibfuncs<T> {
    New,
    Append: (Array<T>, T),
    PopFront: Array<T>,
    PopFrontConsume: Array<T>,
    Get: (@Array<T>, usize),
    Len: @Array<T>,
    SnapPopFront: Span<T>,
    SnapPopBack: Span<T>,
    MultiPopFront: Span<T>,
    MultiPopBack: Span<T>,
    Slice: Span<T>,
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
        Libfuncs::Bool(libfuncs) => bitwise_libfuncs(libfuncs),
        Libfuncs::Felt252(libfuncs) => numeric_libfuncs(libfuncs),
        Libfuncs::Conversions(libfuncs) => conversions_libfuncs(libfuncs),
        Libfuncs::CheckECDSA((
            a, b, c, d
        )) => use_and_panic(core::ecdsa::check_ecdsa_signature(a, b, c, d)),
        Libfuncs::Keccak(input) => use_and_panic(core::keccak::compute_keccak_byte_array(@input)),
        Libfuncs::Sha256(input) => use_and_panic(core::sha256::compute_sha256_byte_array(@input)),
        Libfuncs::ArrayU128(libfuncs) => array_libfuncs(libfuncs),
        Libfuncs::ArrayU256(libfuncs) => array_libfuncs(libfuncs),
        Libfuncs::Circuit((n, in1, in2)) => circuit_libfuncs(n, in1, in2),
    }
}

use core::num::traits::Sqrt;
use core::traits::{BitAnd, BitOr, BitXor};

fn unsigned_int_libfuncs<
    T,
    impl TSqrt: Sqrt<T>,
    +Drop<TSqrt::Target>,
    +BitAnd<T>,
    +BitOr<T>,
    +BitXor<T>,
    +Div<T>,
    +Rem<T>,
    +PartialOrd<T>,
    +Add<T>,
    +Sub<T>,
    +Mul<T>,
    +PartialEq<T>,
    +Copy<T>,
    +Drop<T>
>(
    libfuncs: UnsignedIntLibfuncs<T>
) {
    match libfuncs {
        UnsignedIntLibfuncs::Sqrt(a) => use_and_panic(Sqrt::sqrt(a)),
        UnsignedIntLibfuncs::Bitwise(libfuncs) => bitwise_libfuncs(libfuncs),
        UnsignedIntLibfuncs::Int(libfuncs) => int_libfuncs(libfuncs),
    }
}

fn int_libfuncs<T, +Div<T>, +Rem<T>, +PartialOrd<T>, +Add<T>, +Sub<T>, +Mul<T>, +PartialEq<T>, +Drop<T>>(
    libfuncs: IntLibfuncs<T>
) {
    match libfuncs {
        IntLibfuncs::Div((a, b)) => use_and_panic(a / b),
        IntLibfuncs::Mod((a, b)) => use_and_panic(a % b),
        IntLibfuncs::Lt((a, b)) => use_and_panic(a < b),
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

fn bitwise_libfuncs<T, +BitAnd<T>, +BitOr<T>, +BitXor<T>, +Drop<T>>(libfuncs: BitwiseLibfuncs<T>) {
    match libfuncs {
        BitwiseLibfuncs::And((a, b)) => use_and_panic(a & b),
        BitwiseLibfuncs::Or((a, b)) => use_and_panic(a | b),
        BitwiseLibfuncs::Xor((a, b)) => use_and_panic(a ^ b),
    }
}

fn conversions_libfuncs(libfuncs: ConversionsLibfuncs) {
    match libfuncs {
        ConversionsLibfuncs::Into(into) => into_libfuncs(into),
        ConversionsLibfuncs::TryInto(try_into) => try_into_libfuncs(try_into),
    }
}

fn into_libfuncs(libfuncs: IntoLibfuncs) {
    match libfuncs {
        IntoLibfuncs::U8Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::U16Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::U32Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::U64Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::U128Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::I8Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::I16Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::I32Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::I64Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::I128Felt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::BoolFelt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::Felt252U256(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::U256U384(v) => use_and_panic::<u384>(v.into()),
    }
}

fn try_into_libfuncs(libfuncs: TryIntoLibfuncs) {
    match libfuncs {
        TryIntoLibfuncs::Felt252U8(v) => use_and_panic::<Option<u8>>(v.try_into()),
        TryIntoLibfuncs::Felt252U16(v) => use_and_panic::<Option<u16>>(v.try_into()),
        TryIntoLibfuncs::Felt252U32(v) => use_and_panic::<Option<u32>>(v.try_into()),
        TryIntoLibfuncs::Felt252U64(v) => use_and_panic::<Option<u64>>(v.try_into()),
        TryIntoLibfuncs::Felt252U128(v) => use_and_panic::<Option<u128>>(v.try_into()),
        TryIntoLibfuncs::Felt252I8(v) => use_and_panic::<Option<i8>>(v.try_into()),
        TryIntoLibfuncs::Felt252I16(v) => use_and_panic::<Option<i16>>(v.try_into()),
        TryIntoLibfuncs::Felt252I32(v) => use_and_panic::<Option<i32>>(v.try_into()),
        TryIntoLibfuncs::Felt252I64(v) => use_and_panic::<Option<i64>>(v.try_into()),
        TryIntoLibfuncs::Felt252I128(v) => use_and_panic::<Option<i128>>(v.try_into()),
    }
}

fn array_libfuncs<T, +Drop<T>>(libfuncs: ArrayLibfuncs<T>) {
    match libfuncs {
        ArrayLibfuncs::New => use_and_panic(ArrayTrait::<T>::new()),
        ArrayLibfuncs::Append((mut array, item)) => use_and_panic(array.append(item)),
        ArrayLibfuncs::PopFront(mut array) => use_and_panic_drop(array.pop_front()),
        ArrayLibfuncs::PopFrontConsume(array) => use_and_panic_drop(array.pop_front_consume()),
        ArrayLibfuncs::Get((array, index)) => use_and_panic_drop(array.get(index)),
        ArrayLibfuncs::Len(array) => use_and_panic(array.len()),
        ArrayLibfuncs::SnapPopFront(mut span) => use_and_panic_drop(span.pop_front()),
        ArrayLibfuncs::SnapPopBack(mut span) => use_and_panic_drop(span.pop_back()),
        ArrayLibfuncs::MultiPopFront(mut span) => use_and_panic_drop(span.multi_pop_front::<5>()),
        ArrayLibfuncs::MultiPopBack(mut span) => use_and_panic_drop(span.multi_pop_back::<5>()),
        ArrayLibfuncs::Slice(span) => use_and_panic(span.slice(2, 5)),
    }
}

use core::circuit::{
    circuit_add, circuit_inverse, circuit_mul, circuit_sub, u384, AddInputResultTrait,
    CircuitElement, CircuitInput, CircuitInputs, CircuitModulus, CircuitOutputsTrait,
    EvalCircuitTrait,
};

fn circuit_libfuncs(n: u384, input0: u384, input1: u384) {
    let in1 = CircuitElement::<CircuitInput<0>> {};
    let in2 = CircuitElement::<CircuitInput<1>> {};
    let add = circuit_add(in1, in2);
    let inv = circuit_inverse(add);
    let sub = circuit_sub(inv, in2);
    let mul = circuit_mul(inv, sub);

    let modulus = TryInto::<_, CircuitModulus>::try_into([n.limb0, n.limb1, n.limb2, n.limb3])
        .unwrap();
    let outputs = (mul, add, inv)
        .new_inputs()
        .next(input0)
        .next(input1)
        .done()
        .eval(modulus)
        .unwrap();
    use_and_panic(outputs.get_output(add));
}

#[inline(never)]
fn use_and_panic<T, +PanicDestruct<T>>(t: T) {
    panic!();
}

#[inline(never)]
fn use_and_panic_drop<T, +Drop<T>>(t: T) {
    panic!();
}
