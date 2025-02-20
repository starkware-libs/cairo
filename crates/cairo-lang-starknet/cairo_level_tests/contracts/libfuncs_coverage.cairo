use core::dict::{Felt252Dict, Felt252DictEntryTrait};
use starknet::storage::{StoragePathEntry, StoragePointerReadAccess, StoragePointerWriteAccess};

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
    Felt252: Felt252Libfuncs,
    Conversions: ConversionsLibfuncs,
    CheckECDSA: (felt252, felt252, felt252, felt252),
    RecoverECDSA: (felt252, felt252, felt252, bool),
    Sha256: ByteArray,
    ArrayU128: ArrayLibfuncs<u128>,
    ArrayU256: ArrayLibfuncs<u256>,
    DictU128: DictLibfuncs<u128>,
    DictFelt252: DictLibfuncs<felt252>,
    DictNullable: DictLibfuncs<Nullable<u256>>,
    NullableFelt252: NullableLibfuncs<felt252>,
    NullableU256: NullableLibfuncs<u256>,
    NullableNonCopy: NullableLibfuncs<Felt252Dict<felt252>>,
    Circuit: (u384, u384, u384),
    IsValidSignature: (u256, Signature, starknet::EthAddress),
    Secp256R1: (u256, u256, u256, u256),
    Starknet: StarknetLibfuncs,
    Consts: ConstsLibfuncs,
    Snapshot: SnapshotLibfuncs,
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

enum Felt252Libfuncs {
    Div: (felt252, NonZero<felt252>),
    Numeric: NumericLibfuncs<felt252>,
}

enum ConversionsLibfuncs {
    Into: IntoLibfuncs,
    Felt252TryInto: Felt252TryIntoLibfuncs,
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
    ContractAddressFelt252: starknet::ContractAddress,
    ClassHashFelt252: starknet::ClassHash,
    StorageAddressFelt252: starknet::StorageAddress,
}

enum Felt252TryIntoLibfuncs {
    U8: felt252,
    U16: felt252,
    U32: felt252,
    U64: felt252,
    U128: felt252,
    I8: felt252,
    I16: felt252,
    I32: felt252,
    I64: felt252,
    I128: felt252,
    Bytes31: felt252,
    ContractAddress: felt252,
    ClassHash: felt252,
    StorageAddress: felt252,
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
    SpanFromBox: Box<@[T; 5]>,
    BoxFromSpan: Span<T>,
}

enum DictLibfuncs<T> {
    Default,
    EntryGet: (Felt252Dict<T>, felt252),
}

enum NullableLibfuncs<T> {
    Default,
    New: T,
    IsNull: @Nullable<T>,
    Deref: Nullable<T>,
}

enum StarknetLibfuncs {
    LibraryCall: starknet::ClassHash,
    ContractCall: starknet::ContractAddress,
    StorageRead: starknet::storage::StorageBase<starknet::storage::Map<felt252, u256>>,
    StorageWrite: starknet::storage::StorageBase<starknet::storage::Mutable<ByteArray>>,
    Deploy: (starknet::ClassHash, felt252, Span<felt252>, bool),
    EmitEvent: (Span<felt252>, Span<felt252>),
    GetBlockHash: u64,
    GetExecutionInfo,
    GetExecutionInfoV2,
    ReplaceClass: starknet::ClassHash,
    SendMessageToL1: (felt252, Span<felt252>),
    GetClassHashAt: starknet::ContractAddress,
}

enum ConstsLibfuncs {
    Felt252,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Byte31,
    StorageBase,
    ClassHash,
    ContractAddress,
}

enum SnapshotLibfuncs {
    BoxForward: @Box<Felt252Dict<felt252>>,
    Match: @Option<Felt252Dict<felt252>>,
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
        Libfuncs::Felt252(libfuncs) => felt252_libfuncs(libfuncs),
        Libfuncs::Conversions(libfuncs) => conversions_libfuncs(libfuncs),
        Libfuncs::CheckECDSA((
            a, b, c, d,
        )) => use_and_panic(core::ecdsa::check_ecdsa_signature(a, b, c, d)),
        Libfuncs::RecoverECDSA((
            a, b, c, d,
        )) => use_and_panic(core::ecdsa::recover_public_key(a, b, c, d)),
        Libfuncs::Sha256(input) => use_and_panic(core::sha256::compute_sha256_byte_array(@input)),
        Libfuncs::ArrayU128(libfuncs) => array_libfuncs(libfuncs),
        Libfuncs::ArrayU256(libfuncs) => array_libfuncs(libfuncs),
        Libfuncs::DictU128(libfuncs) => dict_libfuncs(libfuncs),
        Libfuncs::DictFelt252(libfuncs) => dict_libfuncs(libfuncs),
        Libfuncs::DictNullable(libfuncs) => dict_libfuncs(libfuncs),
        Libfuncs::NullableFelt252(libfuncs) => nullable_libfuncs(libfuncs),
        Libfuncs::NullableU256(libfuncs) => nullable_libfuncs(libfuncs),
        Libfuncs::NullableNonCopy(libfuncs) => nullable_libfuncs(libfuncs),
        Libfuncs::Circuit((n, in1, in2)) => circuit_libfuncs(n, in1, in2),
        Libfuncs::IsValidSignature((
            msg_hash, signature, eth_address,
        )) => starknet::eth_signature::verify_eth_signature(msg_hash, signature, eth_address),
        Libfuncs::Secp256R1(sign) => secp_libfuncs::<starknet::secp256r1::Secp256r1Point>(sign),
        Libfuncs::Starknet(libfuncs) => starknet_libfuncs(libfuncs),
        Libfuncs::Consts(libfuncs) => consts_libfuncs(libfuncs),
        Libfuncs::Snapshot(libfuncs) => snapshot_libfuncs(libfuncs),
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
    +Drop<T>,
>(
    libfuncs: UnsignedIntLibfuncs<T>,
) {
    match libfuncs {
        UnsignedIntLibfuncs::Sqrt(a) => use_and_panic(Sqrt::sqrt(a)),
        UnsignedIntLibfuncs::Bitwise(libfuncs) => bitwise_libfuncs(libfuncs),
        UnsignedIntLibfuncs::Int(libfuncs) => int_libfuncs(libfuncs),
    }
}

fn int_libfuncs<
    T, +Div<T>, +Rem<T>, +PartialOrd<T>, +Add<T>, +Sub<T>, +Mul<T>, +PartialEq<T>, +Drop<T>,
>(
    libfuncs: IntLibfuncs<T>,
) {
    match libfuncs {
        IntLibfuncs::Div((a, b)) => use_and_panic(a / b),
        IntLibfuncs::Mod((a, b)) => use_and_panic(a % b),
        IntLibfuncs::Lt((a, b)) => use_and_panic(a < b),
        IntLibfuncs::Numeric(libfuncs) => numeric_libfuncs(libfuncs),
    }
}

fn felt252_libfuncs(libfuncs: Felt252Libfuncs) {
    match libfuncs {
        Felt252Libfuncs::Div((a, b)) => use_and_panic(core::felt252_div(a, b)),
        Felt252Libfuncs::Numeric(libfuncs) => numeric_libfuncs(libfuncs),
    }
}

fn numeric_libfuncs<T, +Add<T>, +Sub<T>, +Mul<T>, +PartialEq<T>, +Drop<T>>(
    libfuncs: NumericLibfuncs<T>,
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
        ConversionsLibfuncs::Into(libfuncs) => into_libfuncs(libfuncs),
        ConversionsLibfuncs::Felt252TryInto(libfuncs) => felt252_try_into_libfuncs(libfuncs),
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
        IntoLibfuncs::ContractAddressFelt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::ClassHashFelt252(v) => use_and_panic::<felt252>(v.into()),
        IntoLibfuncs::StorageAddressFelt252(v) => use_and_panic::<felt252>(v.into()),
    }
}

fn felt252_try_into_libfuncs(libfuncs: Felt252TryIntoLibfuncs) {
    match libfuncs {
        Felt252TryIntoLibfuncs::U8(v) => use_and_panic::<Option<u8>>(v.try_into()),
        Felt252TryIntoLibfuncs::U16(v) => use_and_panic::<Option<u16>>(v.try_into()),
        Felt252TryIntoLibfuncs::U32(v) => use_and_panic::<Option<u32>>(v.try_into()),
        Felt252TryIntoLibfuncs::U64(v) => use_and_panic::<Option<u64>>(v.try_into()),
        Felt252TryIntoLibfuncs::U128(v) => use_and_panic::<Option<u128>>(v.try_into()),
        Felt252TryIntoLibfuncs::I8(v) => use_and_panic::<Option<i8>>(v.try_into()),
        Felt252TryIntoLibfuncs::I16(v) => use_and_panic::<Option<i16>>(v.try_into()),
        Felt252TryIntoLibfuncs::I32(v) => use_and_panic::<Option<i32>>(v.try_into()),
        Felt252TryIntoLibfuncs::I64(v) => use_and_panic::<Option<i64>>(v.try_into()),
        Felt252TryIntoLibfuncs::I128(v) => use_and_panic::<Option<i128>>(v.try_into()),
        Felt252TryIntoLibfuncs::Bytes31(v) => use_and_panic::<Option<bytes31>>(v.try_into()),
        Felt252TryIntoLibfuncs::ContractAddress(v) => use_and_panic::<
            Option<starknet::ContractAddress>,
        >(v.try_into()),
        Felt252TryIntoLibfuncs::ClassHash(v) => use_and_panic::<
            Option<starknet::ClassHash>,
        >(v.try_into()),
        Felt252TryIntoLibfuncs::StorageAddress(v) => use_and_panic::<
            Option<starknet::StorageAddress>,
        >(v.try_into()),
    }
}

fn array_libfuncs<T, +Drop<T>, impl BoxFromSpan: TryInto<Span<T>, @Box<[T; 5]>>>(
    libfuncs: ArrayLibfuncs<T>,
) {
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
        ArrayLibfuncs::SpanFromBox(box) => use_and_panic(box.span()),
        ArrayLibfuncs::BoxFromSpan(span) => use_and_panic_drop(BoxFromSpan::try_into(span)),
    }
}

fn dict_libfuncs<T, +Drop<T>, +Felt252DictValue<T>, +Felt252DictEntryTrait<T>>(
    libfuncs: DictLibfuncs<T>,
) {
    match libfuncs {
        DictLibfuncs::Default => use_and_panic(Default::<Felt252Dict<T>>::default()),
        DictLibfuncs::EntryGet((dict, key)) => {
            let (e, _v) = dict.entry(key);
            use_and_panic(e)
        },
    }
}

// Handling missing destructor.
impl NullableDictDestruct of Destruct<Nullable<Felt252Dict<felt252>>> {
    fn destruct(self: Nullable<Felt252Dict<felt252>>) nopanic {
        match core::nullable::match_nullable(self) {
            core::nullable::FromNullableResult::Null => {},
            core::nullable::FromNullableResult::NotNull(value) => value.unbox().destruct(),
        }
    }
}

fn nullable_libfuncs<T, +Destruct<Nullable<T>>, +Destruct<T>>(libfuncs: NullableLibfuncs<T>) {
    match libfuncs {
        NullableLibfuncs::Default => use_and_panic(Default::<Nullable<T>>::default()),
        NullableLibfuncs::New(value) => use_and_panic(NullableTrait::new(value)),
        NullableLibfuncs::IsNull(nullable) => use_and_panic(nullable.is_null()),
        NullableLibfuncs::Deref(nullable) => use_and_panic(nullable.deref()),
    }
}
use core::circuit::{
    AddInputResultTrait, CircuitElement, CircuitInput, CircuitInputs, CircuitModulus,
    CircuitOutputsTrait, EvalCircuitTrait, circuit_add, circuit_inverse, circuit_mul, circuit_sub,
    u384,
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
use starknet::secp256_trait::{Secp256PointTrait, Secp256Trait, Signature, is_valid_signature};

fn secp_libfuncs<
    Secp256Point,
    +Drop<Secp256Point>,
    impl Secp256Impl: Secp256Trait<Secp256Point>,
    +Secp256PointTrait<Secp256Point>,
>(
    sign: (u256, u256, u256, u256),
) {
    let (a, b, c, x) = sign;
    let p = Secp256Impl::secp256_ec_get_point_from_x_syscall(x, true).unwrap().unwrap();
    use_and_panic(is_valid_signature(a, b, c, p));
}

#[starknet::interface]
trait Foo<TContractState> {
    fn foo(ref self: TContractState);
}
use starknet::syscalls;

fn starknet_libfuncs(libfuncs: StarknetLibfuncs) {
    match libfuncs {
        StarknetLibfuncs::LibraryCall(class_hash) => FooLibraryDispatcher { class_hash }.foo(),
        StarknetLibfuncs::ContractCall(contract_address) => FooDispatcher { contract_address }
            .foo(),
        StarknetLibfuncs::StorageRead(storage) => use_and_panic(storage.entry(0).high.read()),
        StarknetLibfuncs::StorageWrite(storage) => use_and_panic(storage.write("0")),
        StarknetLibfuncs::Deploy((
            class_hash, address, code, is_init,
        )) => use_and_panic(syscalls::deploy_syscall(class_hash, address, code, is_init)),
        StarknetLibfuncs::EmitEvent((
            keys, data,
        )) => use_and_panic(syscalls::emit_event_syscall(keys, data)),
        StarknetLibfuncs::GetBlockHash(block_number) => use_and_panic(
            syscalls::get_block_hash_syscall(block_number),
        ),
        StarknetLibfuncs::GetExecutionInfo => use_and_panic(syscalls::get_execution_info_syscall()),
        StarknetLibfuncs::GetExecutionInfoV2 => use_and_panic(
            syscalls::get_execution_info_v2_syscall(),
        ),
        StarknetLibfuncs::ReplaceClass(class_hash) => use_and_panic(
            syscalls::replace_class_syscall(class_hash),
        ),
        StarknetLibfuncs::SendMessageToL1((
            address, data,
        )) => use_and_panic(syscalls::send_message_to_l1_syscall(address, data)),
        StarknetLibfuncs::GetClassHashAt(address) => use_and_panic(
            syscalls::get_class_hash_at_syscall(address),
        ),
    }
}

extern fn felt252_const<const VALUE: felt252>() -> felt252 nopanic;
extern fn u8_const<const VALUE: u8>() -> u8 nopanic;
extern fn u16_const<const VALUE: u16>() -> u16 nopanic;
extern fn u32_const<const VALUE: u32>() -> u32 nopanic;
extern fn u64_const<const VALUE: u64>() -> u64 nopanic;
extern fn u128_const<const VALUE: u128>() -> u128 nopanic;
extern fn i8_const<const VALUE: i8>() -> i8 nopanic;
extern fn i16_const<const VALUE: i16>() -> i16 nopanic;
extern fn i32_const<const VALUE: i32>() -> i32 nopanic;
extern fn i64_const<const VALUE: i64>() -> i64 nopanic;
extern fn i128_const<const VALUE: i128>() -> i128 nopanic;
extern fn bytes31_const<const VALUE: felt252>() -> bytes31 nopanic;

#[feature("deprecated-starknet-consts")]
fn consts_libfuncs(libfuncs: ConstsLibfuncs) {
    match libfuncs {
        ConstsLibfuncs::Felt252 => use_and_panic(felt252_const::<0>()),
        ConstsLibfuncs::U8 => use_and_panic(u8_const::<0>()),
        ConstsLibfuncs::U16 => use_and_panic(u16_const::<0>()),
        ConstsLibfuncs::U32 => use_and_panic(u32_const::<0>()),
        ConstsLibfuncs::U64 => use_and_panic(u64_const::<0>()),
        ConstsLibfuncs::U128 => use_and_panic(u128_const::<0>()),
        ConstsLibfuncs::I8 => use_and_panic(i8_const::<0>()),
        ConstsLibfuncs::I16 => use_and_panic(i16_const::<0>()),
        ConstsLibfuncs::I32 => use_and_panic(i32_const::<0>()),
        ConstsLibfuncs::I64 => use_and_panic(i64_const::<0>()),
        ConstsLibfuncs::I128 => use_and_panic(i128_const::<0>()),
        ConstsLibfuncs::Byte31 => use_and_panic(bytes31_const::<0>()),
        ConstsLibfuncs::StorageBase => use_and_panic(
            starknet::storage_access::storage_base_address_const::<0>(),
        ),
        ConstsLibfuncs::ClassHash => use_and_panic(starknet::class_hash::class_hash_const::<0>()),
        ConstsLibfuncs::ContractAddress => use_and_panic(
            starknet::contract_address::contract_address_const::<0>(),
        ),
    }
}

fn snapshot_libfuncs(libfuncs: SnapshotLibfuncs) {
    match libfuncs {
        SnapshotLibfuncs::BoxForward(snap) => use_and_panic(snap.as_snapshot()),
        SnapshotLibfuncs::Match(snap) => match snap {
            Some(value) => use_and_panic(value),
            None => {},
        },
    }
}

#[inline(never)]
fn use_and_panic<T, +PanicDestruct<T>>(t: T) {
    panic!();
}

#[inline(never)]
fn use_and_panic_drop<T, +Drop<T>>(t: T) {
    panic!();
}
