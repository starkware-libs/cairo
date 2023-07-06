use super::ap_tracking::ApTrackingLibfunc;
use super::array::{ArrayLibfunc, ArrayType};
use super::bitwise::BitwiseType;
use super::boolean::BoolLibfunc;
use super::branch_align::BranchAlignLibfunc;
use super::bytes31::{Bytes31Libfunc, Bytes31Type};
use super::casts::CastLibfunc;
use super::debug::DebugLibfunc;
use super::drop::DropLibfunc;
use super::duplicate::DupLibfunc;
use super::ec::{EcLibfunc, EcOpType, EcPointType, EcStateType};
use super::enm::{EnumLibfunc, EnumType};
use super::felt252_dict::{
    Felt252DictEntryLibfunc, Felt252DictEntryType, Felt252DictLibfunc, Felt252DictType,
};
use super::gas::BuiltinCostsType;
use super::int::signed::{
    Sint16Libfunc, Sint16Type, Sint32Libfunc, Sint32Type, Sint64Libfunc, Sint64Type, Sint8Libfunc,
    Sint8Type,
};
use super::int::signed128::{Sint128Libfunc, Sint128Type};
use super::int::unsigned::{
    Uint16Libfunc, Uint16Type, Uint32Libfunc, Uint32Type, Uint64Libfunc, Uint64Type, Uint8Libfunc,
    Uint8Type,
};
use super::int::unsigned128::{U128MulGuaranteeType, Uint128Libfunc, Uint128Type};
use super::int::unsigned256::Uint256Libfunc;
use super::int::unsigned512::Uint512Libfunc;
use super::modules::boxing::{BoxLibfunc, BoxType};
use super::modules::felt252::{Felt252Libfunc, Felt252Type};
use super::modules::function_call::FunctionCallLibfunc;
use super::modules::gas::{GasBuiltinType, GasLibfunc};
use super::modules::mem::MemLibfunc;
use super::modules::non_zero::{NonZeroType, UnwrapNonZeroLibfunc};
use super::modules::unconditional_jump::UnconditionalJumpLibfunc;
use super::nullable::{NullableLibfunc, NullableType};
use super::pedersen::{PedersenLibfunc, PedersenType};
use super::poseidon::{PoseidonLibfunc, PoseidonType};
use super::range_check::RangeCheckType;
use super::segment_arena::SegmentArenaType;
use super::snapshot::{SnapshotTakeLibfunc, SnapshotType};
use super::span::SpanType;
use super::squashed_felt252_dict::SquashedFelt252DictType;
use super::starknet::{StarkNetLibfunc, StarkNetType};
use super::structure::{StructLibfunc, StructType};
use super::uninitialized::UninitializedType;
use crate::{define_libfunc_hierarchy, define_type_hierarchy};

define_type_hierarchy! {
    pub enum CoreType {
        Array(ArrayType),
        Bitwise(BitwiseType),
        Box(BoxType),
        EcOp(EcOpType),
        EcPoint(EcPointType),
        EcState(EcStateType),
        Felt252(Felt252Type),
        GasBuiltin(GasBuiltinType),
        BuiltinCosts(BuiltinCostsType),
        Uint8(Uint8Type),
        Uint16(Uint16Type),
        Uint32(Uint32Type),
        Uint64(Uint64Type),
        Uint128(Uint128Type),
        Uint128MulGuarantee(U128MulGuaranteeType),
        Sint8(Sint8Type),
        Sint16(Sint16Type),
        Sint32(Sint32Type),
        Sint64(Sint64Type),
        Sint128(Sint128Type),
        NonZero(NonZeroType),
        Nullable(NullableType),
        RangeCheck(RangeCheckType),
        Uninitialized(UninitializedType),
        Enum(EnumType),
        Struct(StructType),
        Felt252Dict(Felt252DictType),
        Felt252DictEntry(Felt252DictEntryType),
        SquashedFelt252Dict(SquashedFelt252DictType),
        Pedersen(PedersenType),
        Poseidon(PoseidonType),
        Span(SpanType),
        StarkNet(StarkNetType),
        SegmentArena(SegmentArenaType),
        Snapshot(SnapshotType),
        Bytes31(Bytes31Type),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibfunc {
        ApTracking(ApTrackingLibfunc),
        Array(ArrayLibfunc),
        BranchAlign(BranchAlignLibfunc),
        Bool(BoolLibfunc),
        Box(BoxLibfunc),
        Cast(CastLibfunc),
        Drop(DropLibfunc),
        Dup(DupLibfunc),
        Ec(EcLibfunc),
        Felt252(Felt252Libfunc),
        FunctionCall(FunctionCallLibfunc),
        Gas(GasLibfunc),
        Uint8(Uint8Libfunc),
        Uint16(Uint16Libfunc),
        Uint32(Uint32Libfunc),
        Uint64(Uint64Libfunc),
        Uint128(Uint128Libfunc),
        Uint256(Uint256Libfunc),
        Uint512(Uint512Libfunc),
        Sint8(Sint8Libfunc),
        Sint16(Sint16Libfunc),
        Sint32(Sint32Libfunc),
        Sint64(Sint64Libfunc),
        Sint128(Sint128Libfunc),
        Mem(MemLibfunc),
        Nullable(NullableLibfunc),
        UnwrapNonZero(UnwrapNonZeroLibfunc),
        UnconditionalJump(UnconditionalJumpLibfunc),
        Enum(EnumLibfunc),
        Struct(StructLibfunc),
        Felt252Dict(Felt252DictLibfunc),
        Felt252DictEntry(Felt252DictEntryLibfunc),
        Pedersen(PedersenLibfunc),
        Poseidon(PoseidonLibfunc),
        StarkNet(StarkNetLibfunc),
        Debug(DebugLibfunc),
        SnapshotTake(SnapshotTakeLibfunc),
        Bytes31(Bytes31Libfunc),
    }, CoreConcreteLibfunc
}
