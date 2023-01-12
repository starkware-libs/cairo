use super::ap_tracking::RevokeApTrackingLibfunc;
use super::array::{ArrayLibfunc, ArrayType};
use super::bitwise::{BitwiseLibfunc, BitwiseType};
use super::boolean::BoolLibfunc;
use super::branch_align::BranchAlignLibfunc;
use super::builtin_cost::{BuiltinCostLibfunc, BuiltinCostsType};
use super::debug::DebugLibfunc;
use super::dict_felt_to::{DictFeltToLibfunc, DictFeltToType};
use super::dict_manager::DictManagerType;
use super::drop::DropLibfunc;
use super::duplicate::DupLibfunc;
use super::ec::{EcLibfunc, EcOpType, EcPointType, EcStateType};
use super::enm::{EnumLibfunc, EnumType};
use super::modules::boxing::{BoxLibfunc, BoxType};
use super::modules::felt::{FeltLibfunc, FeltType};
use super::modules::function_call::FunctionCallLibfunc;
use super::modules::gas::{GasBuiltinType, GasLibfunc};
use super::modules::mem::MemLibfunc;
use super::modules::non_zero::{NonZeroType, UnwrapNonZeroLibfunc};
use super::modules::uint128::{Uint128Libfunc, Uint128Type};
use super::modules::unconditional_jump::UnconditionalJumpLibfunc;
use super::nullable::{NullableLibfunc, NullableType};
use super::pedersen::{PedersenLibfunc, PedersenType};
use super::range_check::RangeCheckType;
use super::squashed_dict_felt_to::SquashedDictFeltToType;
use super::starknet::{StarkNetLibfunc, StarkNetType};
use super::strct::{StructLibfunc, StructType};
use super::uint::{Uint8Libfunc, Uint8Type};
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
        Felt(FeltType),
        GasBuiltin(GasBuiltinType),
        BuiltinCosts(BuiltinCostsType),
        Uint128(Uint128Type),
        Uint8(Uint8Type),
        NonZero(NonZeroType),
        Nullable(NullableType),
        RangeCheck(RangeCheckType),
        Uninitialized(UninitializedType),
        Enum(EnumType),
        Struct(StructType),
        DictFeltTo(DictFeltToType),
        SquashedDictFeltTo(SquashedDictFeltToType),
        Pedersen(PedersenType),
        StarkNet(StarkNetType),
        DictManager(DictManagerType),
    }, CoreTypeConcrete
}

define_libfunc_hierarchy! {
    pub enum CoreLibfunc {
        ApTracking(RevokeApTrackingLibfunc),
        Array(ArrayLibfunc),
        Bitwise(BitwiseLibfunc),
        BranchAlign(BranchAlignLibfunc),
        Bool(BoolLibfunc),
        Box(BoxLibfunc),
        BuiltinCost(BuiltinCostLibfunc),
        Drop(DropLibfunc),
        Dup(DupLibfunc),
        Ec(EcLibfunc),
        Felt(FeltLibfunc),
        FunctionCall(FunctionCallLibfunc),
        Gas(GasLibfunc),
        Uint128(Uint128Libfunc),
        Uint8(Uint8Libfunc),
        Mem(MemLibfunc),
        Nullable(NullableLibfunc),
        UnwrapNonZero(UnwrapNonZeroLibfunc),
        UnconditionalJump(UnconditionalJumpLibfunc),
        Enum(EnumLibfunc),
        Struct(StructLibfunc),
        DictFeltTo(DictFeltToLibfunc),
        Pedersen(PedersenLibfunc),
        StarkNet(StarkNetLibfunc),
        Debug(DebugLibfunc),
    }, CoreConcreteLibfunc
}
