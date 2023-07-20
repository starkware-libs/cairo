use std::fmt::{Display, Formatter};

use cairo_lang_utils::bigint::BigIntAsHex;
use indoc::formatdoc;
use parity_scale_codec_derive::{Decode, Encode};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::operand::{CellRef, DerefOrImmediate, ResOperand};

#[cfg(test)]
mod test;

// Represents a cairo hint.
// Note: Hint encoding should be backwards-compatible. This is an API guarantee.
// For example, new variants should have new `index`.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Encode, Decode, JsonSchema)]
#[serde(untagged)]
pub enum Hint {
    #[codec(index = 0)]
    Core(CoreHintBase),
    #[codec(index = 1)]
    Starknet(StarknetHint),
}

impl Hint {
    pub fn representing_string(&self) -> String {
        format!("{:?}", self)
    }
}

impl From<CoreHint> for Hint {
    fn from(value: CoreHint) -> Self {
        Hint::Core(value.into())
    }
}
impl From<StarknetHint> for Hint {
    fn from(value: StarknetHint) -> Self {
        Hint::Starknet(value)
    }
}

/// A trait for displaying the pythonic version of a hint.
/// Should only be used from within the compiler.
pub trait PythonicHint {
    fn get_pythonic_hint(&self) -> String;
}

impl PythonicHint for Hint {
    fn get_pythonic_hint(&self) -> String {
        match self {
            Hint::Core(hint) => hint.get_pythonic_hint(),
            Hint::Starknet(hint) => hint.get_pythonic_hint(),
        }
    }
}

/// Represents a hint that triggers a system call.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Encode, Decode, JsonSchema)]
pub enum StarknetHint {
    #[codec(index = 0)]
    SystemCall { system: ResOperand },
    #[codec(index = 1)]
    Cheatcode {
        selector: BigIntAsHex,
        input_start: ResOperand,
        input_end: ResOperand,
        output_start: CellRef,
        output_end: CellRef,
    },
}

// Represents a cairo core hint.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Encode, Decode, JsonSchema)]
#[serde(untagged)]
pub enum CoreHintBase {
    #[codec(index = 0)]
    Core(CoreHint),
    #[codec(index = 1)]
    Deprecated(DeprecatedHint),
}

impl From<CoreHint> for CoreHintBase {
    fn from(value: CoreHint) -> Self {
        CoreHintBase::Core(value)
    }
}
impl From<DeprecatedHint> for CoreHintBase {
    fn from(value: DeprecatedHint) -> Self {
        CoreHintBase::Deprecated(value)
    }
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Encode, Decode, JsonSchema)]
pub enum CoreHint {
    #[codec(index = 0)]
    AllocSegment { dst: CellRef },
    #[codec(index = 1)]
    TestLessThan { lhs: ResOperand, rhs: ResOperand, dst: CellRef },
    #[codec(index = 2)]
    TestLessThanOrEqual { lhs: ResOperand, rhs: ResOperand, dst: CellRef },
    /// Multiplies two 128-bit integers and returns two 128-bit integers: the high and low parts of
    /// the product.
    #[codec(index = 3)]
    WideMul128 { lhs: ResOperand, rhs: ResOperand, high: CellRef, low: CellRef },
    /// Computes lhs/rhs and returns the quotient and remainder.
    ///
    /// Note: the hint may be used to write an already assigned memory cell.
    #[codec(index = 4)]
    DivMod { lhs: ResOperand, rhs: ResOperand, quotient: CellRef, remainder: CellRef },
    /// Divides dividend (represented by 2 128bit limbs) by divisor (represented by 2 128bit
    /// limbs). Returns the quotient (represented by 2 128bit limbs) and remainder (represented by
    /// 2 128bit limbs). In all cases - `name`0 is the least significant limb.
    #[codec(index = 5)]
    Uint256DivMod {
        dividend0: ResOperand,
        dividend1: ResOperand,
        divisor0: ResOperand,
        divisor1: ResOperand,
        quotient0: CellRef,
        quotient1: CellRef,
        remainder0: CellRef,
        remainder1: CellRef,
    },
    /// Divides dividend (represented by 4 128bit limbs) by divisor (represented by 2 128bit
    /// limbs). Returns the quotient (represented by 4 128bit limbs) and remainder (represented
    /// by 2 128bit limbs).
    /// In all cases - `name`0 is the least significant limb.
    #[codec(index = 6)]
    Uint512DivModByUint256 {
        dividend0: ResOperand,
        dividend1: ResOperand,
        dividend2: ResOperand,
        dividend3: ResOperand,
        divisor0: ResOperand,
        divisor1: ResOperand,
        quotient0: CellRef,
        quotient1: CellRef,
        quotient2: CellRef,
        quotient3: CellRef,
        remainder0: CellRef,
        remainder1: CellRef,
    },
    #[codec(index = 7)]
    SquareRoot { value: ResOperand, dst: CellRef },
    /// Computes the square root of value_low<<128+value_high, stores the 64bit limbs of the result
    /// in sqrt0 and sqrt1 as well as the 128bit limbs of the remainder in remainder_low and
    /// remainder_high. The remainder is defined as `value - sqrt**2`.
    /// Lastly it checks weather `2*sqrt - remainder >= 2**128`.
    #[codec(index = 8)]
    Uint256SquareRoot {
        value_low: ResOperand,
        value_high: ResOperand,
        sqrt0: CellRef,
        sqrt1: CellRef,
        remainder_low: CellRef,
        remainder_high: CellRef,
        sqrt_mul_2_minus_remainder_ge_u128: CellRef,
    },
    /// Finds some `x` and `y` such that `x * scalar + y = value` and `x <= max_x`.
    #[codec(index = 9)]
    LinearSplit { value: ResOperand, scalar: ResOperand, max_x: ResOperand, x: CellRef, y: CellRef },
    /// Allocates a new dict segment, and write its start address into the dict_infos segment.
    #[codec(index = 10)]
    AllocFelt252Dict { segment_arena_ptr: ResOperand },
    /// Fetch the previous value of a key in a dict, and write it in a new dict access.
    #[codec(index = 11)]
    Felt252DictEntryInit { dict_ptr: ResOperand, key: ResOperand },
    /// Similar to Felt252DictWrite, but updates an existing entry and does not wirte the previous
    /// value to the stack.
    #[codec(index = 12)]
    Felt252DictEntryUpdate { dict_ptr: ResOperand, value: ResOperand },
    /// Retrieves the index of the given dict in the dict_infos segment.
    #[codec(index = 13)]
    GetSegmentArenaIndex { dict_end_ptr: ResOperand, dict_index: CellRef },
    /// Initialized the lists of accesses of each key of a dict as a preparation of squash_dict.
    #[codec(index = 14)]
    InitSquashData {
        dict_accesses: ResOperand,
        ptr_diff: ResOperand,
        n_accesses: ResOperand,
        big_keys: CellRef,
        first_key: CellRef,
    },
    /// Retrieves the current index of a dict access to process.
    #[codec(index = 15)]
    GetCurrentAccessIndex { range_check_ptr: ResOperand },
    /// Writes if the squash_dict loop should be skipped.
    #[codec(index = 16)]
    ShouldSkipSquashLoop { should_skip_loop: CellRef },
    /// Writes the delta from the current access index to the next one.
    #[codec(index = 17)]
    GetCurrentAccessDelta { index_delta_minus1: CellRef },
    /// Writes if the squash_dict loop should be continued.
    #[codec(index = 18)]
    ShouldContinueSquashLoop { should_continue: CellRef },
    /// Writes the next dict key to process.
    #[codec(index = 19)]
    GetNextDictKey { next_key: CellRef },
    /// Finds the two small arcs from within [(0,a),(a,b),(b,PRIME)] and writes it to the
    /// range_check segment.
    #[codec(index = 20)]
    AssertLeFindSmallArcs { range_check_ptr: ResOperand, a: ResOperand, b: ResOperand },
    /// Writes if the arc (0,a) was excluded.
    #[codec(index = 21)]
    AssertLeIsFirstArcExcluded { skip_exclude_a_flag: CellRef },
    /// Writes if the arc (a,b) was excluded.
    #[codec(index = 22)]
    AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a: CellRef },
    /// Samples a random point on the EC.
    #[codec(index = 23)]
    RandomEcPoint { x: CellRef, y: CellRef },
    /// Computes the square root of `val`, if `val` is a quadratic residue, and of `3 * val`
    /// otherwise.
    ///
    /// Since 3 is not a quadratic residue, exactly one of `val` and `3 * val` is a quadratic
    /// residue (unless `val` is 0). This allows proving that `val` is not a quadratic residue.
    #[codec(index = 24)]
    FieldSqrt { val: ResOperand, sqrt: CellRef },
    /// Prints the values from start to end.
    /// Both must be pointers.
    #[codec(index = 25)]
    DebugPrint { start: ResOperand, end: ResOperand },
    /// Returns an address with `size` free locations afterwards.
    #[codec(index = 26)]
    AllocConstantSize { size: ResOperand, dst: CellRef },
}

/// Represents a deprecated hint which is kept for backward compatibility of previously deployed
/// contracts.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone, Encode, Decode, JsonSchema)]
pub enum DeprecatedHint {
    /// Asserts that the current access indices list is empty (after the loop).
    #[codec(index = 0)]
    AssertCurrentAccessIndicesIsEmpty,
    /// Asserts that the number of used accesses is equal to the length of the original accesses
    /// list.
    #[codec(index = 1)]
    AssertAllAccessesUsed { n_used_accesses: CellRef },
    /// Asserts that the keys list is empty.
    #[codec(index = 2)]
    AssertAllKeysUsed,
    /// Asserts that the arc (b, PRIME) was excluded.
    #[codec(index = 3)]
    AssertLeAssertThirdArcExcluded,
    /// Asserts that the input represents integers and that a<b.
    #[codec(index = 4)]
    AssertLtAssertValidInput { a: ResOperand, b: ResOperand },
    /// Retrieves and writes the value corresponding to the given dict and key from the vm
    /// dict_manager.
    #[codec(index = 5)]
    Felt252DictRead { dict_ptr: ResOperand, key: ResOperand, value_dst: CellRef },
    /// Sets the value corresponding to the key in the vm dict_manager.
    #[codec(index = 6)]
    Felt252DictWrite { dict_ptr: ResOperand, key: ResOperand, value: ResOperand },
}

struct DerefOrImmediateFormatter<'a>(&'a DerefOrImmediate);
impl<'a> Display for DerefOrImmediateFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            DerefOrImmediate::Deref(d) => write!(f, "memory{d}"),
            DerefOrImmediate::Immediate(i) => write!(f, "{}", i.value),
        }
    }
}

struct ResOperandFormatter<'a>(&'a ResOperand);
impl<'a> Display for ResOperandFormatter<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            ResOperand::Deref(d) => write!(f, "memory{d}"),
            ResOperand::DoubleDeref(d, i) => write!(f, "memory[memory{d} + {i}]"),
            ResOperand::Immediate(i) => write!(f, "{}", i.value),
            ResOperand::BinOp(bin_op) => {
                write!(
                    f,
                    "memory{} {} {}",
                    bin_op.a,
                    bin_op.op,
                    DerefOrImmediateFormatter(&bin_op.b)
                )
            }
        }
    }
}

impl PythonicHint for CoreHintBase {
    fn get_pythonic_hint(&self) -> String {
        match self {
            CoreHintBase::Core(hint) => hint.get_pythonic_hint(),
            CoreHintBase::Deprecated(_) => {
                unreachable!("Deprecated hints do not have a pythonic version.")
            }
        }
    }
}

impl PythonicHint for CoreHint {
    fn get_pythonic_hint(&self) -> String {
        match self {
            CoreHint::AllocSegment { dst } => format!("memory{dst} = segments.add()"),
            CoreHint::AllocFelt252Dict { segment_arena_ptr } => {
                let segment_arena_ptr = ResOperandFormatter(segment_arena_ptr);
                formatdoc!(
                    "

                        if '__dict_manager' not in globals():
                            from starkware.cairo.common.dict import DictManager
                            __dict_manager = DictManager()

                        if '__segment_index_to_arena_index' not in globals():
                            # A map from the relocatable value segment index to the index in the
                            # arena.
                            __segment_index_to_arena_index = {{}}

                        # {segment_arena_ptr} is the address of the next SegmentArenaBuiltin.
                        # memory[{segment_arena_ptr} - 2] is the number of allocated segments.
                        index = memory[{segment_arena_ptr} - 2]

                        segment_start = __dict_manager.new_default_dict(
                            segments, 0, temp_segment=index > 0
                        )

                        # Update '__segment_index_to_arena_index'.
                        __segment_index_to_arena_index[segment_start.segment_index] = index

                        # Update 'SegmentInfo::start'.
                        # memory[{segment_arena_ptr} - 3] is the address of the segment arena infos
                        # segment. index * 3 is added to get the address of the new SegmentInfo.
                        memory[memory[{segment_arena_ptr} - 3] + index * 3] = segment_start
                "
                )
            }
            CoreHint::Felt252DictEntryInit { dict_ptr, key } => {
                let (dict_ptr, key) = (ResOperandFormatter(dict_ptr), ResOperandFormatter(key));
                formatdoc!(
                    "

                    dict_tracker = __dict_manager.get_tracker({dict_ptr})
                    dict_tracker.current_ptr += 3
                    memory[{dict_ptr} + 1] = dict_tracker.data[{key}]
                    "
                )
            }
            CoreHint::Felt252DictEntryUpdate { dict_ptr, value } => {
                let (dict_ptr, value) = (ResOperandFormatter(dict_ptr), ResOperandFormatter(value));
                formatdoc!(
                    "

                    dict_tracker = __dict_manager.get_tracker({dict_ptr})
                    dict_tracker.data[memory[{dict_ptr} - 3]] = {value}
                    "
                )
            }
            CoreHint::TestLessThan { lhs, rhs, dst } => {
                format!("memory{dst} = {} < {}", ResOperandFormatter(lhs), ResOperandFormatter(rhs))
            }
            CoreHint::TestLessThanOrEqual { lhs, rhs, dst } => format!(
                "memory{dst} = {} <= {}",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::WideMul128 { lhs, rhs, high, low } => format!(
                "(memory{high}, memory{low}) = divmod({} * {}, 2**128)",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::DivMod { lhs, rhs, quotient, remainder } => format!(
                "(memory{quotient}, memory{remainder}) = divmod({}, {})",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::Uint256DivMod {
                dividend0,
                dividend1,
                quotient0,
                quotient1,
                divisor0,
                divisor1,
                remainder0,
                remainder1,
            } => {
                let (dividend0, dividend1, divisor0, divisor1) = (
                    ResOperandFormatter(dividend0),
                    ResOperandFormatter(dividend1),
                    ResOperandFormatter(divisor0),
                    ResOperandFormatter(divisor1),
                );
                formatdoc!(
                    "

                        dividend = {dividend0} + {dividend1} * 2**128
                        divisor = {divisor0} + {divisor1} * 2**128
                        quotient, remainder = divmod(dividend, divisor)
                        memory{quotient0} = quotient & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                        memory{quotient1} = quotient >> 128
                        memory{remainder0} = remainder & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                        memory{remainder1} = remainder >> 128
                    "
                )
            }
            CoreHint::Uint512DivModByUint256 {
                dividend0,
                dividend1,
                dividend2,
                dividend3,
                divisor0,
                divisor1,
                quotient0,
                quotient1,
                quotient2,
                quotient3,
                remainder0,
                remainder1,
            } => {
                let [dividend0, dividend1, dividend2, dividend3, divisor0, divisor1] =
                    [dividend0, dividend1, dividend2, dividend3, divisor0, divisor1]
                        .map(ResOperandFormatter);
                formatdoc!(
                    "

                    dividend = {dividend0} + {dividend1} * 2**128 + {dividend2} * 2**256 + \
                     {dividend3} * 2**384
                    divisor = {divisor0} + {divisor1} * 2**128
                    quotient, remainder = divmod(dividend, divisor)
                    memory{quotient0} = quotient & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    memory{quotient1} = (quotient >> 128) & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    memory{quotient2} = (quotient >> 256) & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    memory{quotient3} = quotient >> 384
                    memory{remainder0} = remainder & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                    memory{remainder1} = remainder >> 128
                "
                )
            }
            CoreHint::SquareRoot { value, dst } => {
                formatdoc!(
                    "

                        import math
                        memory{dst} = math.isqrt({})
                    ",
                    ResOperandFormatter(value)
                )
            }
            CoreHint::Uint256SquareRoot {
                value_low,
                value_high,
                sqrt0,
                sqrt1,
                remainder_low,
                remainder_high,
                sqrt_mul_2_minus_remainder_ge_u128,
            } => {
                let (value_low, value_high) =
                    (ResOperandFormatter(value_low), ResOperandFormatter(value_high));
                formatdoc!(
                    "

                        import math;
                        value = {value_low} + {value_high} * 2**128
                        root = math.isqrt(value)
                        remainder = value - root ** 2
                        memory{sqrt0} = root & 0xFFFFFFFFFFFFFFFF
                        memory{sqrt1} = root >> 64
                        memory{remainder_low} = remainder & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                        memory{remainder_high} = remainder >> 128
                        memory{sqrt_mul_2_minus_remainder_ge_u128} = root * 2 - remainder >= 2**128
                    "
                )
            }
            CoreHint::LinearSplit { value, scalar, max_x, x, y } => {
                let (value, scalar, max_x) = (
                    ResOperandFormatter(value),
                    ResOperandFormatter(scalar),
                    ResOperandFormatter(max_x),
                );
                formatdoc!(
                    "

                        (value, scalar) = ({value}, {scalar})
                        x = min(value // scalar, {max_x})
                        y = value - x * scalar
                        memory{x} = x
                        memory{y} = y
                    "
                )
            }
            CoreHint::RandomEcPoint { x, y } => {
                formatdoc!(
                    "

                        from starkware.crypto.signature.signature import ALPHA, BETA, FIELD_PRIME
                        from starkware.python.math_utils import random_ec_point
                        (memory{x}, memory{y}) = random_ec_point(FIELD_PRIME, ALPHA, BETA)
                    "
                )
            }
            CoreHint::FieldSqrt { val, sqrt } => {
                formatdoc!(
                    "

                        from starkware.crypto.signature.signature import FIELD_PRIME
                        from starkware.python.math_utils import is_quad_residue, sqrt

                        val = {}
                        if is_quad_residue(val, FIELD_PRIME):
                            memory{sqrt} = sqrt(val, FIELD_PRIME)
                        else:
                            memory{sqrt} = sqrt(val * 3, FIELD_PRIME)
                        ",
                    ResOperandFormatter(val)
                )
            }
            CoreHint::GetCurrentAccessIndex { range_check_ptr } => formatdoc!(
                "

                    current_access_indices = sorted(access_indices[key])[::-1]
                    current_access_index = current_access_indices.pop()
                    memory[{}] = current_access_index
                ",
                ResOperandFormatter(range_check_ptr)
            ),
            CoreHint::ShouldSkipSquashLoop { should_skip_loop } => {
                format!("memory{should_skip_loop} = 0 if current_access_indices else 1")
            }
            CoreHint::GetCurrentAccessDelta { index_delta_minus1 } => formatdoc!(
                "

                    new_access_index = current_access_indices.pop()
                    memory{index_delta_minus1} = new_access_index - current_access_index - 1
                    current_access_index = new_access_index
                "
            ),
            CoreHint::ShouldContinueSquashLoop { should_continue } => {
                format!("memory{should_continue} = 1 if current_access_indices else 0")
            }
            CoreHint::GetNextDictKey { next_key } => formatdoc!(
                "
                    assert len(keys) > 0, 'No keys left but remaining_accesses > 0.'
                    memory{next_key} = key = keys.pop()
                "
            ),
            CoreHint::GetSegmentArenaIndex { dict_end_ptr, dict_index } => {
                let dict_end_ptr = ResOperandFormatter(dict_end_ptr);
                formatdoc!(
                    "

                    memory{dict_index} = __segment_index_to_arena_index[
                        {dict_end_ptr}.segment_index
                    ]
                "
                )
            }
            CoreHint::InitSquashData {
                dict_accesses,
                ptr_diff,
                n_accesses,
                big_keys,
                first_key,
            } => {
                let (dict_accesses, ptr_diff, n_accesses) = (
                    ResOperandFormatter(dict_accesses),
                    ResOperandFormatter(ptr_diff),
                    ResOperandFormatter(n_accesses),
                );
                formatdoc!(
                    "

                    dict_access_size = 3
                    address = {dict_accesses}
                    assert {ptr_diff} % dict_access_size == 0, 'Accesses array size must be \
                     divisible by DictAccess.SIZE'
                    n_accesses = {n_accesses}
                    if '__squash_dict_max_size' in globals():
                        assert n_accesses <= __squash_dict_max_size, f'squash_dict() can only be \
                     used with n_accesses<={{__squash_dict_max_size}}. ' f'Got: \
                     n_accesses={{n_accesses}}.'
                    # A map from key to the list of indices accessing it.
                    access_indices = {{}}
                    for i in range(n_accesses):
                        key = memory[address + dict_access_size * i]
                        access_indices.setdefault(key, []).append(i)
                    # Descending list of keys.
                    keys = sorted(access_indices.keys(), reverse=True)
                    # Are the keys used bigger than range_check bound.
                    memory{big_keys} = 1 if keys[0] >= range_check_builtin.bound else 0
                    memory{first_key} = key = keys.pop()
                "
                )
            }
            CoreHint::AssertLeFindSmallArcs { range_check_ptr, a, b } => {
                let (range_check_ptr, a, b) = (
                    ResOperandFormatter(range_check_ptr),
                    ResOperandFormatter(a),
                    ResOperandFormatter(b),
                );
                formatdoc!(
                    "

                    import itertools

                    from starkware.cairo.common.math_utils import assert_integer
                    assert_integer({a})
                    assert_integer({b})
                    a = {a} % PRIME
                    b = {b} % PRIME
                    assert a <= b, f'a = {{a}} is not less than or equal to b = {{b}}.'

                    # Find an arc less than PRIME / 3, and another less than PRIME / 2.
                    lengths_and_indices = [(a, 0), (b - a, 1), (PRIME - 1 - b, 2)]
                    lengths_and_indices.sort()
                    assert lengths_and_indices[0][0] <= PRIME // 3 and lengths_and_indices[1][0] \
                     <= PRIME // 2
                    excluded = lengths_and_indices[2][1]

                    memory[{range_check_ptr} + 1], memory[{range_check_ptr} + 0] = (
                        divmod(lengths_and_indices[0][0], 3544607988759775765608368578435044694))
                    memory[{range_check_ptr} + 3], memory[{range_check_ptr} + 2] = (
                        divmod(lengths_and_indices[1][0], 5316911983139663648412552867652567041))
                "
                )
            }
            CoreHint::AssertLeIsFirstArcExcluded { skip_exclude_a_flag } => {
                format!("memory{skip_exclude_a_flag} = 1 if excluded != 0 else 0",)
            }
            CoreHint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
                format!("memory{skip_exclude_b_minus_a} = 1 if excluded != 1 else 0",)
            }
            CoreHint::DebugPrint { start, end } => formatdoc!(
                "

                    curr = {}
                    end = {}
                    while curr != end:
                        print(hex(memory[curr]))
                        curr += 1
                ",
                ResOperandFormatter(start),
                ResOperandFormatter(end),
            ),
            CoreHint::AllocConstantSize { size, dst } => {
                formatdoc!(
                    "

                        if '__boxed_segment' not in globals():
                            __boxed_segment = segments.add()
                        memory{dst} = __boxed_segment
                        __boxed_segment += {}
                    ",
                    ResOperandFormatter(size)
                )
            }
        }
    }
}

impl PythonicHint for StarknetHint {
    fn get_pythonic_hint(&self) -> String {
        match self {
            StarknetHint::SystemCall { system } => {
                format!("syscall_handler.syscall(syscall_ptr={})", ResOperandFormatter(system))
            }
            StarknetHint::Cheatcode { .. } => "raise NotImplementedError".to_string(),
        }
    }
}
