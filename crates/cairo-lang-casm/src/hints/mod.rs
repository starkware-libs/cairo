use std::fmt::{Display, Formatter};

use indoc::writedoc;
use serde::{Deserialize, Serialize};

use crate::operand::{CellRef, DerefOrImmediate, ResOperand};

#[cfg(test)]
mod test;

// Represents a cairo hint.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
#[serde(untagged)]
pub enum Hint {
    Core(CoreHint),
    Starknet(StarknetHint),
}
impl From<CoreHint> for Hint {
    fn from(value: CoreHint) -> Self {
        Hint::Core(value)
    }
}
impl From<StarknetHint> for Hint {
    fn from(value: StarknetHint) -> Self {
        Hint::Starknet(value)
    }
}

impl Display for Hint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Hint::Core(hint) => hint.fmt(f),
            Hint::Starknet(hint) => hint.fmt(f),
        }
    }
}

/// Represents a hint that triggers a system call.
#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub enum StarknetHint {
    SystemCall { system: ResOperand },
    SetBlockNumber { value: ResOperand },
    SetBlockTimestamp { value: ResOperand },
    SetCallerAddress { value: ResOperand },
    SetContractAddress { value: ResOperand },
    SetSequencerAddress { value: ResOperand },
}

#[derive(Serialize, Deserialize, Debug, Eq, PartialEq, Clone)]
pub enum CoreHint {
    AllocSegment {
        dst: CellRef,
    },
    TestLessThan {
        lhs: ResOperand,
        rhs: ResOperand,
        dst: CellRef,
    },
    TestLessThanOrEqual {
        lhs: ResOperand,
        rhs: ResOperand,
        dst: CellRef,
    },
    DivMod {
        lhs: ResOperand,
        rhs: ResOperand,
        quotient: CellRef,
        remainder: CellRef,
    },
    /// Divides dividend_low<<128+dividend_high by divisor_low<<128+divisor_high.
    /// Splits the remainder to 128bit words: remainder_low and remainder_high.
    /// Splits the quotient and the divisor to 128bit words.
    /// The lower 128 bits of the quotient are written to quotient0 and quotient1.
    /// The lower 128 bits of the divisor are written to divisor0 and divisor1.
    /// If the divisor is greater than 2^128, the upper 128 bits of the divisor are written to
    /// extra0 and extra1. In this case, quotient must be lower than 2^128.
    /// Otherwise, the upper 128 bits of the quotient are written to extra0 and extra1.
    Uint256DivMod {
        dividend_low: ResOperand,
        dividend_high: ResOperand,
        divisor_low: ResOperand,
        divisor_high: ResOperand,
        quotient0: CellRef,
        quotient1: CellRef,
        divisor0: CellRef,
        divisor1: CellRef,
        extra0: CellRef,
        extra1: CellRef,
        remainder_low: CellRef,
        remainder_high: CellRef,
    },
    SquareRoot {
        value: ResOperand,
        dst: CellRef,
    },
    /// Computes the square root of value_low<<128+value_high, stores the 64bit limbs of the result
    /// in sqrt0 and sqrt1 as well as the 128bit limbs of the remainder in remainder_low and
    /// remainder_high. The remainder is defined as `value - sqrt**2`.
    /// Lastly it checks weather `2*sqrt - remainder >= 2**128`.
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
    LinearSplit {
        value: ResOperand,
        scalar: ResOperand,
        max_x: ResOperand,
        x: CellRef,
        y: CellRef,
    },
    /// Allocates a new dict segment, and write its start address into the dict_infos segment.
    AllocFelt252Dict {
        segment_arena_ptr: ResOperand,
    },
    /// Retrieves and writes the value corresponding to the given dict and key from the vm
    /// dict_manager.
    Felt252DictRead {
        dict_ptr: ResOperand,
        key: ResOperand,
        value_dst: CellRef,
    },
    /// Sets the value corresponding to the key in the vm dict_manager.
    Felt252DictWrite {
        dict_ptr: ResOperand,
        key: ResOperand,
        value: ResOperand,
    },
    /// Fetch the previous value of a key in a dict, and write it in a new dict access.
    Felt252DictEntryInit {
        dict_ptr: ResOperand,
        key: ResOperand,
    },
    /// Similar to Felt252DictWrite, but updates an existing entry and does not wirte the previous
    /// value to the stack.
    Felt252DictEntryUpdate {
        dict_ptr: ResOperand,
        value: ResOperand,
    },
    /// Retrieves the index of the given dict in the dict_infos segment.
    GetSegmentArenaIndex {
        dict_end_ptr: ResOperand,
        dict_index: CellRef,
    },
    /// Initialized the lists of accesses of each key of a dict as a preparation of squash_dict.
    InitSquashData {
        dict_accesses: ResOperand,
        ptr_diff: ResOperand,
        n_accesses: ResOperand,
        big_keys: CellRef,
        first_key: CellRef,
    },
    /// Retrieves the current index of a dict access to process.
    GetCurrentAccessIndex {
        range_check_ptr: ResOperand,
    },
    /// Writes if the squash_dict loop should be skipped.
    ShouldSkipSquashLoop {
        should_skip_loop: CellRef,
    },
    /// Writes the delta from the current access index to the next one.
    GetCurrentAccessDelta {
        index_delta_minus1: CellRef,
    },
    /// Writes if the squash_dict loop should be continued.
    ShouldContinueSquashLoop {
        should_continue: CellRef,
    },
    /// Asserts that the current access indices list is empty (after the loop).
    AssertCurrentAccessIndicesIsEmpty,
    /// Asserts that the number of used accesses is equal to the length of the original accesses
    /// list.
    AssertAllAccessesUsed {
        n_used_accesses: CellRef,
    },
    /// Asserts that the keys list is empty.
    AssertAllKeysUsed,
    /// Writes the next dict key to process.
    GetNextDictKey {
        next_key: CellRef,
    },
    /// Asserts that the input represents integers and that a<b.
    AssertLtAssertValidInput {
        a: ResOperand,
        b: ResOperand,
    },
    /// Finds the two small arcs from within [(0,a),(a,b),(b,PRIME)] and writes it to the
    /// range_check segment.
    AssertLeFindSmallArcs {
        range_check_ptr: ResOperand,
        a: ResOperand,
        b: ResOperand,
    },
    /// Writes if the arc (0,a) was excluded.
    AssertLeIsFirstArcExcluded {
        skip_exclude_a_flag: CellRef,
    },
    /// Writes if the arc (a,b) was excluded.
    AssertLeIsSecondArcExcluded {
        skip_exclude_b_minus_a: CellRef,
    },
    /// Asserts that the arc (b, PRIME) was excluded.
    AssertLeAssertThirdArcExcluded,
    /// Samples a random point on the EC.
    RandomEcPoint {
        x: CellRef,
        y: CellRef,
    },
    /// Computes the square root of `val`, if `val` is a quadratic residue, and of `3 * val`
    /// otherwise.
    ///
    /// Since 3 is not a quadratic residue, exactly one of `val` and `3 * val` is a quadratic
    /// residue (unless `val` is 0). This allows proving that `val` is not a quadratic residue.
    FieldSqrt {
        val: ResOperand,
        sqrt: CellRef,
    },
    /// Prints the values from start to end.
    /// Both must be pointers.
    DebugPrint {
        start: ResOperand,
        end: ResOperand,
    },
    /// Returns an address with `size` free locations afterwards.
    AllocConstantSize {
        size: ResOperand,
        dst: CellRef,
    },
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

impl Display for CoreHint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CoreHint::AllocSegment { dst } => write!(f, "memory{dst} = segments.add()"),
            CoreHint::AllocFelt252Dict { segment_arena_ptr } => {
                let segment_arena_ptr = ResOperandFormatter(segment_arena_ptr);
                writedoc!(
                    f,
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
            // TODO(Gil): get the 3 from DictAccess or pass it as an argument.
            CoreHint::Felt252DictRead { dict_ptr, key, value_dst } => {
                let (dict_ptr, key) = (ResOperandFormatter(dict_ptr), ResOperandFormatter(key));
                writedoc!(
                    f,
                    "

                        dict_tracker = __dict_manager.get_tracker({dict_ptr})
                        dict_tracker.current_ptr += 3
                        memory{value_dst} = dict_tracker.data[{key}]
                    "
                )
            }
            CoreHint::Felt252DictWrite { dict_ptr, key, value } => {
                let (dict_ptr, key, value) = (
                    ResOperandFormatter(dict_ptr),
                    ResOperandFormatter(key),
                    ResOperandFormatter(value),
                );
                writedoc!(
                    f,
                    "

                    dict_tracker = __dict_manager.get_tracker({dict_ptr})
                    memory[{dict_ptr} + 1] = dict_tracker.data[{key}]
                    dict_tracker.current_ptr += 3
                    dict_tracker.data[{key}] = {value}
                    "
                )
            }
            CoreHint::Felt252DictEntryInit { dict_ptr, key } => {
                let (dict_ptr, key) = (ResOperandFormatter(dict_ptr), ResOperandFormatter(key));
                writedoc!(
                    f,
                    "

                    dict_tracker = __dict_manager.get_tracker({dict_ptr})
                    memory[{dict_ptr} + 1] = dict_tracker.data[{key}]
                    "
                )
            }
            CoreHint::Felt252DictEntryUpdate { dict_ptr, value } => {
                let (dict_ptr, value) = (ResOperandFormatter(dict_ptr), ResOperandFormatter(value));
                writedoc!(
                    f,
                    "

                    dict_tracker = __dict_manager.get_tracker({dict_ptr})
                    dict_tracker.data[memory[{dict_ptr} - 3]] = {value}
                    "
                )
            }
            CoreHint::TestLessThan { lhs, rhs, dst } => write!(
                f,
                "memory{dst} = {} < {}",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::TestLessThanOrEqual { lhs, rhs, dst } => write!(
                f,
                "memory{dst} = {} <= {}",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::DivMod { lhs, rhs, quotient, remainder } => write!(
                f,
                "(memory{quotient}, memory{remainder}) = divmod({}, {})",
                ResOperandFormatter(lhs),
                ResOperandFormatter(rhs)
            ),
            CoreHint::Uint256DivMod {
                dividend_low,
                dividend_high,
                divisor_low,
                divisor_high,
                quotient0,
                quotient1,
                divisor0,
                divisor1,
                extra0,
                extra1,
                remainder_low,
                remainder_high,
            } => {
                let (dividend_low, dividend_high, divisor_low, divisor_high) = (
                    ResOperandFormatter(dividend_low),
                    ResOperandFormatter(dividend_high),
                    ResOperandFormatter(divisor_low),
                    ResOperandFormatter(divisor_high),
                );
                writedoc!(
                    f,
                    "

                        dividend = {dividend_low} + {dividend_high} * 2**128
                        divisor = {divisor_low} + {divisor_high} * 2**128
                        quotient, remainder = divmod(dividend, divisor)
                        memory{quotient0} = quotient & 0xFFFFFFFFFFFFFFFF
                        memory{quotient1} = (quotient >> 64) & 0xFFFFFFFFFFFFFFFF
                        memory{divisor0} = divisor & 0xFFFFFFFFFFFFFFFF
                        memory{divisor1} = (divisor >> 64) & 0xFFFFFFFFFFFFFFFF
                        memory{remainder_low} = remainder & 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                        memory{remainder_high} = remainder >> 128
                        if {divisor_high} == 0:
                            memory{extra0} = (quotient >> 128) & 0xFFFFFFFFFFFFFFFF
                            memory{extra1} = quotient >> 192
                        else:
                            memory{extra0} = (divisor >> 128) & 0xFFFFFFFFFFFFFFFF
                            memory{extra1} = divisor >> 192
                    "
                )?;
                Ok(())
            }
            CoreHint::SquareRoot { value, dst } => {
                writedoc!(
                    f,
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
                writedoc!(
                    f,
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
                )?;
                Ok(())
            }
            CoreHint::LinearSplit { value, scalar, max_x, x, y } => {
                let (value, scalar, max_x) = (
                    ResOperandFormatter(value),
                    ResOperandFormatter(scalar),
                    ResOperandFormatter(max_x),
                );
                writedoc!(
                    f,
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
                writedoc!(
                    f,
                    "

                        from starkware.crypto.signature.signature import ALPHA, BETA, FIELD_PRIME
                        from starkware.python.math_utils import random_ec_point
                        (memory{x}, memory{y}) = random_ec_point(FIELD_PRIME, ALPHA, BETA)
                    "
                )
            }
            CoreHint::FieldSqrt { val, sqrt } => {
                writedoc!(
                    f,
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
            CoreHint::GetCurrentAccessIndex { range_check_ptr } => writedoc!(
                f,
                "

                    current_access_indices = sorted(access_indices[key])[::-1]
                    current_access_index = current_access_indices.pop()
                    memory[{}] = current_access_index
                ",
                ResOperandFormatter(range_check_ptr)
            ),
            CoreHint::ShouldSkipSquashLoop { should_skip_loop } => {
                write!(f, "memory{should_skip_loop} = 0 if current_access_indices else 1")
            }
            CoreHint::GetCurrentAccessDelta { index_delta_minus1 } => writedoc!(
                f,
                "

                    new_access_index = current_access_indices.pop()
                    memory{index_delta_minus1} = new_access_index - current_access_index - 1
                    current_access_index = new_access_index
                "
            ),
            CoreHint::ShouldContinueSquashLoop { should_continue } => {
                write!(f, "memory{should_continue} = 1 if current_access_indices else 0")
            }
            CoreHint::AssertCurrentAccessIndicesIsEmpty => {
                write!(f, "assert len(current_access_indices) == 0")
            }
            CoreHint::AssertAllAccessesUsed { n_used_accesses } => {
                write!(f, "assert memory{n_used_accesses} == len(access_indices[key])")
            }
            CoreHint::AssertAllKeysUsed => write!(f, "assert len(keys) == 0"),
            CoreHint::GetNextDictKey { next_key } => writedoc!(
                f,
                "
                    assert len(keys) > 0, 'No keys left but remaining_accesses > 0.'
                    memory{next_key} = key = keys.pop()
                "
            ),
            CoreHint::GetSegmentArenaIndex { dict_end_ptr, dict_index } => {
                let dict_end_ptr = ResOperandFormatter(dict_end_ptr);
                writedoc!(
                    f,
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
                writedoc!(
                    f,
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
            CoreHint::AssertLtAssertValidInput { a, b } => {
                let (a, b) = (ResOperandFormatter(a), ResOperandFormatter(b));
                writedoc!(
                    f,
                    "

                    from starkware.cairo.common.math_utils import assert_integer
                    assert_integer({a})
                    assert_integer({b})
                    assert ({a} % PRIME) < ({b} % PRIME), f'a = {{{a} % PRIME}} is not less than b \
                     = {{{b} % PRIME}}.'
                "
                )
            }
            CoreHint::AssertLeFindSmallArcs { range_check_ptr, a, b } => {
                let (range_check_ptr, a, b) = (
                    ResOperandFormatter(range_check_ptr),
                    ResOperandFormatter(a),
                    ResOperandFormatter(b),
                );
                writedoc!(
                    f,
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
                write!(f, "memory{skip_exclude_a_flag} = 1 if excluded != 0 else 0",)
            }
            CoreHint::AssertLeIsSecondArcExcluded { skip_exclude_b_minus_a } => {
                write!(f, "memory{skip_exclude_b_minus_a} = 1 if excluded != 1 else 0",)
            }
            CoreHint::AssertLeAssertThirdArcExcluded => write!(f, "assert excluded == 2"),
            CoreHint::DebugPrint { start, end } => writedoc!(
                f,
                "

                    start = {}
                    end = {}
                    for i in range(start, end):
                        print(memory[i])
                ",
                ResOperandFormatter(start),
                ResOperandFormatter(end),
            ),
            CoreHint::AllocConstantSize { size, dst } => {
                writedoc!(
                    f,
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

impl Display for StarknetHint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StarknetHint::SystemCall { system } => {
                write!(f, "syscall_handler.syscall(syscall_ptr={})", ResOperandFormatter(system))
            }
            StarknetHint::SetBlockNumber { value } => {
                write!(f, "syscall_handler.block_number = {}", ResOperandFormatter(value))
            }
            StarknetHint::SetBlockTimestamp { value } => {
                write!(f, "syscall_handler.block_timestamp = {}", ResOperandFormatter(value))
            }
            StarknetHint::SetCallerAddress { value } => {
                write!(f, "syscall_handler.caller_address = {}", ResOperandFormatter(value))
            }
            StarknetHint::SetContractAddress { value } => {
                write!(f, "syscall_handler.contract_address = {}", ResOperandFormatter(value))
            }
            StarknetHint::SetSequencerAddress { value } => {
                write!(f, "syscall_handler.sequencer_address = {}", ResOperandFormatter(value))
            }
        }
    }
}
