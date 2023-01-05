use std::fmt::{Display, Formatter};

use indoc::writedoc;

use crate::operand::{CellRef, DerefOrImmediate, ResOperand};

#[cfg(test)]
mod test;

pub mod dict_squash;

// Represents a cairo hint.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Hint {
    AllocSegment {
        dst: CellRef,
    },
    // Allocates a new dict segment, and write its start address into the dict_infos segment.
    AllocDictFeltTo {
        dict_manager_ptr: ResOperand,
    },
    DictFeltToRead {
        dict_ptr: ResOperand,
        key: ResOperand,
        value_dst: CellRef,
    },
    DictFeltToWrite {
        dict_ptr: ResOperand,
        key: ResOperand,
        value: ResOperand,
        prev_value_dst: CellRef,
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
    /// Finds some `x` and `y` such that `x * scalar + y = value` and `x <= max_x`.
    LinearSplit {
        value: ResOperand,
        scalar: ResOperand,
        max_x: ResOperand,
        x: CellRef,
        y: CellRef,
    },
    EnterScope,
    ExitScope,
    DictDestruct {
        dict_manager_ptr: ResOperand,
        dict_end_ptr: ResOperand,
        dict_index: CellRef,
    },
    DictSquash1 {
        dict_end_ptr: ResOperand,
    },
    DictSquash2 {
        squashed_dict_start: ResOperand,
        squashed_dict_end: ResOperand,
    },
    SquashDict {
        dict_accesses: ResOperand,
        ptr_diff: ResOperand,
        n_accesses: ResOperand,
        big_keys: CellRef,
        first_key: CellRef,
    },
    SquashDictInner1 {
        range_check_ptr: ResOperand,
    },
    SquashDictInner2 {
        should_skip_loop: CellRef,
    },
    SquashDictInner3 {
        index_delta_minus1: CellRef,
    },
    SquashDictInner4 {
        should_continue: CellRef,
    },
    SquashDictInner5,
    SquashDictInner6 {
        n_used_accesses: CellRef,
    },
    SquashDictInner7,
    SquashDictInner8 {
        next_key: CellRef,
    },
    AssertLtFelt {
        a: ResOperand,
        b: ResOperand,
    },
    AssertLeFelt1 {
        range_check_ptr: ResOperand,
        a: ResOperand,
        b: ResOperand,
    },
    AssertLeFelt2 {
        skip_exclude_a_flag: ResOperand,
    },
    AssertLeFelt3 {
        skip_exclude_b_minus_a: ResOperand,
    },
    AssertLeFelt4,
    /// Samples a random point on the EC.
    RandomEcPoint {
        x: CellRef,
        y: CellRef,
    },
    /// Represents a hint that triggers a system call.
    SystemCall {
        system: ResOperand,
    },
    /// Prints the values from start to end.
    /// Both must be pointers.
    DebugPrint {
        start: ResOperand,
        end: ResOperand,
    },
}

impl Display for Hint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fmt_access_or_const = |f: &mut Formatter<'_>, v: &DerefOrImmediate| match v {
            DerefOrImmediate::Deref(d) => write!(f, "memory{d}"),
            DerefOrImmediate::Immediate(i) => write!(f, "{i}"),
        };
        let fmt_res_operand = |f: &mut Formatter<'_>, v: &ResOperand| match v {
            ResOperand::Deref(d) => write!(f, "memory{d}"),
            ResOperand::DoubleDeref(d, i) => write!(f, "memory[memory{d} + {i}]"),
            ResOperand::Immediate(i) => write!(f, "{i}"),
            ResOperand::BinOp(bin_op) => {
                write!(f, "memory{} {} ", bin_op.a, bin_op.op)?;
                fmt_access_or_const(f, &bin_op.b)
            }
        };

        match self {
            Hint::AllocSegment { dst } => write!(f, "memory{dst} = segments.add()")?,
            Hint::AllocDictFeltTo { dict_manager_ptr } => {
                writedoc!(
                    f,
                    "

                        if '__dict_manager' not in globals():
                            from starkware.cairo.common.dict import DictManager
                            __dict_manager = DictManager()
                        # memory[dict_manager_ptr] is the address of the current dict manager
                        n_dicts = memory[memory{dict_manager_ptr} + 1]
                        # memory[memory[dict_manager_ptr] + 0] is the address of the dict infos \
                     segment
                        # n_dicts * 3 is added to get the address of the new DictInfo
                        memory[memory[memory{dict_manager_ptr} + 0] + n_dicts * 3] = (
                            __dict_manager.new_default_dict(segments, 0, temp_segment=n_dicts > 0)
                        )
                "
                )?;
            }
            // TODO(Gil): get the 3 from DictAccess or pass it as an argument.
            Hint::DictFeltToRead { dict_ptr, key, value_dst } => {
                writedoc!(
                    f,
                    "

                        dict_tracker = __dict_manager.get_tracker(memory{dict_ptr})
                        dict_tracker.current_ptr += 3
                        memory{value_dst} = dict_tracker.data[memory{key}]
                    "
                )?;
            }
            Hint::DictFeltToWrite { dict_ptr, key, value, prev_value_dst } => {
                writedoc!(
                    f,
                    "

                        dict_tracker = __dict_manager.get_tracker(memory{dict_ptr})
                        dict_tracker.current_ptr += 3
                        memory{prev_value_dst} = dict_tracker.data[memory{key}]
                        dict_tracker.data[memory{key}] = memory{value}
                    "
                )?;
            }
            Hint::TestLessThan { lhs, rhs, dst } => {
                write!(f, "memory{dst} = ")?;
                fmt_res_operand(f, lhs)?;
                write!(f, " < ")?;
                fmt_res_operand(f, rhs)?;
            }
            Hint::TestLessThanOrEqual { lhs, rhs, dst } => {
                write!(f, "memory{dst} = ")?;
                fmt_res_operand(f, lhs)?;
                write!(f, " <= ")?;
                fmt_res_operand(f, rhs)?
            }
            Hint::DivMod { lhs, rhs, quotient, remainder } => {
                write!(f, "(memory{quotient}, memory{remainder}) = divmod(")?;
                fmt_res_operand(f, lhs)?;
                write!(f, ", ")?;
                fmt_res_operand(f, rhs)?;
                write!(f, ")")?;
            }
            Hint::LinearSplit { value, scalar, max_x, x, y } => {
                writeln!(f)?;
                write!(f, "value = ")?;
                fmt_res_operand(f, value)?;
                writeln!(f)?;
                write!(f, "scalar = ")?;
                fmt_res_operand(f, scalar)?;
                writeln!(f)?;
                write!(f, "max_x = ")?;
                fmt_res_operand(f, max_x)?;
                writeln!(f)?;
                writedoc!(
                    f,
                    "
                        x = min(value // scalar, max_x)
                        y = value - x * scalar
                        memory{x} = x
                        memory{y} = y
                    "
                )?;
            }
            Hint::EnterScope => write!(f, "vm_enter_scope()")?,
            Hint::ExitScope => write!(f, "vm_exit_scope()")?,
            Hint::RandomEcPoint { x, y } => {
                writedoc!(
                    f,
                    "

                        def try_sample_point() -> Tuple[int, int]:
                            x = random.randrange(PRIME)
                            y2 = x**3 + ALPHA * x + BETA
                            return x, sympy.ntheory.residue_ntheory.sqrt_mod(
                                y2, PRIME, all_roots=True
                            )
                        x, y = try_sample_point()
                        while y is None:
                            x, y = try_sample_point()
                        (memory{x}, memory{y}) = x, y
                    "
                )?;
            }
            Hint::SystemCall { system } => {
                write!(f, "syscall_handler.syscall(syscall_ptr=",)?;
                fmt_res_operand(f, system)?;
                write!(f, ")")?;
            }
            Hint::SquashDictInner1 { range_check_ptr } => writedoc!(
                f,
                "

                    current_access_indices = sorted(access_indices[key])[::-1]
                    current_access_index = current_access_indices.pop()
                    memory{range_check_ptr} = current_access_index
                "
            )?,
            Hint::SquashDictInner2 { should_skip_loop } => {
                write!(f, " memory{should_skip_loop} = 0 if current_access_indices else 1 ")?
            }
            Hint::SquashDictInner3 { index_delta_minus1 } => writedoc!(
                f,
                "

                    new_access_index = current_access_indices.pop()
                    memory{index_delta_minus1} = new_access_index - current_access_index - 1
                    current_access_index = new_access_index
                "
            )?,
            Hint::SquashDictInner4 { should_continue } => {
                write!(f, " memory{should_continue} = 1 if current_access_indices else 0 ")?
            }
            Hint::SquashDictInner5 => write!(f, " assert len(current_access_indices) == 0 ")?,
            Hint::SquashDictInner6 { n_used_accesses } => {
                write!(f, " assert memory{n_used_accesses} == len(access_indices[key]) ")?
            }
            Hint::SquashDictInner7 => write!(f, " assert len(keys) == 0 ")?,
            Hint::SquashDictInner8 { next_key } => writedoc!(
                f,
                "
                    assert len(keys) > 0, 'No keys left but remaining_accesses > 0.'
                    memory{next_key} = key = keys.pop()
                "
            )?,
            Hint::DictDestruct { dict_manager_ptr, dict_end_ptr, dict_index } => writedoc!(
                f,
                "

                    expected_segment_index = memory{dict_end_ptr}.segment_index
                    for i in range(memory[memory{dict_manager_ptr}]):
                        if memory[memory{dict_manager_ptr} + 1].segment_index == \
                 expected_segment_index:
                            memory{dict_index} = i
                            break
                    else:
                        raise Exception(f\"Dict with end pointer was not found.\")
                "
            )?,
            Hint::DictSquash1 { dict_end_ptr } => writedoc!(
                f,
                "

                    # Prepare arguments for dict_new. In particular, the same dictionary values \
                 should be copied
                    # to the new (squashed) dictionary.
                    vm_enter_scope({{
                        # Make __dict_manager accessible.
                        '__dict_manager': __dict_manager,
                        # Create a copy of the dict, in case it changes in the future.
                        'initial_dict': dict(__dict_manager.get_dict(memory{dict_end_ptr})),
                    }})
                "
            )?,
            Hint::DictSquash2 { squashed_dict_start, squashed_dict_end } => writedoc!(
                f,
                "
                    # Update the DictTracker's current_ptr to point to the end of the squashed \
                 dict.
                    __dict_manager.get_tracker(memory{squashed_dict_start}).current_ptr = \
                 memory{squashed_dict_end}.address_
                "
            )?,
            Hint::SquashDict { dict_accesses, ptr_diff, n_accesses, big_keys, first_key } => {
                writedoc!(
                    f,
                    "
                    dict_access_size = 3
                    address = memory{dict_accesses}.address_
                    assert memory{ptr_diff} % dict_access_size == 0, 'Accesses array size must be \
                     divisible by DictAccess.SIZE'
                    n_accesses = memory{n_accesses}
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
                )?
            }
            Hint::AssertLtFelt { a, b } => writedoc!(
                f,
                "
                    from starkware.cairo.common.math_utils import assert_integer
                    assert_integer(memory{a})
                    assert_integer(memory{b})
                    assert (memory{a} % PRIME) < (memory{b} % PRIME), f'a = {{memory{a} % PRIME}} \
                 is not less than b = {{memory{b} % PRIME}}.'
                "
            )?,
            Hint::AssertLeFelt1 { range_check_ptr, a, b } => writedoc!(
                f,
                "
                    import itertools

                    from starkware.cairo.common.math_utils import assert_integer
                    assert_integer(memory{a})
                    assert_integer(memory{b})
                    a = memory{a} % PRIME
                    b = memory{b} % PRIME
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
            )?,
            Hint::AssertLeFelt2 { skip_exclude_a_flag } => {
                write!(f, "memory{skip_exclude_a_flag} = 1 if excluded != 0 else 0")?
            }
            Hint::AssertLeFelt3 { skip_exclude_b_minus_a } => {
                write!(f, "memory{skip_exclude_b_minus_a} = 1 if excluded != 1 else 0")?
            }
            Hint::AssertLeFelt4 => write!(f, "assert excluded == 2")?,
            Hint::DebugPrint { start, end } => {
                writeln!(f)?;
                write!(f, "let start = ")?;
                fmt_res_operand(f, start)?;
                writeln!(f)?;
                write!(f, "let end = ")?;
                fmt_res_operand(f, end)?;
                writedoc!(
                    f,
                    "

                        for i in range(start, end):
                            print(memory[i])
                    "
                )?;
            }
        }
        Ok(())
    }
}
