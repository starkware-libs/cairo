use std::fmt::{Display, Formatter};

use indoc::writedoc;

use crate::operand::{CellRef, DerefOrImmediate};

#[cfg(test)]
#[path = "hints_test.rs"]
mod test;

// Represents a cairo hint.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Hint {
    AllocSegment {
        dst: CellRef,
    },
    AllocDictFeltTo {
        dst: CellRef,
        default_value: CellRef,
    },
    DictFeltToRead {
        dict_ptr: CellRef,
        dict_offset: u16,
        key: CellRef,
        value_dst: CellRef,
    },
    DictFeltToWrite {
        dict_ptr: CellRef,
        dict_offset: u16,
        key: CellRef,
        value: CellRef,
        prev_value_dst: CellRef,
    },
    TestLessThan {
        lhs: DerefOrImmediate,
        rhs: DerefOrImmediate,
        dst: CellRef,
    },
    DivMod {
        lhs: DerefOrImmediate,
        rhs: DerefOrImmediate,
        quotient: CellRef,
        remainder: CellRef,
    },
}

impl Display for Hint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let fmt_access_or_const = |f: &mut Formatter<'_>, v: &DerefOrImmediate| match v {
            DerefOrImmediate::Deref(d) => write!(f, "memory{d}"),
            DerefOrImmediate::Immediate(i) => write!(f, "{i}"),
        };
        write!(f, "%{{ ")?;
        match self {
            Hint::AllocSegment { dst } => write!(f, "memory{dst} = segments.add()")?,
            Hint::AllocDictFeltTo { dst, default_value } => writedoc!(
                f,
                "

                    if '__dict_manager' not in globals():
                        from starkware.cairo.common.dict import DictManager
                        __dict_manager = DictManager()
                    memory{dst} = __dict_manager.new_default_dict(segments, memory{default_value})
                "
            )?,
            // TODO(Gil): get the 3 from DictAccess or pass it as an argument.
            Hint::DictFeltToRead { dict_ptr, dict_offset, key, value_dst } => {
                writedoc!(
                    f,
                    "

                        dict_tracker = __dict_manager.get_tracker(memory{dict_ptr} + {dict_offset})
                        dict_tracker.current_ptr += 3
                        memory{value_dst} = dict_tracker.data[memory{key}]
                    "
                )?;
            }
            Hint::DictFeltToWrite { dict_ptr, dict_offset, key, value, prev_value_dst } => {
                writedoc!(
                    f,
                    "

                    dict_tracker = __dict_manager.get_tracker(memory{dict_ptr} + {dict_offset})
                    dict_tracker.current_ptr += 3
                    memory{prev_value_dst} = dict_tracker.data[memory{key}]
                    dict_tracker.data[memory{key}] = memory{value}
                "
                )?
            }
            Hint::TestLessThan { lhs, rhs, dst } => {
                write!(f, "memory{dst} = ")?;
                fmt_access_or_const(f, lhs)?;
                write!(f, " < ")?;
                fmt_access_or_const(f, rhs)?;
            }
            Hint::DivMod { lhs, rhs, quotient, remainder } => {
                write!(f, "(memory{quotient}, memory{remainder}) = divmod(")?;
                fmt_access_or_const(f, lhs)?;
                write!(f, ", ")?;
                fmt_access_or_const(f, rhs)?;
                write!(f, ")")?;
            }
        }
        write!(f, " %}}")
    }
}
