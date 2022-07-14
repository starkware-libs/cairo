#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MemLocation {
    Temp(i64),
    Local(i64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RefValue {
    Transient,
    Const(i64),
    Final(MemLocation),
    Op(MemLocation, Op, MemLocation),
    OpWithConst(MemLocation, Op, i64),
}

pub(crate) fn mem_reducer(
    (loc1, size1): (MemLocation, usize),
    (loc2, size2): (MemLocation, usize),
) -> Option<(MemLocation, usize)> {
    let contiguous = |start1, start2| start1 + size1 as i64 == start2;
    match loc1 {
        MemLocation::Temp(start1) => match loc2 {
            MemLocation::Temp(start2) if contiguous(start1, start2) => {}
            _ => {
                return None;
            }
        },
        MemLocation::Local(start1) => match loc2 {
            MemLocation::Local(start2) if contiguous(start1, start2) => {}
            _ => {
                return None;
            }
        },
    }
    Some((loc1, size1 + size2))
}
