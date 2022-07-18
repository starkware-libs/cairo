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
    // Not an actual value - for objects that have no actual meaning in memory.
    Transient,
    // References a constant.
    Const(i64),
    // References a memory location.
    Final(MemLocation),
    // References a calculation over 2 memory locations.
    Op(MemLocation, Op, MemLocation),
    // References a calculation over a memory location and a constant.
    OpWithConst(MemLocation, Op, i64),
}

// Merges two adjacent memory locations - will fail if not actually ajacent.
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

#[test]
fn mem_reducer_test() {
    assert_eq!(
        mem_reducer((MemLocation::Temp(0), 1), (MemLocation::Temp(1), 1)),
        Some((MemLocation::Temp(0), 2))
    );
    assert_eq!(
        mem_reducer((MemLocation::Temp(-2), 3), (MemLocation::Temp(1), 6)),
        Some((MemLocation::Temp(-2), 9))
    );
    assert_eq!(
        mem_reducer((MemLocation::Temp(-2), 2), (MemLocation::Temp(1), 6)),
        None
    );
    assert_eq!(
        mem_reducer((MemLocation::Temp(-2), 4), (MemLocation::Temp(1), 6)),
        None
    );
    assert_eq!(
        mem_reducer((MemLocation::Local(0), 1), (MemLocation::Local(1), 1)),
        Some((MemLocation::Local(0), 2))
    );
    assert_eq!(
        mem_reducer((MemLocation::Local(-2), 3), (MemLocation::Local(1), 6)),
        Some((MemLocation::Local(-2), 9))
    );
    assert_eq!(
        mem_reducer((MemLocation::Local(-2), 2), (MemLocation::Local(1), 6)),
        None
    );
    assert_eq!(
        mem_reducer((MemLocation::Local(-2), 4), (MemLocation::Local(1), 6)),
        None
    );
    assert_eq!(
        mem_reducer((MemLocation::Local(0), 1), (MemLocation::Temp(1), 1)),
        None
    );
    assert_eq!(
        mem_reducer((MemLocation::Temp(-2), 3), (MemLocation::Local(1), 6)),
        None
    );
}
