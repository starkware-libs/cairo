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
