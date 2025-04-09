use crate::tuple::{TuplePartialEqHelper, TuplePartialEq};
impl TuplePartialEqHelperBaseFixedSizedArray<T> of TuplePartialEqHelper<[@T; 0]> {
    fn eq(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        true
    }
    fn ne(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        false
    }
}

impl DefaultFixedSizedArray<T> of Default<[T; 0]> {
    fn default() -> [T; 0] {
        []
    }
}

impl FixedSizedArrayDrop<T, +Drop<T>, const N: u32> of Drop<[T; N]>;
impl FixedSizedArrayCopy<T, +Copy<T>, const N: u32> of Copy<[T; N]>;
