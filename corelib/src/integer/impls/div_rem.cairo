pub impl DivImpl<T, +DivRem<T>, +TryInto<T, NonZero<T>>, +Drop<T>> of Div<T> {
    fn div(lhs: T, rhs: T) -> T {
        let (q, _r) = DivRem::div_rem(lhs, rhs.try_into().expect('Division by 0'));
        q
    }
}

pub impl RemImpl<T, +DivRem<T>, +TryInto<T, NonZero<T>>, +Drop<T>> of Rem<T> {
    fn rem(lhs: T, rhs: T) -> T {
        let (_q, r) = DivRem::div_rem(lhs, rhs.try_into().expect('Division by 0'));
        r
    }
}
