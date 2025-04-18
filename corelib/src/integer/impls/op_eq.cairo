pub impl AddEqImpl<T, +Add<T>> of crate::traits::AddEq<T> {
    fn add_eq(ref self: T, other: T) {
        self = Add::add(self, other);
    }
}

pub impl SubEqImpl<T, +Sub<T>> of crate::traits::SubEq<T> {
    fn sub_eq(ref self: T, other: T) {
        self = Sub::sub(self, other);
    }
}

pub impl MulEqImpl<T, +Mul<T>> of crate::traits::MulEq<T> {
    fn mul_eq(ref self: T, other: T) {
        self = Mul::mul(self, other);
    }
}

pub impl DivEqImpl<T, +Div<T>> of crate::traits::DivEq<T> {
    fn div_eq(ref self: T, other: T) {
        self = Div::div(self, other);
    }
}

pub impl RemEqImpl<T, +Rem<T>> of crate::traits::RemEq<T> {
    fn rem_eq(ref self: T, other: T) {
        self = Rem::rem(self, other);
    }
}
