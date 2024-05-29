/// The addition assignment operator `+=`.
pub trait AddAssign<Lhs, Rhs> {
    /// Performs the `+=` operation.
    fn add_assign(ref self: Lhs, rhs: Rhs);
}

/// The subtraction assignment operator `-=`.
pub trait SubAssign<Lhs, Rhs> {
    /// Performs the `-=` operation.
    fn sub_assign(ref self: Lhs, rhs: Rhs);
}

/// The multiplication assignment operator `*=`.
pub trait MulAssign<Lhs, Rhs> {
    /// Performs the `*=` operation.
    fn mul_assign(ref self: Lhs, rhs: Rhs);
}

/// The division assignment operator `/=`.
pub trait DivAssign<Lhs, Rhs> {
    /// Performs the `/=` operation.
    fn div_assign(ref self: Lhs, rhs: Rhs);
}

/// The remainder assignment operator `%=`.
pub trait RemAssign<Lhs, Rhs> {
    /// Performs the `%=` operation.
    fn rem_assign(ref self: Lhs, rhs: Rhs);
}

#[feature("deprecated-op-assign-traits")]
use core::traits::{AddEq, SubEq, MulEq, DivEq, RemEq};

impl DeprecatedAddAssign<T, impl Deprecated: AddEq<T>> of AddAssign<T, T> {
    fn add_assign(ref self: T, rhs: T) {
        Deprecated::add_eq(ref self, rhs)
    }
}

impl DeprecatedSubAssign<T, impl Deprecated: SubEq<T>> of SubAssign<T, T> {
    fn sub_assign(ref self: T, rhs: T) {
        Deprecated::sub_eq(ref self, rhs)
    }
}

impl DeprecatedMulAssign<T, impl Deprecated: MulEq<T>> of MulAssign<T, T> {
    fn mul_assign(ref self: T, rhs: T) {
        Deprecated::mul_eq(ref self, rhs)
    }
}

impl DeprecatedDivAssign<T, impl Deprecated: DivEq<T>> of DivAssign<T, T> {
    fn div_assign(ref self: T, rhs: T) {
        Deprecated::div_eq(ref self, rhs)
    }
}

impl DeprecatedRemAssign<T, impl Deprecated: RemEq<T>> of RemAssign<T, T> {
    fn rem_assign(ref self: T, rhs: T) {
        Deprecated::rem_eq(ref self, rhs)
    }
}
