trait Copy<T>;
trait Drop<T>;

// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
trait Add<T> {
    fn add(a: T, b: T) -> T;
}
trait AddEq<T> {
    fn add_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
trait Sub<T> {
    fn sub(a: T, b: T) -> T;
}
trait SubEq<T> {
    fn sub_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
trait Mul<T> {
    fn mul(a: T, b: T) -> T;
}
trait MulEq<T> {
    fn mul_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
trait Div<T> {
    fn div(a: T, b: T) -> T;
}
trait DivEq<T> {
    fn div_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Rem<X, Y>.
trait Rem<T> {
    fn rem(a: T, b: T) -> T;
}
trait RemEq<T> {
    fn rem_eq(ref self: T, other: T);
}

trait PartialEq<T> {
    fn eq(a: T, b: T) -> bool;
    fn ne(a: T, b: T) -> bool;
}

// TODO(spapini): When associated types are supported, support the general trait BitAnd<X, Y>.
trait BitAnd<T> {
    fn bitand(a: T, b: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitOr<X, Y>.
trait BitOr<T> {
    fn bitor(a: T, b: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitXor<X, Y>.
trait BitXor<T> {
    fn bitxor(a: T, b: T) -> T;
}

trait PartialOrd<T> {
    fn le(a: T, b: T) -> bool;
    fn ge(a: T, b: T) -> bool;
    fn lt(a: T, b: T) -> bool;
    fn gt(a: T, b: T) -> bool;
}

/// Trait for convertion between types.
trait Into<T, S> {
    fn into(self: T) -> S;
}

/// Trait for fallible convertion between types.
trait TryInto<T, S> {
    fn try_into(self: T) -> Option::<S>;
}

trait Neg<T> {
    fn neg(a: T) -> T;
}

trait Not<T> {
    fn not(a: T) -> T;
}
