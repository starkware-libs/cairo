trait Copy<T>;
trait Drop<T>;

impl SnapshotCopy<T> of Copy<@T>;
impl SnapshotDrop<T> of Drop<@T>;

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

/// Trait for conversion between types.
trait Into<T, S> {
    fn into(self: T) -> S;
}

/// Trait for fallible conversion between types.
trait TryInto<T, S> {
    fn try_into(self: T) -> Option<S>;
}

trait Neg<T> {
    fn neg(a: T) -> T;
}

trait Not<T> {
    fn not(a: T) -> T;
}

/// The following two traits are for implementing the [] operator. Only one should be implemented
/// for each type. Both are not consuming of self, the first gets a snapshot of the object and 
/// the second gets ref.
trait IndexView<C, I, V> {
    fn index(self: @C, index: I) -> V;
}

trait Index<C, I, V> {
    fn index(ref self: C, index: I) -> V;
}

trait Destruct<T> {
    fn destruct(self: T) nopanic;
}

// TODO(spapini): Remove this, it can lead to multiple impls and unwanted Destruct implementation.
impl DestructFromDrop<T, impl TDrop: Drop<T>> of Destruct<T> {
    #[inline(always)]
    fn destruct(self: T) nopanic {}
}

trait Default<T> {
    fn default() -> T;
}
