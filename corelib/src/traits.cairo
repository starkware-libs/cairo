use core::panics::Panic;

pub trait Copy<T>;
pub trait Drop<T>;

impl SnapshotCopy<T> of Copy<@T>;
impl SnapshotDrop<T> of Drop<@T>;

// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
pub trait Add<T> {
    fn add(lhs: T, rhs: T) -> T;
}
pub trait AddEq<T> {
    fn add_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
pub trait Sub<T> {
    fn sub(lhs: T, rhs: T) -> T;
}
pub trait SubEq<T> {
    fn sub_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
pub trait Mul<T> {
    fn mul(lhs: T, rhs: T) -> T;
}
pub trait MulEq<T> {
    fn mul_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
pub trait Div<T> {
    fn div(lhs: T, rhs: T) -> T;
}
pub trait DivEq<T> {
    fn div_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Rem<X, Y>.
pub trait Rem<T> {
    fn rem(lhs: T, rhs: T) -> T;
}
pub trait RemEq<T> {
    fn rem_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait DivRem<X, Y>.
/// Division with remainder.
pub trait DivRem<T> {
    fn div_rem(lhs: T, rhs: NonZero<T>) -> (T, T);
}

pub trait PartialEq<T> {
    fn eq(lhs: @T, rhs: @T) -> bool;
    fn ne(lhs: @T, rhs: @T) -> bool;
}
impl PartialEqSnap<T, +PartialEq<T>> of PartialEq<@T> {
    fn eq(lhs: @@T, rhs: @@T) -> bool {
        PartialEq::<T>::eq(*lhs, *rhs)
    }
    fn ne(lhs: @@T, rhs: @@T) -> bool {
        PartialEq::<T>::ne(*lhs, *rhs)
    }
}

// TODO(spapini): When associated types are supported, support the general trait BitAnd<X, Y>.
pub trait BitAnd<T> {
    fn bitand(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitOr<X, Y>.
pub trait BitOr<T> {
    fn bitor(lhs: T, rhs: T) -> T;
}

// TODO(spapini): When associated types are supported, support the general trait BitXor<X, Y>.
pub trait BitXor<T> {
    fn bitxor(lhs: T, rhs: T) -> T;
}

pub trait BitNot<T> {
    fn bitnot(a: T) -> T;
}

pub trait PartialOrd<T> {
    fn le(lhs: T, rhs: T) -> bool;
    fn ge(lhs: T, rhs: T) -> bool;
    fn lt(lhs: T, rhs: T) -> bool;
    fn gt(lhs: T, rhs: T) -> bool;
}

/// Trait for conversion between types.
pub trait Into<T, S> {
    #[must_use]
    fn into(self: T) -> S;
}

impl TIntoT<T> of Into<T, T> {
    fn into(self: T) -> T {
        self
    }
}

/// Trait for fallible conversion between types.
pub trait TryInto<T, S> {
    fn try_into(self: T) -> Option<S>;
}

impl TryIntoFromInto<From, To, +Into<From, To>> of TryInto<From, To> {
    fn try_into(self: From) -> Option<To> {
        Option::Some(self.into())
    }
}

pub trait Neg<T> {
    fn neg(a: T) -> T;
}

pub trait Not<T> {
    fn not(a: T) -> T;
}

/// The following two traits are for implementing the [] operator. Only one should be implemented
/// for each type. Both are not consuming of self, the first gets a snapshot of the object and
/// the second gets ref.
pub trait IndexView<C, I, V> {
    fn index(self: @C, index: I) -> V;
}

pub trait Index<C, I, V> {
    fn index(ref self: C, index: I) -> V;
}

pub trait Destruct<T> {
    fn destruct(self: T) nopanic;
}
// TODO(spapini): Remove this, it can lead to multiple impls and unwanted Destruct implementation.
impl DestructFromDrop<T, +Drop<T>> of Destruct<T> {
    #[inline(always)]
    fn destruct(self: T) nopanic {}
}

pub trait PanicDestruct<T> {
    fn panic_destruct(self: T, ref panic: Panic) nopanic;
}
impl PanicDestructForDestruct<T, +Destruct<T>> of PanicDestruct<T> {
    #[inline(always)]
    fn panic_destruct(self: T, ref panic: Panic) nopanic {
        Destruct::destruct(self);
    }
}

pub trait Default<T> {
    #[must_use]
    fn default() -> T;
}

impl SnapshotDefault<T, +Default<T>, +Drop<T>> of Default<@T> {
    #[inline(always)]
    fn default() -> @T {
        @Default::default()
    }
}

/// Trait for types allowed as values in a Felt252Dict.
pub trait Felt252DictValue<T> {
    /// Returns the default value for this type as a value in a Felt252Dict.
    /// Should be logically equivalent to 0.
    #[must_use]
    fn zero_default() -> T nopanic;
}

// Tuple Copy impls.
pub(crate) impl TupleSize0Copy of Copy<()>;

impl TupleSize1Copy<E0, +Copy<E0>> of Copy<(E0,)>;

impl TupleSize2Copy<E0, E1, +Copy<E0>, +Copy<E1>> of Copy<(E0, E1)>;

impl TupleSize3Copy<E0, E1, E2, +Copy<E0>, +Copy<E1>, +Copy<E2>> of Copy<(E0, E1, E2)>;

impl TupleSize4Copy<
    E0, E1, E2, E3, +Copy<E0>, +Copy<E1>, +Copy<E2>, +Copy<E3>
> of Copy<(E0, E1, E2, E3)>;

// Tuple Drop impls.
pub(crate) impl TupleSize0Drop of Drop<()>;

impl TupleSize1Drop<E0, +Drop<E0>> of Drop<(E0,)>;

impl TupleSize2Drop<E0, E1, +Drop<E0>, +Drop<E1>> of Drop<(E0, E1)>;

impl TupleSize3Drop<E0, E1, E2, +Drop<E0>, +Drop<E1>, +Drop<E2>> of Drop<(E0, E1, E2)>;

impl TupleSize4Drop<
    E0, E1, E2, E3, +Drop<E0>, +Drop<E1>, +Drop<E2>, +Drop<E3>
> of Drop<(E0, E1, E2, E3)>;

// Tuple PartialEq impls.
impl TupleSize0PartialEq of PartialEq<()> {
    #[inline(always)]
    fn eq(lhs: @(), rhs: @()) -> bool {
        true
    }
    #[inline(always)]
    fn ne(lhs: @(), rhs: @()) -> bool {
        false
    }
}

impl TupleSize1PartialEq<E0, +PartialEq<E0>> of PartialEq<(E0,)> {
    #[inline(always)]
    fn eq(lhs: @(E0,), rhs: @(E0,)) -> bool {
        let (lhs,) = lhs;
        let (rhs,) = rhs;
        lhs == rhs
    }
    #[inline(always)]
    fn ne(lhs: @(E0,), rhs: @(E0,)) -> bool {
        !(rhs == lhs)
    }
}

impl TupleSize2PartialEq<E0, E1, +PartialEq<E0>, +PartialEq<E1>> of PartialEq<(E0, E1)> {
    #[inline(always)]
    fn eq(lhs: @(E0, E1), rhs: @(E0, E1)) -> bool {
        let (lhs0, lhs1) = lhs;
        let (rhs0, rhs1) = rhs;
        lhs0 == rhs0 && lhs1 == rhs1
    }
    #[inline(always)]
    fn ne(lhs: @(E0, E1), rhs: @(E0, E1)) -> bool {
        !(rhs == lhs)
    }
}

impl TupleSize3PartialEq<
    E0, E1, E2, +PartialEq<E0>, +PartialEq<E1>, +PartialEq<E2>
> of PartialEq<(E0, E1, E2)> {
    #[inline(always)]
    fn eq(lhs: @(E0, E1, E2), rhs: @(E0, E1, E2)) -> bool {
        let (lhs0, lhs1, lhs2) = lhs;
        let (rhs0, rhs1, rhs2) = rhs;
        lhs0 == rhs0 && lhs1 == rhs1 && lhs2 == rhs2
    }
    #[inline(always)]
    fn ne(lhs: @(E0, E1, E2), rhs: @(E0, E1, E2)) -> bool {
        !(rhs == lhs)
    }
}

impl TupleSize4PartialEq<
    E0, E1, E2, E3, +PartialEq<E0>, +PartialEq<E1>, +PartialEq<E2>, +PartialEq<E3>
> of PartialEq<(E0, E1, E2, E3)> {
    #[inline(always)]
    fn eq(lhs: @(E0, E1, E2, E3), rhs: @(E0, E1, E2, E3)) -> bool {
        let (lhs0, lhs1, lhs2, lhs3) = lhs;
        let (rhs0, rhs1, rhs2, rhs3) = rhs;
        lhs0 == rhs0 && lhs1 == rhs1 && lhs2 == rhs2 && lhs3 == rhs3
    }
    #[inline(always)]
    fn ne(lhs: @(E0, E1, E2, E3), rhs: @(E0, E1, E2, E3)) -> bool {
        !(rhs == lhs)
    }
}

// Tuple Default impls.
impl TupleSize0Default of Default<()> {
    fn default() -> () {
        ()
    }
}

impl TupleSize1Default<E0, +Default<E0>> of Default<(E0,)> {
    fn default() -> (E0,) {
        (Default::default(),)
    }
}

impl TupleSize2Default<E0, E1, +Default<E0>, +Drop<E0>, +Default<E1>> of Default<(E0, E1)> {
    fn default() -> (E0, E1) {
        (Default::default(), Default::default())
    }
}

impl TupleSize3Default<
    E0, E1, E2, +Default<E0>, +Drop<E0>, +Default<E1>, +Drop<E1>, +Default<E2>
> of Default<(E0, E1, E2)> {
    fn default() -> (E0, E1, E2) {
        (Default::default(), Default::default(), Default::default())
    }
}

impl TupleSize4Default<
    E0,
    E1,
    E2,
    E3,
    +Default<E0>,
    +Drop<E0>,
    +Default<E1>,
    +Drop<E1>,
    +Default<E2>,
    +Drop<E2>,
    +Default<E3>
> of Default<(E0, E1, E2, E3)> {
    fn default() -> (E0, E1, E2, E3) {
        (Default::default(), Default::default(), Default::default(), Default::default())
    }
}
