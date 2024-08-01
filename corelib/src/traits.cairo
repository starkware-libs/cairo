use core::panics::Panic;

pub trait Copy<T>;
pub trait Drop<T>;

impl SnapshotCopy<T> of Copy<@T>;
impl SnapshotDrop<T> of Drop<@T>;

// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
pub trait Add<T> {
    fn add(lhs: T, rhs: T) -> T;
}
#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::AddAssign`.", since: "2.7.0"
)]
pub trait AddEq<T> {
    fn add_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
pub trait Sub<T> {
    fn sub(lhs: T, rhs: T) -> T;
}
#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::SubAssign`.", since: "2.7.0"
)]
pub trait SubEq<T> {
    fn sub_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
pub trait Mul<T> {
    fn mul(lhs: T, rhs: T) -> T;
}
#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::MulAssign`.", since: "2.7.0"
)]
pub trait MulEq<T> {
    fn mul_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
pub trait Div<T> {
    fn div(lhs: T, rhs: T) -> T;
}
#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::DivAssign`.", since: "2.7.0"
)]
pub trait DivEq<T> {
    fn div_eq(ref self: T, other: T);
}

// TODO(spapini): When associated types are supported, support the general trait Rem<X, Y>.
pub trait Rem<T> {
    fn rem(lhs: T, rhs: T) -> T;
}
#[deprecated(
    feature: "deprecated-op-assign-traits", note: "Use `core::ops::RemAssign`.", since: "2.7.0"
)]
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
    fn ne(lhs: @T, rhs: @T) -> bool {
        !Self::eq(lhs, rhs)
    }
}
impl PartialEqSnap<T, +PartialEq<T>> of PartialEq<@T> {
    fn eq(lhs: @@T, rhs: @@T) -> bool {
        PartialEq::<T>::eq(*lhs, *rhs)
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

impl PartialOrdSnap<T, +PartialOrd<T>, +Copy<T>> of PartialOrd<@T> {
    fn le(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::le(*lhs, *rhs)
    }
    fn ge(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::ge(*lhs, *rhs)
    }
    fn lt(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::lt(*lhs, *rhs)
    }
    fn gt(lhs: @T, rhs: @T) -> bool {
        PartialOrd::<T>::gt(*lhs, *rhs)
    }
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
#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::IndexView`.", since: "2.7.0"
)]
pub trait IndexView<C, I, V> {
    fn index(self: @C, index: I) -> V;
}

#[deprecated(
    feature: "deprecated-index-traits", note: "Use `core::ops::index::Index`.", since: "2.7.0"
)]
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
pub(crate) impl PanicDestructForDestruct<T, +Destruct<T>> of PanicDestruct<T> {
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

// Tuple Copy and Drop impls.
pub(crate) impl TupleSize0Copy of Copy<()>;
pub(crate) impl TupleSize0Drop of Drop<()>;
impl TupleNextDrop<
    T,
    impl TH: core::metaprogramming::TupleSplit<T>,
    +core::metaprogramming::IsTuple<T>,
    +Drop<TH::Head>,
    +Drop<TH::Rest>
> of Drop<T>;
impl TupleNextCopy<
    T,
    impl TH: core::metaprogramming::TupleSplit<T>,
    +core::metaprogramming::IsTuple<T>,
    +Copy<TH::Head>,
    +Copy<TH::Rest>
> of Copy<T>;

/// Tuple `PartialEq` implementation.
impl TuplePartialEq<
    T,
    impl TSF: core::metaprogramming::TupleSnapForward<T>,
    +TuplePartialEqHelper<TSF::SnapForward>,
> of PartialEq<T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::eq(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        TuplePartialEqHelper::ne(TSF::snap_forward(lhs), TSF::snap_forward(rhs))
    }
}

/// Trait helper for implementing `PartialEq` for tuples.
/// Provides an `eq` and `ne` function for comparing tuples of snapshots, and basic snapshots.
trait TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool;
    fn ne(lhs: T, rhs: T) -> bool;
}

/// An implementation of `TuplePartialEqHelper` for a snapshot of any type with `PartialEq`
/// implementation.
impl TuplePartialEqHelperByPartialEq<T, +PartialEq<T>> of TuplePartialEqHelper<@T> {
    fn eq(lhs: @T, rhs: @T) -> bool {
        lhs == rhs
    }
    fn ne(lhs: @T, rhs: @T) -> bool {
        lhs != rhs
    }
}

/// Base implementation of `TuplePartialEqHelper` for tuples.
impl TuplePartialEqHelperBaseTuple of TuplePartialEqHelper<()> {
    fn eq(lhs: (), rhs: ()) -> bool {
        true
    }
    fn ne(lhs: (), rhs: ()) -> bool {
        false
    }
}

/// Base implementation of `TuplePartialEqHelper` for fixed-sized arrays.
impl TuplePartialEqHelperBaseFixedSizedArray<T> of TuplePartialEqHelper<[@T; 0]> {
    fn eq(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        true
    }
    fn ne(lhs: [@T; 0], rhs: [@T; 0]) -> bool {
        false
    }
}

/// The recursive implementation of `TuplePartialEqHelper` for tuple style structs.
impl TuplePartialEqHelperNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +TuplePartialEqHelper<TS::Head>,
    +TuplePartialEqHelper<TS::Rest>,
    +Drop<TS::Rest>,
> of TuplePartialEqHelper<T> {
    fn eq(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::eq(lhs_head, rhs_head)
            && TuplePartialEqHelper::<TS::Rest>::eq(lhs_rest, rhs_rest)
    }
    fn ne(lhs: T, rhs: T) -> bool {
        let (lhs_head, lhs_rest) = TS::split_head(lhs);
        let (rhs_head, rhs_rest) = TS::split_head(rhs);
        TuplePartialEqHelper::<TS::Head>::ne(lhs_head, rhs_head)
            || TuplePartialEqHelper::<TS::Rest>::ne(lhs_rest, rhs_rest)
    }
}

/// Base implementation for `Default` for tuples.
impl DefaultTupleBase of Default<()> {
    fn default() -> () {
        ()
    }
}

/// Base implementation for `Default` for fixed-sized arrays.
impl DefaultFixedSizedArray<T> of Default<[T; 0]> {
    fn default() -> [T; 0] {
        []
    }
}

/// Recursive implementation for `Default` for tuple style structs.
impl DefaultNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +Default<TS::Head>,
    +Default<TS::Rest>,
    +Drop<TS::Head>,
> of Default<T> {
    fn default() -> T {
        TS::reconstruct(Default::default(), Default::default())
    }
}

impl FixedSizedArrayDrop<T, +Drop<T>, const N: u32> of Drop<[T; N]>;
impl FixedSizedArrayCopy<T, +Copy<T>, const N: u32> of Copy<[T; N]>;

/// Recursive implementation of `Destruct` for tuple style structs.
impl TupleNextDestruct<
    T,
    impl TH: core::metaprogramming::TupleSplit<T>,
    +Destruct<TH::Head>,
    +Destruct<TH::Rest>,
    -Drop<T>,
> of Destruct<T> {
    fn destruct(self: T) nopanic {
        let (_head, _rest) = TH::split_head(self);
    }
}
