#[derive(Drop)]
pub struct Error {}

/// Configuration for formatting.
#[derive(Default, Drop)]
pub struct Formatter {
    /// The pending result of formatting.
    pub buffer: ByteArray,
}

/// A trait for standard formatting, using the empty format ("{}").
pub trait Display<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error>;
}

impl DisplayByteArray of Display<ByteArray> {
    fn fmt(self: @ByteArray, ref f: Formatter) -> Result<(), Error> {
        f.buffer.append(self);
        Result::Ok(())
    }
}

impl DisplayInteger<
    T, +core::to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>
> of Display<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        // TODO(yuval): determine base according to Formatter parameters.
        let base: T = 10_u8.into();
        self.append_formatted_to_byte_array(ref f.buffer, base.try_into().unwrap());
        Result::Ok(())
    }
}

impl DisplaySignedInteger<
    Signed,
    Unsigned,
    +core::integer::AbsAndSign<Signed, Unsigned>,
    +Display<Unsigned>,
    +Copy<Signed>,
    +Drop<Unsigned>,
> of Display<Signed> {
    fn fmt(self: @Signed, ref f: Formatter) -> Result<(), Error> {
        let (abs, sign) = (*self).abs_and_sign();
        if sign {
            write!(f, "-")?;
        }
        abs.fmt(ref f)
    }
}

impl DisplayNonZero<T, +Display<T>, +Copy<T>, +Drop<T>> of Display<NonZero<T>> {
    fn fmt(self: @NonZero<T>, ref f: Formatter) -> Result<(), Error> {
        let value: T = (*self).into();
        write!(f, "{value}")
    }
}

impl DisplayBool of Display<bool> {
    fn fmt(self: @bool, ref f: Formatter) -> Result<(), Error> {
        if *self {
            write!(f, "true")
        } else {
            write!(f, "false")
        }
    }
}

impl DisplaySnapshot<T, +Display<T>> of Display<@T> {
    fn fmt(self: @@T, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(*self, ref f)
    }
}

/// A trait for debug formatting, using the empty format ("{:?}").
pub trait Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error>;
}

impl DebugByteArray of Debug<ByteArray> {
    fn fmt(self: @ByteArray, ref f: Formatter) -> Result<(), Error> {
        write!(f, "\"")?;
        Display::fmt(self, ref f)?;
        write!(f, "\"")
    }
}

impl DebugInteger<
    T, +core::to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(self, ref f)
    }
}

impl DebugSignedInteger<
    Signed,
    Unsigned,
    +core::integer::AbsAndSign<Signed, Unsigned>,
    +Display<Unsigned>,
    +Copy<Signed>,
    +Drop<Unsigned>,
> of Debug<Signed> {
    fn fmt(self: @Signed, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(self, ref f)
    }
}

impl DebugNonZero<T, +Debug<T>, +Copy<T>, +Drop<T>> of Debug<NonZero<T>> {
    fn fmt(self: @NonZero<T>, ref f: Formatter) -> Result<(), Error> {
        let value: T = (*self).into();
        write!(f, "{value:?}")
    }
}

impl DebugBool of Debug<bool> {
    fn fmt(self: @bool, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(self, ref f)
    }
}

impl DebugSnapshot<T, +Debug<T>> of Debug<@T> {
    fn fmt(self: @@T, ref f: Formatter) -> Result<(), Error> {
        write!(f, "@")?;
        Debug::fmt(*self, ref f)
    }
}

/// Tuple `Debug` implementation.
impl TupleDebug<
    T,
    impl TSF: core::metaprogramming::TupleSnapForward<T>,
    +TupleDebugHelper<TSF::SnapForward>,
    +core::metaprogramming::IsTuple<T>,
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        write!(f, "(")?;
        TupleDebugHelper::fmt(TSF::snap_forward(self), ref f)?;
        write!(f, ")")
    }
}

/// Fixed sized array `Debug` implementation.
impl FixedSizedArrayDebug<
    T,
    impl TSF: core::metaprogramming::TupleSnapForward<T>,
    +TupleDebugHelper<TSF::SnapForward>,
    -core::metaprogramming::IsTuple<T>,
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        write!(f, "[")?;
        TupleDebugHelper::fmt(TSF::snap_forward(self), ref f)?;
        write!(f, "]")
    }
}

/// A helper trait for `Debug` implementation of tuples.
trait TupleDebugHelper<T> {
    fn fmt(value: T, ref f: Formatter) -> Result<(), Error>;
}

/// An implementation of `TupleDebugHelper` for snapshots of types with `Debug` implementations.
impl TupleDebugHelperFromDebug<T, +Debug<T>> of TupleDebugHelper<@T> {
    fn fmt(value: @T, ref f: Formatter) -> Result<(), Error> {
        Debug::fmt(value, ref f)
    }
}

/// `Debug` impl for tuples of size 0.
impl TupleDebugHelperTuple0 of TupleDebugHelper<()> {
    fn fmt(value: (), ref f: Formatter) -> Result<(), Error> {
        Result::Ok(())
    }
}

/// `Debug` impl for tuples of size 1.
impl TupleDebugHelperTuple1<E0, +TupleDebugHelper<@E0>> of TupleDebugHelper<(@E0,)> {
    fn fmt(value: (@E0,), ref f: Formatter) -> Result<(), Error> {
        let (e0,) = value;
        TupleDebugHelper::fmt(e0, ref f)?;
        write!(f, ",")
    }
}

/// `Debug` impl for tuples of size 2.
impl TupleDebugHelperTuple2<
    E0, E1, +TupleDebugHelper<@E0>, +TupleDebugHelper<@E1>
> of TupleDebugHelper<(@E0, @E1)> {
    fn fmt(value: (@E0, @E1), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1) = value;
        TupleDebugHelper::fmt(e0, ref f)?;
        write!(f, ", ")?;
        TupleDebugHelper::fmt(e1, ref f)
    }
}

/// `Debug` impl for tuples of size 3 and above.
/// Not starting from size 1 since we have special cases for 0 and 1.
impl TupleDebugHelperTupleNext<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +core::metaprogramming::IsTuple<T>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
    // Making sure the size it at least 3.
    impl NTS: core::metaprogramming::TupleSplit<TS::Rest>,
    +core::metaprogramming::TupleSplit<NTS::Rest>,
> of TupleDebugHelper<T> {
    fn fmt(value: T, ref f: Formatter) -> Result<(), Error> {
        fmt_head_and_rest(value, ref f)
    }
}

/// `Debug` impl for fixed sized arrays of size 0.
impl TupleDebugHelperFixedSizedArray0<T> of TupleDebugHelper<[@T; 0]> {
    fn fmt(value: [@T; 0], ref f: Formatter) -> Result<(), Error> {
        Result::Ok(())
    }
}

/// `Debug` impl for fixed sized arrays of size 1.
impl TupleDebugHelperFixedSizedArray1<T, +TupleDebugHelper<@T>> of TupleDebugHelper<[@T; 1]> {
    fn fmt(value: [@T; 1], ref f: Formatter) -> Result<(), Error> {
        let [e0] = value;
        TupleDebugHelper::fmt(e0, ref f)
    }
}

/// `Debug` impl for fixed sized arrays of size 2 and above.
/// Not starting from size 1 since we have a special case for 0.
impl TupleDebugHelperFixedSizedArrayNext<
    T,
    const N: usize,
    impl TS: core::metaprogramming::TupleSplit<[@T; N]>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
    // Making sure the size it at least 2.
    +core::metaprogramming::TupleSplit<TS::Rest>,
> of TupleDebugHelper<[@T; N]> {
    fn fmt(value: [@T; N], ref f: Formatter) -> Result<(), Error> {
        fmt_head_and_rest(value, ref f)
    }
}

/// A helper function for formatting the head and tail of a tuple style struct.
fn fmt_head_and_rest<
    T,
    impl TS: core::metaprogramming::TupleSplit<T>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
>(
    value: T, ref f: Formatter
) -> Result<(), Error> {
    let (head, rest) = TS::split_head(value);
    TupleDebugHelper::fmt(head, ref f)?;
    write!(f, ", ")?;
    TupleDebugHelper::fmt(rest, ref f)
}

impl ArrayTDebug<T, +Debug<T>> of Debug<Array<T>> {
    fn fmt(self: @Array<T>, ref f: Formatter) -> Result<(), Error> {
        Debug::fmt(@self.span(), ref f)
    }
}

impl SpanTDebug<T, +Debug<T>> of Debug<Span<T>> {
    fn fmt(self: @Span<T>, ref f: Formatter) -> Result<(), Error> {
        let mut self = *self;
        write!(f, "[")?;
        loop {
            match self.pop_front() {
                Option::Some(value) => {
                    if Debug::fmt(value, ref f).is_err() {
                        break Result::Err(Error {});
                    };
                    if self.is_empty() {
                        break Result::Ok(());
                    }
                    if write!(f, ", ").is_err() {
                        break Result::Err(Error {});
                    };
                },
                Option::None => { break Result::Ok(()); }
            };
        }?;
        write!(f, "]")
    }
}

/// Impls for `Debug` for types that can be converted into `felt252` using the `Into` trait.
/// Usage example:
/// ```ignore
/// impl MyTypeDebug = core::fmt::into_felt252_based::DebugImpl<MyType>;`
/// ```
pub mod into_felt252_based {
    pub impl DebugImpl<T, +Into<T, felt252>, +Copy<T>> of core::fmt::Debug<T> {
        fn fmt(self: @T, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
            core::fmt::DebugInteger::<felt252>::fmt(@(*self).into(), ref f)
        }
    }
}
