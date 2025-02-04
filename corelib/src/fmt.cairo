//! Functionality for formatting values.
//!
//! The main components of this module are:
//!
//! - `Error`: A type representing formatting errors.
//! - `Formatter`: A struct that holds the configuration and buffer for formatting.
//! - `Display`: A trait for standard formatting using the empty format ("{}").
//! - `Debug`: A trait for debug formatting using the empty format ("{:?}").
//! - `LowerHex`: A trait for hex formatting in lower case.
//!
//! The module includes implementations of the [`Display`], [`Debug`] and [`LowerHex`] traits for
//! various types.

/// Dedicated type for representing formatting errors.
#[derive(Drop)]
pub struct Error {}

/// Configuration for formatting.
#[derive(Default, Drop)]
pub struct Formatter {
    /// The pending result of formatting.
    pub buffer: ByteArray,
}

/// A trait for standard formatting, using the empty format ("{}").
///
/// # Examples
///
/// ```
/// let word: ByteArray = "123";
/// println!("{}", word);
/// ```
pub trait Display<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error>;
}

impl DisplayByteArray of Display<ByteArray> {
    fn fmt(self: @ByteArray, ref f: Formatter) -> Result<(), Error> {
        f.buffer.append(self);
        Ok(())
    }
}

impl DisplayInteger<
    T, +crate::to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>,
> of Display<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        // TODO(yuval): determine base according to Formatter parameters.
        let base: T = 10_u8.into();
        self.append_formatted_to_byte_array(ref f.buffer, base.try_into().unwrap());
        Ok(())
    }
}

impl DisplaySignedInteger<
    Signed,
    Unsigned,
    +crate::integer::AbsAndSign<Signed, Unsigned>,
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
///
/// # Examples
///
/// ```
/// let word: ByteArray = "123";
/// println!("{:?}", word);
/// ```
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
    T, +crate::to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>,
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(self, ref f)
    }
}

impl DebugSignedInteger<
    Signed,
    Unsigned,
    +crate::integer::AbsAndSign<Signed, Unsigned>,
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

impl TupleDebug<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    +TupleDebugHelper<TSF::SnapForward>,
    +crate::metaprogramming::IsTuple<T>,
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        write!(f, "(")?;
        TupleDebugHelper::fmt(TSF::snap_forward(self), ref f)?;
        write!(f, ")")
    }
}

impl FixedSizedArrayDebug<
    T,
    impl TSF: crate::metaprogramming::TupleSnapForward<T>,
    +TupleDebugHelper<TSF::SnapForward>,
    -crate::metaprogramming::IsTuple<T>,
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

impl TupleDebugHelperFromDebug<T, +Debug<T>> of TupleDebugHelper<@T> {
    fn fmt(value: @T, ref f: Formatter) -> Result<(), Error> {
        Debug::fmt(value, ref f)
    }
}

impl TupleDebugHelperTuple0 of TupleDebugHelper<()> {
    fn fmt(value: (), ref f: Formatter) -> Result<(), Error> {
        Ok(())
    }
}

impl TupleDebugHelperTuple1<E0, +TupleDebugHelper<@E0>> of TupleDebugHelper<(@E0,)> {
    fn fmt(value: (@E0,), ref f: Formatter) -> Result<(), Error> {
        let (e0,) = value;
        TupleDebugHelper::fmt(e0, ref f)?;
        write!(f, ",")
    }
}

impl TupleDebugHelperTuple2<
    E0, E1, +TupleDebugHelper<@E0>, +TupleDebugHelper<@E1>,
> of TupleDebugHelper<(@E0, @E1)> {
    fn fmt(value: (@E0, @E1), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1) = value;
        TupleDebugHelper::fmt(e0, ref f)?;
        write!(f, ", ")?;
        TupleDebugHelper::fmt(e1, ref f)
    }
}

// `Debug` impl for tuples of size 3 and above.
// Not starting from size 1 nor 2 since we have special cases for 1 and 2.
impl TupleDebugHelperTupleNext<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +crate::metaprogramming::IsTuple<T>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
    // Making sure the size it at least 3.
    impl NTS: crate::metaprogramming::TupleSplit<TS::Rest>,
    +crate::metaprogramming::TupleSplit<NTS::Rest>,
> of TupleDebugHelper<T> {
    fn fmt(value: T, ref f: Formatter) -> Result<(), Error> {
        fmt_head_and_rest(value, ref f)
    }
}

impl TupleDebugHelperFixedSizedArray0<T> of TupleDebugHelper<[@T; 0]> {
    fn fmt(value: [@T; 0], ref f: Formatter) -> Result<(), Error> {
        Ok(())
    }
}

impl TupleDebugHelperFixedSizedArray1<T, +TupleDebugHelper<@T>> of TupleDebugHelper<[@T; 1]> {
    fn fmt(value: [@T; 1], ref f: Formatter) -> Result<(), Error> {
        let [e0] = value;
        TupleDebugHelper::fmt(e0, ref f)
    }
}

// `Debug` impl for fixed sized arrays of size 2 and above.
// Not starting from size 1 since we have a special case for 1.
impl TupleDebugHelperFixedSizedArrayNext<
    T,
    const N: usize,
    impl TS: crate::metaprogramming::TupleSplit<[@T; N]>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
    // Making sure the size it at least 2.
    +crate::metaprogramming::TupleSplit<TS::Rest>,
> of TupleDebugHelper<[@T; N]> {
    fn fmt(value: [@T; N], ref f: Formatter) -> Result<(), Error> {
        fmt_head_and_rest(value, ref f)
    }
}

/// A helper function for formatting the head and tail of a tuple style struct.
fn fmt_head_and_rest<
    T,
    impl TS: crate::metaprogramming::TupleSplit<T>,
    +TupleDebugHelper<TS::Head>,
    +TupleDebugHelper<TS::Rest>,
    +Drop<TS::Rest>,
>(
    value: T, ref f: Formatter,
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
                Some(value) => {
                    if Debug::fmt(value, ref f).is_err() {
                        break Err(Error {});
                    }
                    if self.is_empty() {
                        break Ok(());
                    }
                    if write!(f, ", ").is_err() {
                        break Err(Error {});
                    }
                },
                None => { break Ok(()); },
            }
        }?;
        write!(f, "]")
    }
}

/// Implementations for `Debug` and `LowerHex` for types that can be converted into `felt252` using
/// the `Into` trait.
///
/// # Examples
///
/// ```
/// impl MyTypeDebug = crate::fmt::into_felt252_based::DebugImpl<MyType>;`
/// impl MyTypeLowerHex = crate::fmt::into_felt252_based::LowerHexImpl<MyType>;
/// ```
pub mod into_felt252_based {
    pub impl DebugImpl<T, +Into<T, felt252>, +Copy<T>> of crate::fmt::Debug<T> {
        fn fmt(self: @T, ref f: crate::fmt::Formatter) -> Result<(), crate::fmt::Error> {
            crate::fmt::DebugInteger::<felt252>::fmt(@(*self).into(), ref f)
        }
    }

    pub impl LowerHexImpl<T, +Into<T, felt252>, +Copy<T>> of core::fmt::LowerHex<T> {
        fn fmt(self: @T, ref f: core::fmt::Formatter) -> Result<(), core::fmt::Error> {
            core::fmt::LowerHexInteger::<felt252>::fmt(@(*self).into(), ref f)
        }
    }
}

/// A trait for hex formatting in lower case, using the empty format ("{:x}").
pub trait LowerHex<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error>;
}

impl LowerHexInteger<
    T, +crate::to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>,
> of LowerHex<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        let base: T = 16_u8.into();
        self.append_formatted_to_byte_array(ref f.buffer, base.try_into().unwrap());
        Ok(())
    }
}

impl LowerHexNonZero<T, +LowerHex<T>, +Copy<T>, +Drop<T>> of LowerHex<NonZero<T>> {
    fn fmt(self: @NonZero<T>, ref f: Formatter) -> Result<(), Error> {
        let value: T = (*self).into();
        write!(f, "{value:x}")
    }
}
