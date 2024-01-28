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
    T, +to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>
> of Display<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        // TODO(yuval): determine base according to Formatter parameters.
        let base: T = 10_u8.into();
        self.append_formatted_to_byte_array(ref f.buffer, base.try_into().unwrap());
        Result::Ok(())
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
    T, +to_byte_array::AppendFormattedToByteArray<T>, +Into<u8, T>, +TryInto<T, NonZero<T>>
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
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

impl DebugTuple0 of Debug<()> {
    fn fmt(self: @(), ref f: Formatter) -> Result<(), Error> {
        write!(f, "()")
    }
}

impl DebugTuple1<E0, impl E0Debug: Debug<E0>> of Debug<(E0,)> {
    fn fmt(self: @(E0,), ref f: Formatter) -> Result<(), Error> {
        let (e0,) = self;
        write!(f, "(")?;
        E0Debug::fmt(e0, ref f)?;
        write!(f, ",)")
    }
}

impl DebugTuple2<E0, E1, impl E0Debug: Debug<E0>, impl E1Debug: Debug<E1>> of Debug<(E0, E1)> {
    fn fmt(self: @(E0, E1), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1) = self;
        write!(f, "(")?;
        E0Debug::fmt(e0, ref f)?;
        write!(f, ", ")?;
        E1Debug::fmt(e1, ref f)?;
        write!(f, ")")
    }
}

impl DebugTuple3<
    E0, E1, E2, impl E0Debug: Debug<E0>, impl E1Debug: Debug<E1>, impl E2Debug: Debug<E2>
> of Debug<(E0, E1, E2)> {
    fn fmt(self: @(E0, E1, E2), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1, e2) = self;
        write!(f, "(")?;
        E0Debug::fmt(e0, ref f)?;
        write!(f, ", ")?;
        E1Debug::fmt(e1, ref f)?;
        write!(f, ", ")?;
        E2Debug::fmt(e2, ref f)?;
        write!(f, ")")
    }
}

impl DebugTuple4<
    E0,
    E1,
    E2,
    E3,
    impl E0Debug: Debug<E0>,
    impl E1Debug: Debug<E1>,
    impl E2Debug: Debug<E2>,
    impl E3Debug: Debug<E3>
> of Debug<(E0, E1, E2, E3)> {
    fn fmt(self: @(E0, E1, E2, E3), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1, e2, e3) = self;
        write!(f, "(")?;
        E0Debug::fmt(e0, ref f)?;
        write!(f, ", ")?;
        E1Debug::fmt(e1, ref f)?;
        write!(f, ", ")?;
        E2Debug::fmt(e2, ref f)?;
        write!(f, ", ")?;
        E3Debug::fmt(e3, ref f)?;
        write!(f, ")")
    }
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
