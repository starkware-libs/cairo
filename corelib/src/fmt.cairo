#[derive(Drop)]
struct Error {}

/// Configuration for formatting.
#[derive(Default, Drop)]
struct Formatter {
    /// The pending result of formatting.
    buffer: ByteArray,
}

/// A trait for standard formatting, using the empty format ("{}").
trait Display<T> {
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

impl DisplayBool of Display<bool> {
    fn fmt(self: @bool, ref f: Formatter) -> Result<(), Error> {
        if *self {
            write!(f, "true")
        } else {
            write!(f, "false")
        }
    }
}

/// A trait for debug formatting, using the empty format ("{:?}").
trait Debug<T> {
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
    T,
    +to_byte_array::AppendFormattedToByteArray<T>,
    +Into<u8, T>,
    +TryInto<T, NonZero<T>>,
    +Copy<T>
> of Debug<T> {
    fn fmt(self: @T, ref f: Formatter) -> Result<(), Error> {
        Display::fmt(self, ref f)
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

impl DebugTuple1<E0, +Debug<E0>> of Debug<(E0,)> {
    fn fmt(self: @(E0,), ref f: Formatter) -> Result<(), Error> {
        let (e0,) = self;
        write!(f, "({e0:?},)")
    }
}

impl DebugTuple2<E0, E1, +Debug<E0>, +Debug<E1>> of Debug<(E0, E1)> {
    fn fmt(self: @(E0, E1), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1) = self;
        write!(f, "({e0:?}, {e1:?})")
    }
}

impl DebugTuple3<E0, E1, E2, +Debug<E0>, +Debug<E1>, +Debug<E2>> of Debug<(E0, E1, E2)> {
    fn fmt(self: @(E0, E1, E2), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1, e2) = self;
        write!(f, "({e0:?}, {e1:?}, {e2:?})")
    }
}

impl DebugTuple4<
    E0, E1, E2, E3, +Debug<E0>, +Debug<E1>, +Debug<E2>, +Debug<E3>
> of Debug<(E0, E1, E2, E3)> {
    fn fmt(self: @(E0, E1, E2, E3), ref f: Formatter) -> Result<(), Error> {
        let (e0, e1, e2, e3) = self;
        write!(f, "({e0:?}, {e1:?}, {e2:?}, {e3:?})")
    }
}
