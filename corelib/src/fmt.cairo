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

/// Formats a generic type using the `Display` trait.
/// Panics if a formatting trait implementation returns an error.
fn display_format<T, +Display<T>, +PanicDestruct<T>>(arg: @T) -> ByteArray {
    let mut f = Default::default();
    Display::fmt(arg, ref f).unwrap();
    f.buffer
}
