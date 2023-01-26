use array::ArrayTrait;

enum Option<T> {
    Some: T,
    None: (),
}
trait OptionTrait<T> {
    /// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics with `err`.
    fn expect(self: Option::<T>, err: felt) -> T;
    /// If `val` is `Option::Some(x)`, returns `x`. Otherwise, panics.
    fn unwrap(self: Option::<T>) -> T;
    /// Returns `true` if the `Option` is `Option::Some`.
    fn is_some(self: Option::<T>) -> bool;
    /// Returns `true` if the `Option` is `Option::None`.
    fn is_none(self: Option::<T>) -> bool;
}
impl OptionTraitImpl<T> of OptionTrait::<T> {
    fn expect(self: Option::<T>, err: felt) -> T {
        match self {
            Option::Some(x) => x,
            Option::None(()) => {
                let mut data = ArrayTrait::new();
                data.append(err)
                panic(data)
            },
        }
    }
    fn unwrap(self: Option::<T>) -> T {
        self.expect('Option::unwrap failed.')
    }
    fn is_some(self: Option::<T>) -> bool {
        match self {
            Option::Some(_) => true,
            Option::None(_) => false,
        }
    }
    fn is_none(self: Option::<T>) -> bool {
        match self {
            Option::Some(_) => false,
            Option::None(_) => true,
        }
    }
}

// Impls for common generic types
impl OptionUnitCopy of Copy::<Option::<()>>;
impl OptionUnitDrop of Drop::<Option::<()>>;
