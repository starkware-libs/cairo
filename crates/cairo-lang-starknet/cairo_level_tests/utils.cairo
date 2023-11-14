pub fn serialized<T, +Serde<T>, +Destruct<T>>(value: T) -> Span<felt252> {
    let mut arr = Default::default();
    value.serialize(ref arr);
    arr.span()
}
