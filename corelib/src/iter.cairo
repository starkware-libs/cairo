trait Iterator<T, Item> {
    fn next(ref self: T) -> Option<Item>;
}

trait IntoIterator<T, Item, IntoIter, impl IntoIterImpl: Iterator::<IntoIter, Item>> {
    fn into_iter(self: T) -> IntoIter;
}
