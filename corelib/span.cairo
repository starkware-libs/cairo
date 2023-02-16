struct Span<T> {
    snapshot: @Array::<T>
}

trait SpanTrait<T> {
    fn len(self: Span::<T>) -> usize;
    fn is_empty(self: Span::<T>) -> bool;
}
impl SpanImpl<T> of SpanTrait::<T> {
    #[inline(always)]
    fn len(self: Span::<T>) -> usize {
        array_len(self.snapshot)
    }
    #[inline(always)]
    fn is_empty(self: Span::<T>) -> bool {
        self.len() == 0_usize
    }
}
