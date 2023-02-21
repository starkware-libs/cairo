use option::OptionTrait;

struct Span<T> {
    snapshot: @Array::<T>
}

impl SpanFeltCopy of Copy::<Span::<felt>>;
impl SpanFeltDrop of Drop::<Span::<felt>>;

trait SpanTrait<T> {
    fn get(self: Span::<T>, index: usize) -> Option::<@T>;
    fn at(self: Span::<T>, index: usize) -> @T;
    fn len(self: Span::<T>) -> usize;
    fn is_empty(self: Span::<T>) -> bool;
}
impl SpanImpl<T> of SpanTrait::<T> {
    #[inline(always)]
    fn get(self: Span::<T>, index: usize) -> Option::<@T> {
        array_get(self.snapshot, index)
    }
    #[inline(always)]
    fn at(self: Span::<T>, index: usize) -> @T {
        array_at(self.snapshot, index)
    }
    #[inline(always)]
    fn len(self: Span::<T>) -> usize {
        array_len(self.snapshot)
    }
    #[inline(always)]
    fn is_empty(self: Span::<T>) -> bool {
        self.len() == 0_usize
    }
}
