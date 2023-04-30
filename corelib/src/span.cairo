use box::BoxTrait;
use option::OptionTrait;
use traits::IndexView;

extern type Span<T>;
impl SpanCopy<T, impl TCopy: Copy<T>> of Copy<Span<T>>;
impl SpanDrop<T, impl TDrop: Drop<T>> of Drop<Span<T>>;

extern fn span_pop_front<T>(ref span: Span<T>) -> Option<Box<T>> nopanic;
extern fn span_pop_back<T>(ref span: Span<T>) -> Option<Box<T>> nopanic;
#[panic_with('Index out of bounds', span_at)]
extern fn span_get<T>(span: Span<T>, index: usize) -> Option<Box<T>> implicits(RangeCheck) nopanic;
extern fn span_slice<T>(
    span: Span<T>, start: usize, length: usize
) -> Option<Span<T>> implicits(RangeCheck) nopanic;
extern fn span_len<T>(span: @Span<T>) -> usize nopanic;
extern fn snapshot_span_as_span<T>(arr: @Span<T>) -> Span<@T> nopanic;

// TODO(spapini): Do a deref: Span<@T> -> Span<T> for copyable T.

trait SpanTrait<T> {
    fn pop_front(ref self: Span<T>) -> Option<T>;
    fn pop_back(ref self: Span<T>) -> Option<T>;
    fn get(self: Span<T>, index: usize) -> Option<Box<T>>;
    fn at(self: Span<T>, index: usize) -> T;
    fn slice(self: Span<T>, start: usize, length: usize) -> Span<T>;
    fn len(self: @Span<T>) -> usize;
    fn is_empty(self: @Span<T>) -> bool;
    fn span(self: @Span<T>) -> Span<@T>;
}
impl SpanImpl<T> of SpanTrait<T> {
    #[inline(always)]
    fn pop_front(ref self: Span<T>) -> Option<T> {
        match span_pop_front(ref self) {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None(_) => Option::None(()),
        }
    }
    #[inline(always)]
    fn pop_back(ref self: Span<T>) -> Option<T> {
        match span_pop_back(ref self) {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None(_) => Option::None(()),
        }
    }
    #[inline(always)]
    fn get(self: Span<T>, index: usize) -> Option<Box<T>> {
        span_get(self, index)
    }
    #[inline(always)]
    fn at(self: Span<T>, index: usize) -> T {
        span_at(self, index).unbox()
    }
    #[inline(always)]
    fn slice(self: Span<T>, start: usize, length: usize) -> Span<T> {
        span_slice(self, start, length).expect('Index out of bounds')
    }
    #[inline(always)]
    fn len(self: @Span<T>) -> usize {
        span_len(self)
    }
    #[inline(always)]
    fn is_empty(self: @Span<T>) -> bool {
        self.len() == 0_usize
    }
    fn span(self: @Span<T>) -> Span<@T> {
        snapshot_span_as_span(self)
    }
}

impl SpanIndex<T, impl TCopy: Copy<T>> of IndexView<Span<T>, usize, T> {
    #[inline(always)]
    fn index(self: @Span<T>, index: usize) -> T {
        span_at(*self, index).unbox()
    }
}
