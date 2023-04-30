use box::BoxTrait;
use gas::withdraw_gas;
use option::OptionTrait;
use span::Span;
use span::SpanTrait;
use traits::IndexView;

extern type Array<T>;
extern fn array_new<T>() -> Array<T> nopanic;
extern fn array_append<T>(ref arr: Array<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array<T>) -> Option<Box<T>> nopanic;
extern fn array_to_span<T>(arr: Array<T>) -> Span<T> nopanic;
extern fn snapshot_array_as_span<T>(arr: @Array<T>) -> Span<@T> nopanic;

trait ArrayTrait<T> {
    fn new() -> Array<T>;
    fn append(ref self: Array<T>, value: T);
    fn pop_front(ref self: Array<T>) -> Option<T> nopanic;
    fn get(self: @Array<T>, index: usize) -> Option<Box<@T>>;
    fn at(self: @Array<T>, index: usize) -> @T;
    fn len(self: @Array<T>) -> usize;
    fn is_empty(self: @Array<T>) -> bool;
    fn span(self: @Array<T>) -> Span<@T>;
    fn into_span(self: Array<T>) -> Span<T>;
}
impl ArrayImpl<T> of ArrayTrait<T> {
    #[inline(always)]
    fn new() -> Array<T> {
        array_new()
    }
    #[inline(always)]
    fn append(ref self: Array<T>, value: T) {
        array_append(ref self, value)
    }
    #[inline(always)]
    fn pop_front(ref self: Array<T>) -> Option<T> nopanic {
        match array_pop_front(ref self) {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None(_) => Option::None(()),
        }
    }
    #[inline(always)]
    fn get(self: @Array<T>, index: usize) -> Option<Box<@T>> {
        self.span().get(index)
    }
    #[inline(always)]
    fn at(self: @Array<T>, index: usize) -> @T {
        self.span().at(index)
    }
    #[inline(always)]
    fn len(self: @Array<T>) -> usize {
        self.span().len()
    }
    #[inline(always)]
    fn is_empty(self: @Array<T>) -> bool {
        self.len() == 0_usize
    }
    #[inline(always)]
    fn span(self: @Array<T>) -> Span<@T> {
        snapshot_array_as_span(self)
    }
    #[inline(always)]
    fn into_span(self: Array<T>) -> Span<T> {
        array_to_span(self)
    }
}

impl ArrayIndex<T> of IndexView<Array<T>, usize, @T> {
    fn index(self: @Array<T>, index: usize) -> @T {
        self.at(index)
    }
}

// Impls for common generic types
impl ArrayDrop<T, impl TDrop: Drop<T>> of Drop<Array<T>>;

// TODO(spapini): Remove TDrop. It is necessary to get rid of response in case of panic.
impl ArrayTCloneImpl<T, impl TClone: Clone<T>, impl TDrop: Drop<T>> of Clone<Array<T>> {
    fn clone(self: @Array<T>) -> Array<T> {
        let mut response = array_new();
        let mut span = self.span();
        loop {
            match span.pop_front() {
                Option::Some(v) => {
                    response.append(TClone::clone(v));
                },
                Option::None(_) => {
                    break ();
                },
            };
        };
        response
    }
}
