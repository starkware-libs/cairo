use traits::IndexView;

use box::BoxTrait;
use gas::withdraw_gas;
use option::OptionTrait;

extern type Array<T>;
extern fn array_new<T>() -> Array<T> nopanic;
extern fn array_append<T>(ref arr: Array<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array<T>) -> Option<Box<T>> nopanic;
extern fn array_snapshot_pop_front<T>(ref arr: @Array<T>) -> Option<Box<@T>> nopanic;
extern fn array_snapshot_pop_back<T>(ref arr: @Array<T>) -> Option<Box<@T>> nopanic;
#[panic_with('Index out of bounds', array_at)]
extern fn array_get<T>(
    arr: @Array<T>, index: usize
) -> Option<Box<@T>> implicits(RangeCheck) nopanic;
extern fn array_slice<T>(
    arr: @Array<T>, start: usize, length: usize
) -> Option<@Array<T>> implicits(RangeCheck) nopanic;
extern fn array_len<T>(arr: @Array<T>) -> usize nopanic;

trait ArrayTrait<T> {
    fn new() -> Array<T>;
    fn append(ref self: Array<T>, value: T);
    fn pop_front(ref self: Array<T>) -> Option<T> nopanic;
    fn get(self: @Array<T>, index: usize) -> Option<Box<@T>>;
    fn at(self: @Array<T>, index: usize) -> @T;
    fn len(self: @Array<T>) -> usize;
    fn is_empty(self: @Array<T>) -> bool;
    fn span(self: @Array<T>) -> Span<T>;
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
        array_get(self, index)
    }
    fn at(self: @Array<T>, index: usize) -> @T {
        array_at(self, index).unbox()
    }
    #[inline(always)]
    fn len(self: @Array<T>) -> usize {
        array_len(self)
    }
    #[inline(always)]
    fn is_empty(self: @Array<T>) -> bool {
        self.len() == 0_usize
    }
    #[inline(always)]
    fn span(self: @Array<T>) -> Span<T> {
        Span { snapshot: self }
    }
}

impl ArrayIndex<T> of IndexView<Array<T>, usize, @T> {
    fn index(self: @Array<T>, index: usize) -> @T {
        array_at(self, index).unbox()
    }
}

// Impls for common generic types
impl ArrayDrop<T, impl TDrop: Drop<T>> of Drop<Array<T>>;

// Span.
struct Span<T> {
    snapshot: @Array<T>
}

impl SpanCopy<T> of Copy<Span<T>>;
impl SpanDrop<T> of Drop<Span<T>>;

trait SpanTrait<T> {
    fn pop_front(ref self: Span<T>) -> Option<@T>;
    fn pop_back(ref self: Span<T>) -> Option<@T>;
    fn get(self: Span<T>, index: usize) -> Option<Box<@T>>;
    fn at(self: Span<T>, index: usize) -> @T;
    fn slice(self: Span<T>, start: usize, length: usize) -> Span<T>;
    fn len(self: Span<T>) -> usize;
    fn is_empty(self: Span<T>) -> bool;
}
impl SpanImpl<T> of SpanTrait<T> {
    #[inline(always)]
    fn pop_front(ref self: Span<T>) -> Option<@T> {
        let mut snapshot = self.snapshot;
        let item = array_snapshot_pop_front(ref snapshot);
        self = Span { snapshot };
        match item {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None(_) => Option::None(()),
        }
    }
    #[inline(always)]
    fn pop_back(ref self: Span<T>) -> Option<@T> {
        let mut snapshot = self.snapshot;
        let item = array_snapshot_pop_back(ref snapshot);
        self = Span { snapshot };
        match item {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None(_) => Option::None(()),
        }
    }
    #[inline(always)]
    fn get(self: Span<T>, index: usize) -> Option<Box<@T>> {
        array_get(self.snapshot, index)
    }
    #[inline(always)]
    fn at(self: Span<T>, index: usize) -> @T {
        array_at(self.snapshot, index).unbox()
    }
    #[inline(always)]
    fn slice(self: Span<T>, start: usize, length: usize) -> Span<T> {
        Span { snapshot: array_slice(self.snapshot, start, length).expect('Index out of bounds') }
    }
    #[inline(always)]
    fn len(self: Span<T>) -> usize {
        array_len(self.snapshot)
    }
    #[inline(always)]
    fn is_empty(self: Span<T>) -> bool {
        self.len() == 0_usize
    }
}

impl SpanIndex<T> of IndexView<Span<T>, usize, @T> {
    #[inline(always)]
    fn index(self: @Span<T>, index: usize) -> @T {
        array_at(*self.snapshot, index).unbox()
    }
}

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
