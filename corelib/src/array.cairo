use traits::IndexView;

use box::BoxTrait;
use gas::withdraw_gas;
use option::OptionTrait;
use serde::Serde;

#[derive(Drop)]
extern type Array<T>;

extern fn array_new<T>() -> Array<T> nopanic;
extern fn array_append<T>(ref arr: Array<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array<T>) -> Option<Box<T>> nopanic;
extern fn array_pop_front_consume<T>(arr: Array<T>) -> Option<(Array<T>, Box<T>)> nopanic;
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

#[generate_trait]
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
            Option::None => Option::None,
        }
    }
    #[inline(always)]
    fn pop_front_consume(self: Array<T>) -> Option<(Array<T>, T)> nopanic {
        match array_pop_front_consume(self) {
            Option::Some((arr, x)) => Option::Some((arr, x.unbox())),
            Option::None => Option::None,
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

impl ArrayDefault<T> of Default<Array<T>> {
    #[inline(always)]
    fn default() -> Array<T> {
        ArrayTrait::new()
    }
}

impl ArrayIndex<T> of IndexView<Array<T>, usize, @T> {
    fn index(self: @Array<T>, index: usize) -> @T {
        array_at(self, index).unbox()
    }
}

impl ArraySerde<T, +Serde<T>, +Drop<T>> of Serde<Array<T>> {
    fn serialize(self: @Array<T>, ref output: Array<felt252>) {
        self.len().serialize(ref output);
        serialize_array_helper(self.span(), ref output);
    }
    fn deserialize(ref serialized: Span<felt252>) -> Option<Array<T>> {
        let length = *serialized.pop_front()?;
        let mut arr = array![];
        deserialize_array_helper(ref serialized, arr, length)
    }
}

fn serialize_array_helper<T, +Serde<T>, +Drop<T>>(mut input: Span<T>, ref output: Array<felt252>) {
    match input.pop_front() {
        Option::Some(value) => {
            value.serialize(ref output);
            serialize_array_helper(input, ref output);
        },
        Option::None => {},
    }
}

fn deserialize_array_helper<T, +Serde<T>, +Drop<T>>(
    ref serialized: Span<felt252>, mut curr_output: Array<T>, remaining: felt252
) -> Option<Array<T>> {
    if remaining == 0 {
        return Option::Some(curr_output);
    }
    curr_output.append(Serde::deserialize(ref serialized)?);
    deserialize_array_helper(ref serialized, curr_output, remaining - 1)
}

// Span.
struct Span<T> {
    snapshot: @Array<T>
}

impl SpanCopy<T> of Copy<Span<T>>;
impl SpanDrop<T> of Drop<Span<T>>;

impl SpanSerde<T, +Serde<T>, +Drop<T>> of Serde<Span<T>> {
    fn serialize(self: @Span<T>, ref output: Array<felt252>) {
        (*self).len().serialize(ref output);
        serialize_array_helper(*self, ref output)
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<Span<T>> {
        let length = *serialized.pop_front()?;
        let mut arr = array_new();
        Option::Some(deserialize_array_helper(ref serialized, arr, length)?.span())
    }
}

#[generate_trait]
impl SpanImpl<T> of SpanTrait<T> {
    #[inline(always)]
    fn pop_front(ref self: Span<T>) -> Option<@T> {
        let mut snapshot = self.snapshot;
        let item = array_snapshot_pop_front(ref snapshot);
        self = Span { snapshot };
        match item {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None => Option::None,
        }
    }
    #[inline(always)]
    fn pop_back(ref self: Span<T>) -> Option<@T> {
        let mut snapshot = self.snapshot;
        let item = array_snapshot_pop_back(ref snapshot);
        self = Span { snapshot };
        match item {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None => Option::None,
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
impl ArrayTCloneImpl<T, +Clone<T>, +Drop<T>> of Clone<Array<T>> {
    fn clone(self: @Array<T>) -> Array<T> {
        let mut response = array_new();
        let mut span = self.span();
        loop {
            match span.pop_front() {
                Option::Some(v) => { response.append(v.clone()); },
                Option::None => { break (); },
            };
        };
        response
    }
}

impl ArrayPartialEq<T, +PartialEq<T>> of PartialEq<Array<T>> {
    fn eq(lhs: @Array<T>, rhs: @Array<T>) -> bool {
        lhs.span() == rhs.span()
    }
    fn ne(lhs: @Array<T>, rhs: @Array<T>) -> bool {
        !(lhs == rhs)
    }
}

impl SpanPartialEq<T, +PartialEq<T>> of PartialEq<Span<T>> {
    fn eq(lhs: @Span<T>, rhs: @Span<T>) -> bool {
        if (*lhs).len() != (*rhs).len() {
            return false;
        }
        let mut lhs_span = *lhs;
        let mut rhs_span = *rhs;
        loop {
            match lhs_span.pop_front() {
                Option::Some(lhs_v) => {
                    if lhs_v != rhs_span.pop_front().unwrap() {
                        break false;
                    }
                },
                Option::None => { break true; },
            };
        }
    }
    fn ne(lhs: @Span<T>, rhs: @Span<T>) -> bool {
        !(lhs == rhs)
    }
}
