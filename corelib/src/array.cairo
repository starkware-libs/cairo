#[feature("deprecated-index-traits")]
use core::traits::IndexView;

use core::box::BoxTrait;
use core::gas::withdraw_gas;
use core::option::OptionTrait;
use core::serde::Serde;
use core::metaprogramming::TypeEqual;
use core::iter::Iterator;
use core::RangeCheck;

/// A collection of elements of the same type continuous in memory.
#[derive(Drop)]
pub extern type Array<T>;

extern fn array_new<T>() -> Array<T> nopanic;
extern fn array_append<T>(ref arr: Array<T>, value: T) nopanic;
extern fn array_pop_front<T>(ref arr: Array<T>) -> Option<Box<T>> nopanic;
extern fn array_pop_front_consume<T>(arr: Array<T>) -> Option<(Array<T>, Box<T>)> nopanic;
pub(crate) extern fn array_snapshot_pop_front<T>(ref arr: @Array<T>) -> Option<Box<@T>> nopanic;
extern fn array_snapshot_pop_back<T>(ref arr: @Array<T>) -> Option<Box<@T>> nopanic;
extern fn array_snapshot_multi_pop_front<PoppedT, impl Info: FixedSizedArrayInfo<PoppedT>>(
    ref arr: @Array<Info::Element>
) -> Option<@Box<PoppedT>> implicits(RangeCheck) nopanic;
extern fn array_snapshot_multi_pop_back<PoppedT, impl Info: FixedSizedArrayInfo<PoppedT>>(
    ref arr: @Array<Info::Element>
) -> Option<@Box<PoppedT>> implicits(RangeCheck) nopanic;
#[panic_with('Index out of bounds', array_at)]
extern fn array_get<T>(
    arr: @Array<T>, index: usize
) -> Option<Box<@T>> implicits(RangeCheck) nopanic;
extern fn array_slice<T>(
    arr: @Array<T>, start: usize, length: usize
) -> Option<@Array<T>> implicits(RangeCheck) nopanic;
extern fn array_len<T>(arr: @Array<T>) -> usize nopanic;

/// Basic trait for the `Array` type.
#[generate_trait]
pub impl ArrayImpl<T> of ArrayTrait<T> {
    /// Creates a new array.
    #[inline(always)]
    fn new() -> Array<T> nopanic {
        array_new()
    }
    /// Adds a value T to the end of the array
    ///
    /// Example:
    ///
    /// ```
    /// let mut arr = array![];
    /// arr.append(1);
    /// arr.append(2);
    /// ```
    #[inline(always)]
    fn append(ref self: Array<T>, value: T) nopanic {
        array_append(ref self, value)
    }
    /// Adds a span to the end of the array.
    ///
    /// Example:
    ///
    /// ```
    /// let mut arr = array![];
    /// arr.append_span(array![3, 4, 5].span());
    /// ```
    fn append_span<+Clone<T>, +Drop<T>>(ref self: Array<T>, mut span: Span<T>) {
        match span.pop_front() {
            Option::Some(current) => {
                self.append(current.clone());
                self.append_span(span);
            },
            Option::None => {}
        };
    }
    /// Pops a value from the front of the array.
    /// Return `Option::Some` if the array is not empty, `Option::None` otherwise.
    ///
    /// Example:
    ///
    /// ```
    /// let mut arr = array![2, 3, 4];
    /// assert!(arr.pop_front() == Option::Some(2));
    /// assert!(arr.pop_front() == Option::Some(3));
    /// assert!(arr.pop_front() == Option::Some(4));
    /// assert!(arr.pop_front() == Option::None);
    /// ```
    #[inline(always)]
    fn pop_front(ref self: Array<T>) -> Option<T> nopanic {
        match array_pop_front(ref self) {
            Option::Some(x) => Option::Some(x.unbox()),
            Option::None => Option::None,
        }
    }
    /// Pops a value from the front of the array.
    /// Return `Option::Some` containing the remaining array and the value removed if the array is
    /// not empty, otherwise `Option::None` and drops the array.
    ///
    /// Example:
    ///
    /// ```
    /// let arr = array![2, 3, 4];
    /// assert!(arr.pop_front_consume() == Option::Some((array![3, 4], 2)));
    /// ```
    #[inline(always)]
    fn pop_front_consume(self: Array<T>) -> Option<(Array<T>, T)> nopanic {
        match array_pop_front_consume(self) {
            Option::Some((arr, x)) => Option::Some((arr, x.unbox())),
            Option::None => Option::None,
        }
    }
    /// Returns 'Option::Some' of the value at the given 'index',
    /// if the array contains this index, 'Option::None' otherwise.
    ///
    /// Example:
    ///
    /// ```
    /// let arr = array![2, 3, 4];
    /// assert!(arr.get(1) == Option::Some(@3));
    /// ```
    #[inline(always)]
    fn get(self: @Array<T>, index: usize) -> Option<Box<@T>> {
        array_get(self, index)
    }
    /// Returns a snapshot of the value at the given 'index'.
    ///
    /// Example:
    ///
    /// ```
    /// let arr = array![2, 3, 4];
    /// assert!(arr.at(1) == @4);
    /// ```
    fn at(self: @Array<T>, index: usize) -> @T {
        array_at(self, index).unbox()
    }
    /// Returns the length of the array.
    ///
    /// Example:
    ///
    /// ```
    /// let arr = array![2, 3, 4];
    /// assert!(arr.len() == 3);
    /// ```
    #[inline(always)]
    #[must_use]
    fn len(self: @Array<T>) -> usize {
        array_len(self)
    }
    /// Returns whether the array is empty.
    ///
    /// Example:
    ///
    /// ```
    /// let mut arr = array![];
    /// assert!(arr.is_empty());
    /// arr.append(1);
    /// assert!(!arr.is_empty());
    /// ```
    #[inline(always)]
    #[must_use]
    fn is_empty(self: @Array<T>) -> bool {
        let mut snapshot = self;
        match array_snapshot_pop_front(ref snapshot) {
            Option::Some(_) => false,
            Option::None => true,
        }
    }
    /// Returns a span of the array.
    #[inline(always)]
    #[must_use]
    fn span(snapshot: @Array<T>) -> Span<T> {
        Span { snapshot }
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

/// A span is a view into a continuous collection of the same type - such as `Array`.
pub struct Span<T> {
    /// The snapshot of the array.
    pub(crate) snapshot: @Array<T>
}

impl SpanCopy<T> of Copy<Span<T>>;
impl SpanDrop<T> of Drop<Span<T>>;

impl ArrayIntoSpan<T, +Drop<T>> of Into<Array<T>, Span<T>> {
    fn into(self: Array<T>) -> Span<T> {
        self.span()
    }
}

impl SpanIntoArray<T, +Drop<T>, +Clone<T>> of Into<Span<T>, Array<T>> {
    fn into(self: Span<T>) -> Array<T> {
        let mut arr = array![];
        arr.append_span(self);
        arr
    }
}

impl SpanIntoArraySnap<T> of Into<Span<T>, @Array<T>> {
    fn into(self: Span<T>) -> @Array<T> {
        self.snapshot
    }
}

impl SpanFelt252Serde of Serde<Span<felt252>> {
    fn serialize(self: @Span<felt252>, ref output: Array<felt252>) {
        (*self).len().serialize(ref output);
        serialize_array_helper(*self, ref output)
    }

    fn deserialize(ref serialized: Span<felt252>) -> Option<Span<felt252>> {
        let length: u32 = (*serialized.pop_front()?).try_into()?;
        let res = serialized.slice(0, length);
        serialized = serialized.slice(length, serialized.len() - length);
        Option::Some(res)
    }
}

impl SpanSerde<T, +Serde<T>, +Drop<T>, -TypeEqual<felt252, T>> of Serde<Span<T>> {
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

/// Basic trait for the `Span` type.
#[generate_trait]
pub impl SpanImpl<T> of SpanTrait<T> {
    /// Pops a value from the front of the span.
    /// Return `Option::Some` if the span is not empty, `Option::None` otherwise.
    ///
    /// Example:
    /// ```
    /// let mut span = array![1, 2, 3].span();
    /// assert!(span.pop_front() == Option::Some(@1));
    /// ```
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
    /// Pops a value from the back of the span.
    /// Return `Option::Some` if the span is not empty, `Option::None` otherwise.
    ///
    /// Example:
    /// ```
    /// let mut span = array![1, 2, 3].span();
    /// assert!(span.pop_back() == Option::Some(@7));
    /// ```
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
    /// Pops multiple values from the front of the span.
    /// Returns 'Option::Some' of the elements removed if the
    /// action completed successfully, 'Option::None' otherwise.
    ///
    /// Example:
    /// ```
    /// let mut span = array![1, 2, 3].span();
    /// assert!(span.multi_pop_front::<2>() == Option::Some(@[1, 2]));
    /// ```
    fn multi_pop_front<const SIZE: usize>(ref self: Span<T>) -> Option<@Box<[T; SIZE]>> {
        array_snapshot_multi_pop_front(ref self.snapshot)
    }
    /// Pops multiple values from the back of the span.
    /// Returns 'Option::Some' of the elements removed if the
    /// action completed successfully, 'Option::None' otherwise.
    ///
    /// Example:
    /// ```
    /// let mut span = array![1, 2, 3].span();
    /// assert!(span.multi_pop_back::<2>() == Option::Some(@[2, 3]));
    /// ```
    fn multi_pop_back<const SIZE: usize>(ref self: Span<T>) -> Option<@Box<[T; SIZE]>> {
        array_snapshot_multi_pop_back(ref self.snapshot)
    }
    /// Returns 'Option::Some' of the value at the given 'index',
    ///
    /// Example:
    /// ```
    /// let span = array![1, 2, 3].span();
    /// assert!(span.get(1) == Option::Some(@2));
    /// ```
    #[inline(always)]
    fn get(self: Span<T>, index: usize) -> Option<Box<@T>> {
        array_get(self.snapshot, index)
    }
    /// Returns a snapshot of the value at the given 'index'.
    ///
    /// Example:
    /// ```
    /// let span = array![1, 2, 3].span();
    /// assert!(span.at(1) == @2);
    /// ```
    #[inline(always)]
    fn at(self: Span<T>, index: usize) -> @T {
        array_at(self.snapshot, index).unbox()
    }
    /// Returns a span containing values from the 'start' index, with
    /// amount equal to 'length'.
    ///
    /// Example:
    /// ```
    /// let span = array![1, 2, 3].span();
    /// assert!(span.slice(1, 2) == array![2, 3].span());
    /// ```
    #[inline(always)]
    fn slice(self: Span<T>, start: usize, length: usize) -> Span<T> {
        Span { snapshot: array_slice(self.snapshot, start, length).expect('Index out of bounds') }
    }
    /// Returns the length of the span.
    ///
    /// Example:
    /// ```
    /// let span = array![1, 2, 3].span();
    /// assert!(span.len() == 3);
    /// ```
    #[inline(always)]
    #[must_use]
    fn len(self: Span<T>) -> usize {
        array_len(self.snapshot)
    }
    /// Returns whether the span is empty.
    ///
    /// Example:
    /// ```
    /// let span = array![1, 2, 3].span();
    /// assert!(!span.is_empty());
    /// ```
    #[inline(always)]
    #[must_use]
    fn is_empty(self: Span<T>) -> bool {
        let mut snapshot = self.snapshot;
        match array_snapshot_pop_front(ref snapshot) {
            Option::Some(_) => false,
            Option::None => true,
        }
    }
}

/// Implementation of the `SpanIndex` trait for spans.
pub impl SpanIndex<T> of IndexView<Span<T>, usize, @T> {
    #[inline(always)]
    fn index(self: @Span<T>, index: usize) -> @T {
        array_at(*self.snapshot, index).unbox()
    }
}

/// A Trait that, given a data structure, returns a span of the data.
pub trait ToSpanTrait<C, T> {
    /// Returns a span pointing to the data in the input.
    #[must_use]
    fn span(self: @C) -> Span<T>;
}

impl ArrayToSpan<T> of ToSpanTrait<Array<T>, T> {
    #[inline(always)]
    fn span(self: @Array<T>) -> Span<T> {
        ArrayTrait::span(self)
    }
}

impl SnapIntoSpanWhereToSpanTrait<C, T, +ToSpanTrait<C, T>> of Into<@C, Span<T>> {
    fn into(self: @C) -> Span<T> {
        self.span()
    }
}

/// Returns a span from a box of struct of members of the same type.
/// The additional `+Copy<@T>` arg is to prevent later stages from propagating the `S` type Sierra
/// level, where it is deduced by the `T` type.
extern fn span_from_tuple<T, impl Info: FixedSizedArrayInfo<T>>(
    struct_like: Box<@T>
) -> @Array<Info::Element> nopanic;

impl FixedSizeArrayBoxToSpan<T, const SIZE: usize> of ToSpanTrait<Box<@[T; SIZE]>, T> {
    fn span(self: @Box<@[T; SIZE]>) -> Span<T> {
        Span { snapshot: span_from_tuple(*self) }
    }
}

impl FixedSizeArrayToSpan<
    T, const SIZE: usize, -TypeEqual<[T; SIZE], [T; 0]>
> of ToSpanTrait<[T; SIZE], T> {
    #[inline(always)]
    fn span(self: @[T; SIZE]) -> Span<T> {
        BoxTrait::new(self).span()
    }
}

impl EmptyFixedSizeArrayImpl<T, +Drop<T>> of ToSpanTrait<[T; 0], T> {
    #[inline(always)]
    fn span(self: @[T; 0]) -> Span<T> {
        array![].span()
    }
}

/// Returns a box of struct of members of the same type from a span.
/// The additional `+Copy<@T>` arg is to prevent later stages from propagating the `S` type Sierra
/// level, where it is deduced by the `T` type.
extern fn tuple_from_span<T, impl Info: FixedSizedArrayInfo<T>>(
    span: @Array<Info::Element>
) -> Option<@Box<T>> nopanic;

/// Implements `TryInto` for only copyable types
impl SpanTryIntoFixedSizedArray<
    T, const SIZE: usize, -TypeEqual<[T; SIZE], [T; 0]>
> of TryInto<Span<T>, @Box<[T; SIZE]>> {
    #[inline(always)]
    fn try_into(self: Span<T>) -> Option<@Box<[T; SIZE]>> {
        tuple_from_span(self.snapshot)
    }
}

impl SpanTryIntoEmptyFixedSizedArray<T, +Drop<T>> of TryInto<Span<T>, @Box<[T; 0]>> {
    #[inline(always)]
    fn try_into(self: Span<T>) -> Option<@Box<[T; 0]>> {
        if self.is_empty() {
            Option::Some(@BoxTrait::new([]))
        } else {
            Option::None
        }
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
}

/// An iterator struct over a span collection.
pub struct SpanIter<T> {
    span: Span<T>,
}

impl SpanIterDrop<T> of Drop<SpanIter<T>>;
impl SpanIterCopy<T> of Copy<SpanIter<T>>;

impl SpanIterator<T> of Iterator<SpanIter<T>> {
    type Item = @T;
    fn next(ref self: SpanIter<T>) -> Option<@T> {
        self.span.pop_front()
    }
}

impl SpanIntoIterator<T> of core::iter::IntoIterator<Span<T>> {
    type IntoIter = SpanIter<T>;
    fn into_iter(self: Span<T>) -> SpanIter<T> {
        SpanIter { span: self }
    }
}

/// An iterator struct over an array collection.
#[derive(Drop)]
pub struct ArrayIter<T> {
    array: Array<T>,
}

impl ArrayIterClone<T, +core::clone::Clone<T>, +Drop<T>> of core::clone::Clone<ArrayIter<T>> {
    fn clone(self: @ArrayIter<T>) -> ArrayIter<T> {
        ArrayIter { array: core::clone::Clone::clone(self.array), }
    }
}

impl ArrayIterator<T> of Iterator<ArrayIter<T>> {
    type Item = T;
    fn next(ref self: ArrayIter<T>) -> Option<T> {
        self.array.pop_front()
    }
}

impl ArrayIntoIterator<T> of core::iter::IntoIterator<Array<T>> {
    type IntoIter = ArrayIter<T>;
    fn into_iter(self: Array<T>) -> ArrayIter<T> {
        ArrayIter { array: self }
    }
}

/// Information about a fixed-sized array.
trait FixedSizedArrayInfo<S> {
    /// The type of the elements in the array.
    type Element;
    /// The size of the array.
    const SIZE: usize;
}
impl FixedSizedArrayInfoImpl<T, const SIZE: usize> of FixedSizedArrayInfo<[T; SIZE]> {
    type Element = T;
    const SIZE: usize = SIZE;
}
