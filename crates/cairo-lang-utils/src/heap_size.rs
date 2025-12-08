use std::sync::LazyLock;

use crate::ordered_hash_map::OrderedHashMap;

/// Environment variable to control whether shared allocations (Arc, SmolStr) are counted.
/// When set to "1" or "true", these types will include their heap allocations in the count.
/// Note: This may lead to double-counting when the same Arc is referenced multiple times.
static COUNT_SHARED_ALLOCATIONS: LazyLock<bool> = LazyLock::new(|| {
    std::env::var("CAIRO_HEAPSIZE_COUNT_SHARED")
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false)
});

/// Trait for calculating the heap size of an object's owned data.
/// Arc's and references are not included in the sum by default.
/// Set CAIRO_HEAPSIZE_COUNT_SHARED=1 to include shared allocations (may double-count).
pub trait HeapSize {
    /// Returns the size of the, heap-allocated, owned data in bytes.
    /// It does not include the size of the object itself (stack size), not references.
    fn heap_size(&self) -> usize;
}

// Implementations for standard library types
impl<T: HeapSize> HeapSize for Vec<T> {
    fn heap_size(&self) -> usize {
        // Vec's heap size is the capacity * size_of::<T>() + heap_size of elements
        self.capacity() * std::mem::size_of::<T>()
            + self.iter().map(|x| x.heap_size()).sum::<usize>()
    }
}

impl HeapSize for String {
    fn heap_size(&self) -> usize {
        self.capacity()
    }
}

impl<T: HeapSize> HeapSize for Option<T> {
    fn heap_size(&self) -> usize {
        match self {
            Some(x) => x.heap_size(),
            None => 0,
        }
    }
}

impl<T: HeapSize, E: HeapSize> HeapSize for Result<T, E> {
    fn heap_size(&self) -> usize {
        match self {
            Ok(value) => value.heap_size(),
            Err(err) => err.heap_size(),
        }
    }
}

impl<T: HeapSize> HeapSize for Box<T> {
    fn heap_size(&self) -> usize {
        std::mem::size_of::<T>() + self.as_ref().heap_size()
    }
}

impl<T: HeapSize> HeapSize for std::sync::Arc<T> {
    fn heap_size(&self) -> usize {
        if *COUNT_SHARED_ALLOCATIONS {
            // Count the Arc's allocation: the data plus Arc's metadata
            // Note: This may double-count if the same Arc is referenced multiple times
            std::mem::size_of::<T>() + self.as_ref().heap_size()
        } else {
            // We do not count "unowned" data by default
            0
        }
    }
}

impl<T: HeapSize> HeapSize for std::rc::Rc<T> {
    fn heap_size(&self) -> usize {
        if *COUNT_SHARED_ALLOCATIONS {
            // Count the Rc's allocation: the data plus Rc's metadata
            // Note: This may double-count if the same Rc is referenced multiple times
            std::mem::size_of::<T>() + self.as_ref().heap_size()
        } else {
            // We do not count "unowned" data by default
            0
        }
    }
}

// For types with no heap allocation
impl HeapSize for u8 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for i8 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for i32 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for u32 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for i64 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for u64 {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for usize {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for isize {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for bool {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for char {
    fn heap_size(&self) -> usize {
        0
    }
}

impl<T> HeapSize for std::marker::PhantomData<T> {
    fn heap_size(&self) -> usize {
        0
    }
}

impl HeapSize for std::path::PathBuf {
    fn heap_size(&self) -> usize {
        self.capacity()
    }
}

impl<K: HeapSize, V: HeapSize> HeapSize for std::collections::HashMap<K, V> {
    fn heap_size(&self) -> usize {
        // Approximate: capacity * size_of key/value + heap of keys/values
        self.capacity() * (std::mem::size_of::<K>() + std::mem::size_of::<V>())
            + self.iter().map(|(k, v)| k.heap_size() + v.heap_size()).sum::<usize>()
    }
}

impl<T: HeapSize> HeapSize for std::collections::HashSet<T> {
    fn heap_size(&self) -> usize {
        self.capacity() * std::mem::size_of::<T>()
            + self.iter().map(|x| x.heap_size()).sum::<usize>()
    }
}

impl<T: HeapSize> HeapSize for std::collections::BTreeSet<T> {
    fn heap_size(&self) -> usize {
        // BTreeSet has internal structure, approximate with len * size + heap
        self.len() * std::mem::size_of::<T>() + self.iter().map(|x| x.heap_size()).sum::<usize>()
    }
}

impl<K: HeapSize, V: HeapSize, BH> HeapSize for OrderedHashMap<K, V, BH> {
    fn heap_size(&self) -> usize {
        self.iter().map(|(k, v)| k.heap_size() + v.heap_size()).sum()
    }
}

// For smol_str::SmolStr
impl HeapSize for smol_str::SmolStr {
    fn heap_size(&self) -> usize {
        if *COUNT_SHARED_ALLOCATIONS && self.is_heap_allocated() {
            // SmolStr stores short strings inline, but longer strings on the heap
            // Note: This may double-count if the same SmolStr is cloned and shares the allocation
            self.len()
        } else {
            0
        }
    }
}

// For num-bigint
impl HeapSize for num_bigint::BigUint {
    fn heap_size(&self) -> usize {
        // BigUint has internal Vec<u8>
        self.to_bytes_le().capacity()
    }
}

impl HeapSize for num_bigint::BigInt {
    fn heap_size(&self) -> usize {
        self.to_bytes_le().1.capacity()
    }
}

// Implementations for tuples
impl HeapSize for () {
    fn heap_size(&self) -> usize {
        0
    }
}

impl<T0: HeapSize> HeapSize for (T0,) {
    fn heap_size(&self) -> usize {
        self.0.heap_size()
    }
}

impl<T0: HeapSize, T1: HeapSize> HeapSize for (T0, T1) {
    fn heap_size(&self) -> usize {
        self.0.heap_size() + self.1.heap_size()
    }
}

impl<T0: HeapSize, T1: HeapSize, T2: HeapSize> HeapSize for (T0, T1, T2) {
    fn heap_size(&self) -> usize {
        self.0.heap_size() + self.1.heap_size() + self.2.heap_size()
    }
}

impl<T0: HeapSize, T1: HeapSize, T2: HeapSize, T3: HeapSize> HeapSize for (T0, T1, T2, T3) {
    fn heap_size(&self) -> usize {
        self.0.heap_size() + self.1.heap_size() + self.2.heap_size() + self.3.heap_size()
    }
}

impl<T0: HeapSize, T1: HeapSize, T2: HeapSize, T3: HeapSize, T4: HeapSize> HeapSize
    for (T0, T1, T2, T3, T4)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
    }
}

impl<T0: HeapSize, T1: HeapSize, T2: HeapSize, T3: HeapSize, T4: HeapSize, T5: HeapSize> HeapSize
    for (T0, T1, T2, T3, T4, T5)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
    T7: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6, T7)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
            + self.7.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
    T7: HeapSize,
    T8: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6, T7, T8)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
            + self.7.heap_size()
            + self.8.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
    T7: HeapSize,
    T8: HeapSize,
    T9: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
            + self.7.heap_size()
            + self.8.heap_size()
            + self.9.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
    T7: HeapSize,
    T8: HeapSize,
    T9: HeapSize,
    T10: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
            + self.7.heap_size()
            + self.8.heap_size()
            + self.9.heap_size()
            + self.10.heap_size()
    }
}

impl<
    T0: HeapSize,
    T1: HeapSize,
    T2: HeapSize,
    T3: HeapSize,
    T4: HeapSize,
    T5: HeapSize,
    T6: HeapSize,
    T7: HeapSize,
    T8: HeapSize,
    T9: HeapSize,
    T10: HeapSize,
    T11: HeapSize,
> HeapSize for (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)
{
    fn heap_size(&self) -> usize {
        self.0.heap_size()
            + self.1.heap_size()
            + self.2.heap_size()
            + self.3.heap_size()
            + self.4.heap_size()
            + self.5.heap_size()
            + self.6.heap_size()
            + self.7.heap_size()
            + self.8.heap_size()
            + self.9.heap_size()
            + self.10.heap_size()
            + self.11.heap_size()
    }
}
