pub trait HeapSize {
    /// Returns the size of the object in bytes, including the size of its fields,
    /// and the size of the data allocated on the heap.
    /// It does not include the size of the object itself (stack size).
    fn heap_size(&self) -> usize;
}

impl<T: get_size2::GetSize> HeapSize for T {
    fn heap_size(&self) -> usize {
        get_size2::GetSize::get_heap_size(self)
    }
}

pub fn heap_size<T: HeapSize>(t: &T) -> usize {
    t.heap_size()
}
