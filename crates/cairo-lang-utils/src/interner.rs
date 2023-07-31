use core::hash::Hash;
use std::hash::Hasher;
use std::marker::PhantomData;
use std::sync::Mutex;

use fxhash::FxHasher;

#[cfg(test)]
#[path = "interner_test.rs"]
mod test;

pub trait InternedElement: PartialEq + Hash + 'static {
    fn from_u8s(buf: &[u8]) -> &Self;
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PtrIndex(pub u32);
impl PtrIndex {
    pub fn is_empty(&self) -> bool {
        self.0 == 0
    }
}
pub struct UnsizedInterner<T: InternedElement + ?Sized> {
    inner: Mutex<MutablePart<T>>,
    ptrs: Vec<usize>,
}
impl<T: InternedElement + ?Sized> UnsizedInterner<T> {
    pub fn new(capacity: usize) -> Self {
        let mut res = Self {
            inner: Mutex::new(MutablePart {
                hash_table: vec![PtrIndex(0); capacity],
                buf: Vec::with_capacity(capacity),
                bufs: Vec::new(),
                phantom: PhantomData,
            }),
            ptrs: Vec::with_capacity(capacity),
        };
        res.ptrs.push(0); // PtrIndex(0) should not be assigned.
        res
    }

    #[allow(clippy::cast_ref_to_mut)]
    pub fn intern(&self, element: &T) -> PtrIndex {
        let mut inner = self.inner.lock().unwrap();
        let hash_table = &mut inner.hash_table;
        let h0 = self.get_hash(element);

        let mut hash_table_index = h0 as usize & (hash_table.len() - 1);
        let mut ptr_index = *unsafe { hash_table.get_unchecked(hash_table_index) };
        while !ptr_index.is_empty() {
            if self.lookup(ptr_index) == element {
                return ptr_index;
            }
            hash_table_index = (hash_table_index + 1) & (hash_table.len() - 1);
            ptr_index = *unsafe { hash_table.get_unchecked(hash_table_index) };
        }
        // Allocate a new ptr.
        let ptr_as_usize = inner.allocate_on_buf(element);
        let ptr_index = PtrIndex(self.ptrs.len() as u32);
        let ptrs_mut = unsafe { &mut *(&self.ptrs as *const Vec<usize> as *mut Vec<usize>) };
        ptrs_mut.push(ptr_as_usize);

        inner.hash_table[hash_table_index] = ptr_index;
        // Resize hash table if needed.
        if inner.hash_table.len() * 3 / 4 < self.ptrs.len() {
            self.resize_hash_table(&mut inner.hash_table);
        }
        assert!(ptr_index.0 != 0);
        ptr_index
    }

    fn get_hash(&self, element: &T) -> u64 {
        let mut hasher = FxHasher::default();
        element.hash(&mut hasher);
        hasher.finish()
    }

    pub fn lookup(&self, ptr_index: PtrIndex) -> &'static T {
        unsafe {
            let ptr = (*self.ptrs.get_unchecked(ptr_index.0 as usize)) as *const u32;
            let slice_ref = core::slice::from_raw_parts(ptr.add(1) as *const u8, *ptr as usize);
            T::from_u8s(slice_ref)
        }
    }

    fn resize_hash_table(&self, hash_table: &mut Vec<PtrIndex>) {
        let new_size = hash_table.len() * 2;
        *hash_table = vec![PtrIndex(0); new_size];
        for ptr_index in 1..self.ptrs.len() {
            let ptr_index = PtrIndex(ptr_index as u32);
            let element = self.lookup(ptr_index);
            let h0 = self.get_hash(element);
            let mut hash_table_index = h0 as usize & (new_size - 1);

            while !hash_table[hash_table_index].is_empty() {
                hash_table_index = (hash_table_index + 1) & (new_size - 1);
            }
            hash_table[hash_table_index] = ptr_index;
        }
    }
}

pub struct MutablePart<T: InternedElement + ?Sized> {
    hash_table: Vec<PtrIndex>,
    buf: Vec<u8>,
    bufs: Vec<Vec<u8>>,
    phantom: PhantomData<T>,
}
impl<T: InternedElement + ?Sized> MutablePart<T> {
    fn allocate_on_buf(&mut self, element: &T) -> usize {
        let current_capacity = self.buf.capacity();
        let element_size = std::mem::size_of_val(element);
        let needed_size = std::mem::size_of::<u32>() + element_size;
        let mut start = self.buf.len();
        start = (start + 3) & !3; // Align to 4 bytes.
        if start + needed_size > current_capacity {
            let capacity = (std::cmp::max(current_capacity, needed_size) + 1).next_power_of_two();
            let new_buf = Vec::with_capacity(capacity);
            self.bufs.push(std::mem::replace(&mut self.buf, new_buf));
            start = 0;
        };

        unsafe {
            // Write the size of the element in native bit ordering.
            let size_ptr = self.buf.as_mut_ptr().add(start) as *mut u32;
            let ptr_as_usize = size_ptr as usize;
            *size_ptr = element_size as u32;
            let element_dst_ptr = size_ptr.add(1) as *mut u8;
            std::ptr::copy_nonoverlapping(
                element as *const T as *const u8,
                element_dst_ptr,
                element_size,
            );
            self.buf.set_len(start + needed_size);
            ptr_as_usize
        }
    }
}
