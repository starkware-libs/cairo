/// Allocates unique sequential identifiers.
pub struct IdAllocator {
    next_id: usize,
}
impl IdAllocator {
    pub fn default() -> Self {
        Self { next_id: 0 }
    }

    pub fn allocate(&mut self) -> usize {
        let cur_id = self.next_id;
        self.next_id += 1;
        cur_id
    }
}
