extern type Queue<T>;
extern fn queue_new<T>() -> Queue::<T> nopanic;
extern fn queue_append<T>(ref q: Queue::<T>, value: T) nopanic;
extern fn queue_pop_front<T>(ref q: Queue::<T>) -> Option::<T> nopanic;
#[panic_with('Queue out of bounds', queue_at)]
extern fn queue_get<T>(
    ref q: Queue::<T>, index: usize
) -> Option::<T> implicits(RangeCheck) nopanic;
extern fn queue_len<T>(q: @Queue::<T>) -> usize nopanic;

trait QueueTrait<T> {
    fn new() -> Queue::<T>;
    fn append(ref self: Queue::<T>, value: T);
    fn pop_front(ref self: Queue::<T>) -> Option::<T>;
    fn get(ref self: Queue::<T>, index: usize) -> Option::<T>;
    fn at(ref self: Queue::<T>, index: usize) -> T;
    fn len(self: @Queue::<T>) -> usize;
    fn is_empty(self: @Queue::<T>) -> bool;
}
impl QueueImpl<T> of QueueTrait::<T> {
    #[inline(always)]
    fn new() -> Queue::<T> {
        queue_new()
    }
    #[inline(always)]
    fn append(ref self: Queue::<T>, value: T) {
        queue_append(ref self, value)
    }
    #[inline(always)]
    fn pop_front(ref self: Queue::<T>) -> Option::<T> {
        queue_pop_front(ref self)
    }
    #[inline(always)]
    fn get(ref self: Queue::<T>, index: usize) -> Option::<T> {
        queue_get(ref self, index)
    }
    fn at(ref self: Queue::<T>, index: usize) -> T {
        queue_at(ref self, index)
    }
    #[inline(always)]
    fn len(self: @Queue::<T>) -> usize {
        queue_len(self)
    }
    #[inline(always)]
    fn is_empty(self: @Queue::<T>) -> bool {
        self.len() == 0_usize
    }
}

// Impls for common generic types
impl QueueFeltDrop of Drop::<Queue::<felt>>;
