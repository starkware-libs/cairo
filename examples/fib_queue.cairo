use queue::QueueTrait;

// Returns a queue of size n with the values of the Fibonacci sequence, the length of the queue,
// and the value of the last element.
fn fib(n: usize) -> (Queue::<felt>, felt, usize) {
    let mut q = QueueTrait::new();
    q.append(1);
    q.append(1);
    let mut q = fib_inner(:n, :q);
    let len = q.len();
    let last = q.at(len - 1_usize);
    return (q, last, len);
}

fn fib_inner(n: usize, mut q: Queue::<felt>) -> Queue::<felt> {
    let length = q.len();
    if n <= length {
        return q;
    }
    q.append(q.at(length - 1_usize) + q.at(length - 2_usize));
    fib_inner(:n, :q)
}
