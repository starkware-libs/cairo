use queue::QueueTrait;

extern fn print(message: Queue::<felt>) nopanic;

fn print_felt(message: felt) {
    let mut q = QueueTrait::new();
    q.append(message);
    print(q);
}
