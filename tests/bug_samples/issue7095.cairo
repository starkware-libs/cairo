trait MyTrait<T> {
    fn consume<B, F, +core::ops::Fn<F, (T,)>[Output: B], +Destruct<F>>(
        self: T, f: F,
    ) -> B {
        f(self)
    }
    fn user<+Destruct<T>>(self: T) -> usize {
        Self::consume(self, |_v| 1)
    }
}

impl MyImpl<T> of MyTrait<T> {}

#[test]
fn test_call() {
    MyTrait::<u8>::user(0);
}
