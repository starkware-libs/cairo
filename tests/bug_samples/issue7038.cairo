trait DefaultTraitImpl {
    fn call<T, F, +Drop<F>, impl func: core::ops::FnOnce<F, ()>[Output: T], +Drop<T>>(
        f: F,
    ) {
        func::call(f, ());
        f();
    }
}
