fn call_with<F, +Drop<F>, +core::ops::Fn<F, (A,)>[Output: usize]>(func: F) -> usize {
    func(A { inner: array![] })
}


#[derive(Drop)]
struct A {
    inner: Array<usize>,
}

fn main() {
    let aclosure = |mut a: A| {
        a.inner.append(3);
        *a.inner[0]
    };
    call_with(aclosure);
}
