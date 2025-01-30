#[test]
fn closure() {
    let x = 8;
    let c = |a| {
        return x * (a + 3);
        7_felt252
    };
    assert_eq!(c(2), 40);
}

#[derive(Destruct)]
struct DestructOnly {}

#[test]
fn closure_destruct() {
    let a = DestructOnly {};
    || {
        let _b = a;
    };
}

#[derive(PanicDestruct)]
struct PanicDestructOnly {}

#[test]
#[should_panic(expected: "outer")]
fn closure_panic_destruct() {
    let a = PanicDestructOnly {};
    || -> () {
        let _b = a;
        panic!("inner")
    };
    panic!("outer")
}

#[test]
fn panicable_closure() {
    let c = |a: u32| {
        a + 3
    };
    assert_eq!(c(2), 5);
}

struct Callable<F, +core::ops::Fn<F, ()>> {
    f: F,
}

#[test]
fn closure_snapshot_call() {
    let callable = Callable { f: || 10_u8 };
    assert_eq!(core::ops::FnOnce::call(callable.f, ()), 10);
    assert_eq!(core::ops::Fn::call(@callable.f, ()), 10);
    // With snapshot
    assert_eq!(core::ops::FnOnce::call(@callable.f, ()), 10);
    assert_eq!(core::ops::Fn::call(@@callable.f, ()), 10);
}

fn option_map<T, F, +core::ops::FnOnce<F, (T,)>, +Drop<F>>(
    opt: Option<T>, f: F,
) -> Option<core::ops::FnOnce::<F, (T,)>::Output> {
    match opt {
        Some(x) => Some(f(x)),
        None => None,
    }
}

#[test]
fn option_map_test() {
    assert_eq!(option_map(Some(2), |x| x + 3), Some(5));
    assert_eq!(option_map(None, |x| x + 3), None);
    assert_eq!(option_map(Some(2), |x| Some(x)), Some(Some(2)));
}

fn fix_sized_array_map<
    T, F, impl Fn: core::ops::Fn<F, (T,)>, +Drop<T>, +Drop<F>, +Drop<Fn::Output>,
>(
    arr: [T; 2], f: F,
) -> [core::ops::Fn::<F, (T,)>::Output; 2] {
    let [a, b] = arr;
    [f(a), f(b)]
}

#[test]
fn fix_sized_array_map_test() {
    assert_eq!(fix_sized_array_map([2, 3], |x| x + 3), [5, 6]);
}

#[generate_trait]
impl ArrayExt of ArrayExtTrait {
    fn map<T, +Drop<T>, F, +Drop<F>, impl func: core::ops::Fn<F, (T,)>, +Drop<func::Output>>(
        self: Array<T>, f: F,
    ) -> Array<func::Output> {
        let mut output: Array<func::Output> = array![];
        for elem in self {
            output.append(f(elem));
        }
        output
    }
}

#[test]
fn array_map_test() {
    let arr = array![1, 2, 3];
    let result = arr.map(|x| x + 1);
    assert_eq!(result, array![2, 3, 4]);
}
