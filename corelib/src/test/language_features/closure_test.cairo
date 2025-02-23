fn closure() ->bool {
    let x = 8;
    let c = |a| {
        return x * (a + 3);
        7_felt252
    };
    return c(2) == 40;
}

#[derive(Destruct)]
struct DestructOnly {}


fn closure_destruct() {
    let a = DestructOnly {};
    || {
        let _b = a;
    };
}

#[derive(PanicDestruct)]
struct PanicDestructOnly {}




fn panicable_closure() {
    let c = |a: u32| {
        a + 3
    };
    c(2) == 5;
}

struct Callable<F, +core::ops::Fn<F, ()>> {
    f: F,
}


fn closure_snapshot_call() {
    let callable = Callable { f: || 10_u8 };
    core::ops::Fn::call(@@callable.f, ()) ==  10;
}

fn option_map<T, F, +core::ops::FnOnce<F, (T,)>, +Drop<F>>(
    opt: Option<T>, f: F,
) -> Option<core::ops::FnOnce::<F, (T,)>::Output> {
    match opt {
        Some(x) => Some(f(x)),
        None => None,
    }
}


fn option_map_test() {
    option_map(Some(2), |x| Some(x)) == Some(Some(2));
}

fn fix_sized_array_map<
    T, F, impl Fn: core::ops::Fn<F, (T,)>, +Drop<T>, +Drop<F>, +Drop<Fn::Output>,
>(
    arr: [T; 2], f: F,
) -> [core::ops::Fn::<F, (T,)>::Output; 2] {
    let [a, b] = arr;
    [f(a), f(b)]
}


fn fix_sized_array_map_test() {
    fix_sized_array_map([2, 3], |x| x + 3) == [5, 6];
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


fn array_map_test() -> bool {
    let arr = array![1, 2, 3];
    let result = arr.map(|x| x + 1);
    result == array![2, 3, 4]
}
