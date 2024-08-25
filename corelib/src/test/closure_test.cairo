#[test]
fn closure() {
    let x = 8;
    let c = |a| {
        return x * (a + 3);
        7_felt252
    };
    assert_eq!(c(2), 40);
}
struct A {}
impl ADestruct of Destruct<A> {
    fn destruct(self: A) nopanic {
        let A { } = self;
    }
}
#[test]
fn closure_destruct() {
    let a = A {};
    || {
        let _b = a;
    };
}

struct PanicA {}
impl PanicADestruct of PanicDestruct<PanicA> {
    fn panic_destruct(self: PanicA, ref panic: Panic) nopanic {
        let PanicA { } = self;
    }
}
#[test]
#[should_panic]
fn closure_panic_destruct() {
    let a = PanicA {};
    || -> () {
        let _b = a;
        panic!()
    };
    panic!()
}

