trait Trait1<T> {
    fn func1(value: T);
}

trait Trait2<T> {
    fn func2(value: T);
}

mod impls {
    impl Impl1<T, +Drop<T>> of super::Trait1<T> {
        fn func1(value: T) {}
    }
    pub impl ImplAlias1 = Impl1<felt252>;
    impl Impl2<T, +Drop<T>> of super::Trait2<T> {
        fn func2(value: T) {}
    }
    pub impl ImplAlias2 = Impl2<felt252>;
}
use impls::ImplAlias1;
impl Impl2 = impls::ImplAlias2;

fn foo() {
    Trait1::func1(0_felt252);
    Trait2::func2(0_felt252);
}
