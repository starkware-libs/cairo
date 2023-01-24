trait Copy<T>;
trait Drop<T>;
// TODO(spapini): When associated types are supported, support the general trait Add<X, Y>.
trait Add<T> {
    fn add(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Sub<X, Y>.
trait Sub<T> {
    fn sub(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Mul<X, Y>.
trait Mul<T> {
    fn mul(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Div<X, Y>.
trait Div<T> {
    fn div(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Mod<X, Y>.
trait Mod<T> {
    fn modulo(a: T, b: T) -> T;
}
trait PartialEq<T> {
    fn eq(a: T, b: T) -> bool;
    fn ne(a: T, b: T) -> bool;
}
// TODO(spapini): When associated types are supported, support the general trait And<X, Y>.
trait And<T> {
    fn and(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Or<X, Y>.
trait Or<T> {
    fn or(a: T, b: T) -> T;
}
// TODO(spapini): When associated types are supported, support the general trait Xor<X, Y>.
trait Xor<T> {
    fn xor(a: T, b: T) -> T;
}
trait PartialOrd<T> {
    fn le(a: T, b: T) -> bool;
    fn ge(a: T, b: T) -> bool;
    fn lt(a: T, b: T) -> bool;
    fn gt(a: T, b: T) -> bool;
}
