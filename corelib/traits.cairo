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
