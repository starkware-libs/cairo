trait ATrait<T> {}
trait ATrait2<T, impl Timpl: ATrait<T>> {}

struct B<T, impl Timpl1: ATrait<T>, impl Timpl2: ATrait2<T>> {
    t: T
}
