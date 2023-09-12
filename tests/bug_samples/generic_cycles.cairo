trait ATrait<T> {}
trait ATrait2<T, +ATrait<T>> {}

struct B<T, +ATrait<T>, +ATrait2<T>> {
    t: T
}
