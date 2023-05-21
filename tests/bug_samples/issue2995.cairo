trait Trait0<T> {}
trait ATrait {}

struct B<T, G, impl GTriat: ATrait> {
    t: T,
    g: G,
}

impl BCopy<T, G, impl GTriat: ATrait, impl TCopy: Copy<T>, impl GCopy: Copy<G>> of Copy<B<T, G>>;
