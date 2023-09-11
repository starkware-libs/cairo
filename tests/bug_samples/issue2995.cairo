trait ATrait {}

struct B<T, G, impl GImpl: ATrait> {
    t: T,
    g: G,
}

impl BCopy<T, G, impl GImpl: ATrait, +Copy<T>, +Copy<G>> of Copy<B<T, G>>;
