trait ATrait {}

struct B<T, G, impl GImpl: ATrait> {
    t: T,
    g: G,
}

impl BCopy<T, G, impl GImpl: ATrait, impl TCopy: Copy<T>, impl GCopy: Copy<G>> of Copy<B<T, G>>;
