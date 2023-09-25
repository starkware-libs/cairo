trait ATrait {}

struct B<T, G, +ATrait> {
    t: T,
    g: G,
}

impl BCopy<T, G, +ATrait, +Copy<T>, +Copy<G>> of Copy<B<T, G>>;
