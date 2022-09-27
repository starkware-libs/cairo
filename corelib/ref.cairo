extern type Ref < T >;
extern func into_ref< T > (value: T) -> Ref::<T>;
extern func deref< T > (ref: Ref::<T>) -> T;
