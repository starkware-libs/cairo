extern type Box < T >;
extern func into_box< T > (value: T) -> Box::<T>;
extern func unbox< T > (box: Box::<T>) -> T;
