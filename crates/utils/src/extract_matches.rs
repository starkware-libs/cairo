/// Macro to verify an expression matches a pattern and extract its fields.
/// # Examples:
/// ```
/// use utils::extract_matches;
///
/// #[derive(Debug, Clone, Copy)]
/// struct Point {
///     x: u32,
///     y: u32,
/// }
/// #[derive(Debug)]
/// enum MyEnum {
///     Point(Point),
///     Value(u32),
/// }
/// let p = MyEnum::Point(Point { x: 3, y: 5 });
/// let Point { x, y: _ } = extract_matches!(p, MyEnum::Point);
/// assert_eq!(x, 3);
///
/// // Would panic with 'assertion failed: `Point(Point { x: 3, y: 5 })` does not match `MyEnum::Value`:
/// // Expected a point!'
/// // let _value = extract_matches!(p, MyEnum::Value, "Expected a point!");
/// ```
#[macro_export]
macro_rules! extract_matches {
    ($e:expr, $variant:path) => {
        match $e {
            $variant(x) => x,
            ref e => {
                panic!("assertion failed: `{:?}` does not match `{}`", e, stringify!($variant))
            }
        }
    };
    ( $e:expr , $variant:path , $($arg:tt)* ) => {
        match $e {
            $variant(x) => x,
            ref e => panic!("assertion failed: `{:?}` does not match `{}`: {}",
                e, stringify!($variant), format_args!($($arg)*))
        }
    };
}
