/// Macro to try to evaluate an expression as a pattern and extract its fields.
/// # Examples:
/// ```
/// use utils::try_extract_matches;
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
/// if let Some(Point { x, y: _ }) = try_extract_matches!(p, MyEnum::Point) {
///     assert_eq!(x, 3);
/// }
/// ```
#[macro_export]
macro_rules! try_extract_matches {
    ($e:expr, $variant:path) => {
        if let $variant(x) = $e { Some(x) } else { None }
    };
}

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
                panic!("Variant extract failed: `{:?}` is not of variant `{}`", e, stringify!($variant))
            }
        }
    };
    ( $e:expr , $variant:path , $($arg:tt)* ) => {
        match $e {
            $variant(x) => x,
            ref e => panic!("Variant extract failed: `{:?}` is not of variant `{}`: {}",
                e, stringify!($variant), format_args!($($arg)*))
        }
    };
}
