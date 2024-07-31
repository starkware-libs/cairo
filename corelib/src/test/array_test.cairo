use core::test::test_utils::{assert_eq, assert_ne};
use core::iter::{IntoIterator, Iterator};

fn test_span_into_array_snap() -> bool {
    @array![1, 2, 3] == array![1, 2, 3].span().into()
}
