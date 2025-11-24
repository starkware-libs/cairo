extern fn local_into_box<T>(value: T) -> Box<T> nopanic;

/// Boxes a value by storing it in a local variable first (via calling conventions), then delegating
/// to `local_into_box` which expects a local.
#[inline(never)]
pub fn into_box<T>(value: T) -> Box<T> {
    local_into_box(value)
}

#[test]
fn test_local_into_box() {
    assert_eq!(into_box((1, 2_u256, 3)).unbox(), (1, 2, 3));
    assert_eq!(into_box(()).unbox(), ());
    assert_eq!(into_box(Some(6_u8)).unbox(), Some(6));
    assert_eq!(into_box(None::<u8>).unbox(), None);
    assert_eq!(into_box(Ok::<u16, u256>(7)).unbox(), Ok(7));
    assert_eq!(into_box(Err::<u16, u256>(8)).unbox(), Err(8));
}
