#[path("other_paths/a.cairo")]
mod a;

#[path("other_paths/b.cairo")]
mod b;

#[path("other_paths/define_c.cairo")]
mod c;

#[path("other_paths/inner/a.cairo")]
mod inner_a;

#[path("other_paths/inner/b.cairo")]
mod inner_b;

#[test]
fn test_access_all() {
    assert_eq!(a::VALUE, 'a');
    assert_eq!(b::VALUE, 'b');
    assert_eq!(c::VALUE, 'c');
    assert_eq!(inner_a::VALUE, 'inner/a');
    assert_eq!(inner_b::VALUE, 'inner/b');
}
