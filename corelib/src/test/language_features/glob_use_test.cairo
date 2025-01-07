mod a {
    pub const NON_SHADOWED: felt252 = 'non_shadowed';
    pub const SHADOWED: felt252 = 'original_shadowed';
    pub const PUBLISHED_ONCE: felt252 = 'published_once';
}

mod b {
    const PUBLISHED_ONCE: felt252 = 'ignored';
    #[allow(unused_imports)]
    use core::array::Span;
}

pub const SHADOWED: felt252 = 'new_shadowed';
use a::*;
use b::*;

#[test]
fn test_use_star_shadowing() {
    assert_eq!(NON_SHADOWED, 'non_shadowed');
    assert_eq!(SHADOWED, 'new_shadowed');
}

#[test]
fn test_use_star_non_pub() {
    assert_eq!(PUBLISHED_ONCE, 'published_once');
    let _x: Span<felt252> = [].span();
}
