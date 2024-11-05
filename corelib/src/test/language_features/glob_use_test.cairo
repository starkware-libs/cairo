pub mod a {
    pub const NON_SHADOWED: felt252 = 'non_shadowed';
    pub const SHADOWED: felt252 = 'original_shadowed';
}

pub const SHADOWED: u8 = 'new_shadowed';

use a::*;

#[test]
fn test_use_star_shadowing() {
    assert_eq!(NON_SHADOWED, 'non_shadowed');
    assert_eq!(SHADOWED, 'new_shadowed');
}
