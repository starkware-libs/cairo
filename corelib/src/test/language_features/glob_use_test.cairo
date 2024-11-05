pub mod const_module {
    pub const C: u8 = 1;
}

use const_module::*;

#[test]
fn test_use_star_shadowing() {
    assert_eq!(C, 1);
    {
        const C: u8 = 2;
        assert_eq!(C, 2);
    }
}

