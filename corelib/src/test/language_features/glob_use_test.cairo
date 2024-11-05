pub mod mock_data {
    pub const C: u8 = 1;
}

use mock_data::*;

#[test]
fn test_use_star_shadowing() {
    assert_eq!(C, 1);
    {
        const C: u8 = 2;
        assert_eq!(C, 2);
    }
}
