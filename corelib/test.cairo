#[test]
#[should_panic]
func test_assert_false() {
    assert(false, 1);
}

#[test]
func test_assert_true() {
    assert(true, 1);
}
