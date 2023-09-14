#[test]
fn var_dropped_after_merge_remapping() {
    let mut x: u32 = 0;
    if (x == 0 && x != 1 && false) {
        x = x + 1;
    }
}
