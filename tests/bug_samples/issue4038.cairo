#[inline(never)]
fn get_one() -> u32 {
    1
}

#[test]
fn ap_tracked_for_wrong_vars() {
    let mut a: u32 = get_one();
    if (a == 1) {
        core::internal::revoke_ap_tracking();
    }
    if (a > 0 && a != 2) {
        a += 1;
    }
}
