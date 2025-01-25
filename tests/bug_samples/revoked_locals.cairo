#[inline(never)]
fn use_v(v: NonZero<felt252>) {
    core::internal::revoke_ap_tracking();
}


#[test]
fn test_revoked_local() {
    let v: NonZero<felt252> = match core::felt252_is_zero(16) {
        core::zeroable::IsZeroResult::Zero => None,
        core::zeroable::IsZeroResult::NonZero(x) => Some(x),
    }.unwrap();
    use_v(v);
    use_v(v);
}
