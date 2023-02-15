use hash::LegacyHash;

#[test]
fn test_bug_test() {
    let a = 1;
    let b = 2;
    let mut c = 0;
    if a < b {
        c = LegacyHash::hash(a, b);
    } else {
        c = LegacyHash::hash(b, a);
    }
}
