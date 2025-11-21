use super::VersionId;

#[test]
fn test_version_support() {
    let v131 = VersionId { major: 1, minor: 3, patch: 1 };
    let v140 = VersionId { major: 1, minor: 4, patch: 0 };
    let v141 = VersionId { major: 1, minor: 4, patch: 1 };
    let v150 = VersionId { major: 1, minor: 5, patch: 0 };
    let v151 = VersionId { major: 1, minor: 5, patch: 1 };
    let v200 = VersionId { major: 2, minor: 0, patch: 0 };
    assert!(!v131.supports(v140));
    assert!(v140.supports(v131));
    assert!(v140.supports(v140));
    assert!(v140.supports(v141));
    assert!(!v140.supports(v150));
    assert!(!v140.supports(v151));
    assert!(!v140.supports(v200));

    assert!(v141.supports(v131));
    assert!(v141.supports(v140));
    assert!(v141.supports(v141));
    assert!(!v141.supports(v150));
    assert!(!v141.supports(v151));
    assert!(!v141.supports(v200));

    assert!(v150.supports(v141));
    assert!(v150.supports(v150));
    assert!(v150.supports(v151));
    assert!(!v150.supports(v200));

    assert!(v151.supports(v141));
    assert!(v151.supports(v150));
    assert!(v151.supports(v151));
    assert!(!v151.supports(v200));

    assert!(v200.supports(v151));
    assert!(v200.supports(v200));
}
