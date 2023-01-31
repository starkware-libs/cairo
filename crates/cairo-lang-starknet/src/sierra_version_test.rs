use std::collections::HashSet;

use serde::Serialize;
use smol_str::SmolStr;

use super::SierraVersion;

#[test]
fn test_serialize() {
    let sierra_version = SierraVersion {
        version_id: 1,
        allowed_libfuncs: HashSet::from_iter(
            ["libfunc_A", "libfunc_B", "libfunc_C"].iter().map(|s| s.into()),
        ),
    };
    println!("{}", serde_json::to_string(&sierra_version).unwrap());
}
