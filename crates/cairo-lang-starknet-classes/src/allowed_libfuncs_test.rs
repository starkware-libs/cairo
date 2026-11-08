use std::collections::BTreeSet;

use cairo_lang_sierra::extensions::GenericLibfunc;
use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::ids::GenericLibfuncId;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;

use super::{
    AllowedLibfuncs, AllowedLibfuncsError, AllowedVersions, BUILTIN_ALL_LIBFUNCS_LIST,
    BUILTIN_AUDITED_LIBFUNCS_LIST, ListSelector, lookup_allowed_libfuncs_list,
};
use crate::compiler_version::VersionId;

#[test]
fn all_list_includes_all_supported() {
    let blocked_libfuncs =
        ["print", "cheatcode", "get_available_gas", "get_unspent_gas", "dummy_function_call"];
    pretty_assertions::assert_eq!(
        lookup_allowed_libfuncs_list(ListSelector::ListName(BUILTIN_ALL_LIBFUNCS_LIST.to_string()))
            .unwrap()
            .allowed_libfuncs
            .into_keys()
            .map(|id| id.0)
            .collect::<BTreeSet<_>>(),
        CoreLibfunc::supported_ids()
            .into_iter()
            .map(|id| id.0)
            .filter(|id| !blocked_libfuncs.contains(&id.as_str()))
            .collect()
    );
}

#[test]
fn libfunc_lists_include_only_supported_libfuncs() {
    let supported_ids = CoreLibfunc::supported_ids().into_iter().collect::<UnorderedHashSet<_>>();
    for list_name in [BUILTIN_ALL_LIBFUNCS_LIST, BUILTIN_AUDITED_LIBFUNCS_LIST] {
        let allowed_libfuncs =
            lookup_allowed_libfuncs_list(ListSelector::ListName(list_name.to_string())).unwrap();
        for (libfunc_id, _) in allowed_libfuncs.allowed_libfuncs {
            assert!(
                supported_ids.contains(&libfunc_id),
                "libfunc {libfunc_id} from list {list_name} is not supported."
            );
        }
    }
}

#[test]
fn allowed_versions_representations() {
    let v = |major, minor| VersionId { major, minor, patch: 0 };
    let list: AllowedLibfuncs = serde_json::from_str(
        r#"{ "allowed_libfuncs": {
            "always": null,
            "stable": { "major": 1, "minor": 8, "patch": 0 },
            "deprecated": { "deprecated": { "major": 1, "minor": 10, "patch": 0 } },
            "range": { "stable": { "major": 1, "minor": 8, "patch": 0 },
                       "deprecated": { "major": 1, "minor": 10, "patch": 0 } }
        } }"#,
    )
    .unwrap();
    let get = |name: &str| list.allowed_libfuncs[&GenericLibfuncId::from_string(name)];
    assert_eq!(get("always"), AllowedVersions { stable: None, deprecated: None });
    assert_eq!(get("stable"), AllowedVersions { stable: Some(v(1, 8)), deprecated: None });
    assert_eq!(get("deprecated"), AllowedVersions { stable: None, deprecated: Some(v(1, 10)) });
    assert_eq!(get("range"), AllowedVersions { stable: Some(v(1, 8)), deprecated: Some(v(1, 10)) });

    let libfunc = GenericLibfuncId::from_string("range");
    let range = get("range");
    assert_eq!(
        range.validate(&libfunc, v(1, 7)),
        Err(AllowedLibfuncsError::UnsupportedLibfuncAtVersion {
            invalid_libfunc: "range".into(),
            required_version: v(1, 8),
            class_version: v(1, 7),
        })
    );
    assert_eq!(range.validate(&libfunc, v(1, 8)), Ok(()));
    assert_eq!(range.validate(&libfunc, v(1, 9)), Ok(()));
    assert_eq!(
        range.validate(&libfunc, v(1, 10)),
        Err(AllowedLibfuncsError::DeprecatedLibfuncAtVersion {
            invalid_libfunc: "range".into(),
            deprecated_version: v(1, 10),
            class_version: v(1, 10),
        })
    );

    assert!(
        serde_json::from_str::<AllowedLibfuncs>(
            r#"{ "allowed_libfuncs": { "unknown": { "until": null } } }"#
        )
        .is_err()
    );
}
