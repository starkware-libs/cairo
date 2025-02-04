use std::collections::{BTreeSet, HashSet};

use cairo_lang_sierra::extensions::GenericLibfunc;
use cairo_lang_sierra::extensions::core::CoreLibfunc;

use super::{
    BUILTIN_ALL_LIBFUNCS_LIST, BUILTIN_AUDITED_LIBFUNCS_LIST, BUILTIN_EXPERIMENTAL_LIBFUNCS_LIST,
    ListSelector, lookup_allowed_libfuncs_list,
};

#[test]
fn all_list_includes_all_supported() {
    let blocked_libfuncs = ["print", "cheatcode", "get_available_gas", "get_unspent_gas"];
    pretty_assertions::assert_eq!(
        lookup_allowed_libfuncs_list(ListSelector::ListName(BUILTIN_ALL_LIBFUNCS_LIST.to_string()))
            .unwrap()
            .allowed_libfuncs
            .into_iter()
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
    let supported_ids = CoreLibfunc::supported_ids().into_iter().collect::<HashSet<_>>();
    for list_name in [
        BUILTIN_ALL_LIBFUNCS_LIST,
        BUILTIN_AUDITED_LIBFUNCS_LIST,
        BUILTIN_EXPERIMENTAL_LIBFUNCS_LIST,
    ] {
        let allowed_libfuncs =
            lookup_allowed_libfuncs_list(ListSelector::ListName(list_name.to_string())).unwrap();
        for libfunc_id in allowed_libfuncs.allowed_libfuncs {
            assert!(
                supported_ids.contains(&libfunc_id),
                "libfunc {libfunc_id} from list {list_name} is not supported."
            );
        }
    }
}
