use std::collections::BTreeSet;

use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::GenericLibfunc;

use super::{lookup_allowed_libfuncs_list, DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST};

#[test]
fn experimental_list_includes_all() {
    let blocked_libfuncs = [
        "print",
        "set_block_number",
        "set_block_timestamp",
        "set_caller_address",
        "set_contract_address",
        "set_sequencer_address",
        "get_available_gas",
    ];
    pretty_assertions::assert_eq!(
        lookup_allowed_libfuncs_list(super::ListSelector::ListName(
            DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST.to_string()
        ))
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
