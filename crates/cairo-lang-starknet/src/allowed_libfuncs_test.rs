use std::collections::{BTreeSet, HashSet};

use cairo_lang_sierra::extensions::core::CoreLibfunc;
use cairo_lang_sierra::extensions::GenericLibfunc;

use super::{
    lookup_allowed_libfuncs_list, ListSelector, DEFAULT_AUDITED_LIBFUNCS_LIST,
    DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST, DEFAULT_TESTNET_LIBFUNCS_LIST,
};

#[test]
fn experimental_list_includes_all() {
    let blocked_libfuncs = [
        "print",
        "set_block_number",
        "set_block_timestamp",
        "set_caller_address",
        "set_contract_address",
        "set_sequencer_address",
        "set_version",
        "set_account_contract_address",
        "set_max_fee",
        "set_transaction_hash",
        "set_chain_id",
        "set_nonce",
        "set_signature",
        "get_available_gas",
    ];
    pretty_assertions::assert_eq!(
        lookup_allowed_libfuncs_list(ListSelector::ListName(
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

#[test]
fn allowed_lists_include_only_valid_libfuncs() {
    let supported_ids = CoreLibfunc::supported_ids().into_iter().collect::<HashSet<_>>();
    for list_name in [
        DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST,
        DEFAULT_AUDITED_LIBFUNCS_LIST,
        DEFAULT_TESTNET_LIBFUNCS_LIST,
    ] {
        let allowed_libfuncs =
            lookup_allowed_libfuncs_list(ListSelector::ListName(list_name.to_string())).unwrap();
        for libfunc_id in allowed_libfuncs.allowed_libfuncs {
            assert!(
                supported_ids.contains(&libfunc_id),
                "libfunc {libfunc_id} is missing on list {list_name}."
            );
        }
    }
}
