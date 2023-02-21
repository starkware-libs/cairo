use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::fs;

use cairo_lang_sierra::ids::GenericLibfuncId;
use serde::Deserialize;
use smol_str::SmolStr;
use thiserror::Error;

use crate::contract_class::ContractClass;
use crate::felt_serde::sierra_from_felts;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AllowedLibfuncsError {
    #[error("Invalid Sierra program.")]
    SierraProgramError,
    #[error("No libfunc list named '{allowed_libfuncs_list_name}' is known.")]
    UnexpectedAllowedLibfuncsList { allowed_libfuncs_list_name: String },
    #[error(
        "Libfunc {invalid_libfunc} is not allowed in the libfuncs list \
         '{allowed_libfuncs_list_name}'.\n Run with '--allowed-libfuncs-list-name \
         {DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST}' to allow all libfuncs."
    )]
    UnsupportedLibfunc { invalid_libfunc: String, allowed_libfuncs_list_name: String },
}

/// A selector for the allowed libfunc list.
pub enum ListSelector {
    /// A list with one of the predfined names.
    ListName(String),
    /// A list to be read from a file.
    ListFile(String),
    DefaultList,
}

impl ListSelector {
    /// Gets the cli arguments of both the list name and list file and return a selector, or None if
    /// both were supplied.
    pub fn new(list_name: Option<String>, list_file: Option<String>) -> Option<ListSelector> {
        match (list_name, list_file) {
            // Both options supplied, can't decide.
            (Some(_), Some(_)) => None,
            (Some(list_name), None) => Some(ListSelector::ListName(list_name)),
            (None, Some(list_file)) => Some(ListSelector::ListFile(list_file)),
            (None, None) => Some(ListSelector::DefaultList),
        }
    }
}

impl Display for ListSelector {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ListSelector::ListName(s) => write!(f, "{s}"),
            ListSelector::ListFile(s) => write!(f, "{s}"),
            ListSelector::DefaultList => write!(f, "Default libfunc list"),
        }
    }
}

/// Represents a list of allowed sierra libfuncs.
#[derive(Debug, PartialEq, Eq, Deserialize)]
pub struct AllowedLibfuncs {
    #[serde(deserialize_with = "deserialize_libfuncs_set::<_>")]
    pub allowed_libfuncs: HashSet<GenericLibfuncId>,
}

fn deserialize_libfuncs_set<'de, D: serde::Deserializer<'de>>(
    deserializer: D,
) -> Result<HashSet<GenericLibfuncId>, D::Error> {
    Ok(HashSet::from_iter(
        Vec::<SmolStr>::deserialize(deserializer)?.into_iter().map(GenericLibfuncId::from_string),
    ))
}

/// The allowed libfuncs list to use if no list is supplied to the compiler.
pub const DEFAULT_AUDITED_LIBFUNCS_LIST: &str = "audited_v0.1.0";
/// The allowed libfuncs list to use allowed on testnet.
pub const DEFAULT_TESTNET_LIBFUNCS_LIST: &str = "testnet_v0.1.0";
/// The experimental list contains all the libfuncs and is currently used for development.
pub const DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST: &str = "experimental_v0.1.0";

/// Returns the sierra version corresponding to the given version id.
pub fn lookup_allowed_libfuncs_list(
    list_selector: ListSelector,
) -> Result<AllowedLibfuncs, AllowedLibfuncsError> {
    let list_name = list_selector.to_string();
    let allowed_libfuncs_str: String = match list_selector {
        ListSelector::ListName(list_name) => match list_name.as_str() {
            DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/experimental_v0.1.0.json").to_string()
            }
            DEFAULT_TESTNET_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/testnet_v0.1.0.json").to_string()
            }
            DEFAULT_AUDITED_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/audited_v0.1.0.json").to_string()
            }
            _ => {
                return Err(AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
                    allowed_libfuncs_list_name: list_name.to_string(),
                });
            }
        },
        ListSelector::ListFile(file_path) => fs::read_to_string(&file_path).map_err(|_| {
            AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
                allowed_libfuncs_list_name: file_path,
            }
        })?,
        ListSelector::DefaultList => {
            include_str!("allowed_libfuncs_lists/testnet_v0.1.0.json").to_string()
        }
    };
    let allowed_libfuncs: Result<AllowedLibfuncs, serde_json::Error> =
        serde_json::from_str(&allowed_libfuncs_str);
    allowed_libfuncs.map_err(|_| AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
        allowed_libfuncs_list_name: list_name,
    })
}

/// Checks that all the used libfuncs in the contract class are allowed in the contract class
/// sierra version.
pub fn validate_compatible_sierra_version(
    contract: &ContractClass,
    list_selector: ListSelector,
) -> Result<(), AllowedLibfuncsError> {
    let list_name = list_selector.to_string();
    let allowed_libfuncs = lookup_allowed_libfuncs_list(list_selector)?;
    let (_, sierra_program) = sierra_from_felts(&contract.sierra_program)
        .map_err(|_| AllowedLibfuncsError::SierraProgramError)?;
    for libfunc in sierra_program.libfunc_declarations.iter() {
        if !allowed_libfuncs.allowed_libfuncs.contains(&libfunc.long_id.generic_id) {
            return Err(AllowedLibfuncsError::UnsupportedLibfunc {
                invalid_libfunc: libfunc.long_id.generic_id.to_string(),
                allowed_libfuncs_list_name: list_name,
            });
        }
    }
    Ok(())
}
