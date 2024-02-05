use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::fs;

use cairo_lang_sierra::ids::GenericLibfuncId;
use serde::Deserialize;
use smol_str::SmolStr;
use thiserror::Error;

#[cfg(test)]
#[path = "allowed_libfuncs_test.rs"]
mod test;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AllowedLibfuncsError {
    #[error("Invalid Sierra program.")]
    SierraProgramError,
    #[error("No libfunc list named '{allowed_libfuncs_list_name}' is known.")]
    UnexpectedAllowedLibfuncsList { allowed_libfuncs_list_name: String },
    #[error("The allowed libfuncs file '{allowed_libfuncs_list_file}' was not found.")]
    UnknownAllowedLibfuncsFile { allowed_libfuncs_list_file: String },
    #[error("Failed to deserialize the allowed libfuncs file '{allowed_libfuncs_list_file}'.")]
    DeserializationError { allowed_libfuncs_list_file: String },
    #[error(
        "Libfunc {invalid_libfunc} is not allowed in the libfuncs list \
         '{allowed_libfuncs_list_name}'.\n Run with '--allowed-libfuncs-list-name \
         {BUILTIN_ALL_LIBFUNCS_LIST}' to allow all libfuncs."
    )]
    UnsupportedLibfunc { invalid_libfunc: String, allowed_libfuncs_list_name: String },
}

/// A selector for the allowed libfunc list.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum ListSelector {
    /// A list with one of the predefined names.
    ListName(String),
    /// A list to be read from a file.
    ListFile(String),
    #[default]
    DefaultList,
}

impl ListSelector {
    /// Gets the cli arguments of both the list name and list file and return a selector, or None if
    /// both were supplied.
    pub fn new(list_name: Option<String>, list_file: Option<String>) -> Option<Self> {
        match (list_name, list_file) {
            // Both options supplied, can't decide.
            (Some(_), Some(_)) => None,
            (Some(list_name), None) => Some(Self::ListName(list_name)),
            (None, Some(list_file)) => Some(Self::ListFile(list_file)),
            (None, None) => Some(Self::default()),
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
/// Should only contain libfuncs that are audited and tested.
pub const BUILTIN_AUDITED_LIBFUNCS_LIST: &str = "audited";
/// The allowed libfuncs list to use allowed on testnet2 - should be all libfuncs currently
/// supported by starknet.
pub const BUILTIN_EXPERIMENTAL_LIBFUNCS_LIST: &str = "experimental";
/// The experimental list contains all the libfuncs and is currently used for development.
pub const BUILTIN_ALL_LIBFUNCS_LIST: &str = "all";

/// Returns the sierra version corresponding to the given version id.
pub fn lookup_allowed_libfuncs_list(
    list_selector: ListSelector,
) -> Result<AllowedLibfuncs, AllowedLibfuncsError> {
    let list_name = list_selector.to_string();
    let allowed_libfuncs_str: String = match list_selector {
        ListSelector::ListName(list_name) => match list_name.as_str() {
            BUILTIN_ALL_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/all.json").to_string()
            }
            BUILTIN_EXPERIMENTAL_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/experimental.json").to_string()
            }
            BUILTIN_AUDITED_LIBFUNCS_LIST => {
                include_str!("allowed_libfuncs_lists/audited.json").to_string()
            }
            _ => {
                return Err(AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
                    allowed_libfuncs_list_name: list_name.to_string(),
                });
            }
        },
        ListSelector::ListFile(file_path) => fs::read_to_string(&file_path).map_err(|_| {
            AllowedLibfuncsError::UnknownAllowedLibfuncsFile {
                allowed_libfuncs_list_file: file_path,
            }
        })?,
        ListSelector::DefaultList => {
            include_str!("allowed_libfuncs_lists/audited.json").to_string()
        }
    };
    let allowed_libfuncs: Result<AllowedLibfuncs, serde_json::Error> =
        serde_json::from_str(&allowed_libfuncs_str);
    allowed_libfuncs.map_err(|_| AllowedLibfuncsError::DeserializationError {
        allowed_libfuncs_list_file: list_name,
    })
}
