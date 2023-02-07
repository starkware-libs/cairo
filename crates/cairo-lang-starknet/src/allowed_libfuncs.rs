use std::collections::HashSet;

use cairo_lang_sierra::ids::GenericLibfuncId;
use serde::Deserialize;
use smol_str::SmolStr;
use thiserror::Error;

#[derive(Error, Debug, Eq, PartialEq)]
pub enum AllowedLibfuncsError {
    #[error("Invalid Sierra program.")]
    SierraProgramError,
    #[error("No libfunc list named '{allowed_libfuncs_list_name}' is known.")]
    UnexpectedAllowedLibfuncsList { allowed_libfuncs_list_name: String },
    #[error(
        "Libfunc {invalid_libfunc} is not allowed in the libfuncs list \
         '{allowed_libfuncs_list_name}'."
    )]
    UnsupportedLibfunc { invalid_libfunc: String, allowed_libfuncs_list_name: String },
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
/// The experimental list contains all the libfuncs and is currently used for development.
pub const DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST: &str = "experimental_v0.1.0";

/// Returns the sierra version corresponding to the given version id.
pub fn lookup_allowed_libfuncs_list(
    list_name: &str,
) -> Result<AllowedLibfuncs, AllowedLibfuncsError> {
    let allowed_libfuncs_str: &str = match list_name {
        DEFAULT_EXPERIMENTAL_LIBFUNCS_LIST => {
            include_str!("allowed_libfuncs_lists/experimental_v0.1.0.json")
        }
        DEFAULT_AUDITED_LIBFUNCS_LIST => {
            include_str!("allowed_libfuncs_lists/audited_v0.1.0.json")
        }
        _ => {
            return Err(AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
                allowed_libfuncs_list_name: list_name.to_string(),
            });
        }
    };
    let allowed_libfuncs: Result<AllowedLibfuncs, serde_json::Error> =
        serde_json::from_str(allowed_libfuncs_str);
    allowed_libfuncs.map_err(|_| AllowedLibfuncsError::UnexpectedAllowedLibfuncsList {
        allowed_libfuncs_list_name: list_name.to_string(),
    })
}
