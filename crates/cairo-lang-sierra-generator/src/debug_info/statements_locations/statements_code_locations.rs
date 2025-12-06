use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

use crate::debug_info::{SourceCodeSpan, SourceFileFullPath};

/// The mapping between Sierra statement indexes and locations in Cairo code
/// (if obtainable) which caused the statement to be generated. Contains the additional information
/// if the location is a part of a macro expansion.
///
/// Should be created using
/// [`crate::debug_info::StatementsLocations::extract_statements_source_code_locations`].
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct StatementsSourceCodeLocations {
    pub statements_to_code_location_map:
        HashMap<StatementIdx, Vec<(SourceFileFullPath, SourceCodeSpan, bool)>>,
}

impl From<StatementsSourceCodeLocations> for Annotations {
    fn from(value: StatementsSourceCodeLocations) -> Self {
        let mapping = serde_json::to_value(value.statements_to_code_location_map).unwrap();
        OrderedHashMap::from([(
            "github.com/software-mansion/cairo-coverage".to_string(),
            serde_json::Value::from_iter([("statements_code_locations", mapping)]),
        )])
    }
}
