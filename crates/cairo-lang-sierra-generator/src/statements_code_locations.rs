use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

/// A full path to a cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceFileFullPath(pub String);

/// A location in a cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceCodeLocation {
    /// Line index, 0 based.
    pub line: usize,
    /// Character index inside the line, 0 based.
    pub col: usize,
}

/// A location in a cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceCodeSpan {
    /// Beginning of the text span in the cairo source file.
    pub start: SourceCodeLocation,
    /// End of the text span in the cairo source file, not included.
    pub end: SourceCodeLocation,
}

/// The mapping between sierra statement indexes and locations in cairo code
/// (if obtainable) which caused the statement to be generated. Contains the additional information
/// if the location is a part of a macro expansion.
///
/// Should be created using
/// [`crate::statements_locations::StatementsLocations::extract_statements_source_code_locations`].
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
