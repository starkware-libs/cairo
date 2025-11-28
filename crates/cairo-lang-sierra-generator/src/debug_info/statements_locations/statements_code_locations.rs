use std::collections::HashMap;

use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_filesystem::ids::FileLongId;
use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;
use serde::{Deserialize, Serialize};

/// A full path to a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceFileFullPath(pub String);

/// A location in a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceCodeLocation {
    /// Line index, 0 based.
    pub line: usize,
    /// Character index inside the line, 0 based.
    pub col: usize,
}

/// A location in a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceCodeSpan {
    /// Beginning of the text span in the Cairo source file.
    pub start: SourceCodeLocation,
    /// End of the text span in the Cairo source file, not included.
    pub end: SourceCodeLocation,
}

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

/// Returns a location in the user file corresponding to the given [StableLocation].
/// It consists of a full path to the file, a text span in the file and a boolean indicating
/// if the location is a part of a macro expansion.
pub fn maybe_code_location<'db>(
    db: &'db dyn Database,
    location: StableLocation<'db>,
) -> Option<(SourceFileFullPath, SourceCodeSpan, bool)> {
    let is_macro =
        matches!(location.file_id(db).long(db), FileLongId::Virtual(_) | FileLongId::External(_));
    let location = location.span_in_file(db).user_location(db);
    let file_full_path = location.file_id.full_path(db);
    let position = location.span.position_in_file(db, location.file_id)?;
    let source_location = SourceCodeSpan {
        start: SourceCodeLocation { col: position.start.col, line: position.start.line },
        end: SourceCodeLocation { col: position.end.col, line: position.end.line },
    };

    Some((SourceFileFullPath(file_full_path), source_location, is_macro))
}
