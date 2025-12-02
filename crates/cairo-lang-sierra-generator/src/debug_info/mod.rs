use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_filesystem::ids::FileLongId;
use salsa::Database;
use serde::{Deserialize, Serialize};

mod function_debug_info;
mod statements_locations;

pub use function_debug_info::serializable::{
    SerializableAllFunctionsDebugInfo, SerializableFunctionDebugInfo,
};
pub use function_debug_info::{AllFunctionsDebugInfo, FunctionDebugInfo};
pub use statements_locations::StatementsLocations;
pub use statements_locations::statements_code_locations::StatementsSourceCodeLocations;
pub use statements_locations::statements_functions::StatementsFunctions;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SierraProgramDebugInfo<'db> {
    pub statements_locations: StatementsLocations<'db>,
    pub functions_info: AllFunctionsDebugInfo<'db>,
}

/// A full path to a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SourceFileFullPath(pub String);

/// A location in a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SourceCodeLocation {
    /// Line index, 0 based.
    pub line: usize,
    /// Character index inside the line, 0 based.
    pub col: usize,
}

/// A location in a Cairo source file.
#[derive(Clone, Debug, Default, Eq, PartialEq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct SourceCodeSpan {
    /// Beginning of the text span in the Cairo source file.
    pub start: SourceCodeLocation,
    /// End of the text span in the Cairo source file, not included.
    pub end: SourceCodeLocation,
}

impl SourceCodeSpan {
    pub fn contains(&self, other: &Self) -> bool {
        self.start <= other.start && self.end >= other.end
    }
}

/// Returns a location in the user file corresponding to the given [StableLocation].
/// It consists of a full path to the file, a text span in the file and a boolean indicating
/// if the location is a part of a macro expansion.
fn maybe_code_location<'db>(
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
