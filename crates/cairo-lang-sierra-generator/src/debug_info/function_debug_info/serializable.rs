use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

use crate::debug_info::{SourceCodeSpan, SourceFileFullPath};

/// The serializable debug info of all Sierra functions in the program.
pub struct SerializableAllFunctionsDebugInfo(
    pub(super) HashMap<SierraFunctionId, SerializableFunctionDebugInfo>,
);

impl From<SerializableAllFunctionsDebugInfo> for Annotations {
    fn from(value: SerializableAllFunctionsDebugInfo) -> Self {
        let mapping = serde_json::to_value(value.0).unwrap();
        OrderedHashMap::from([(
            "github.com/software-mansion-labs/cairo-debugger".to_string(),
            serde_json::Value::from_iter([("functions_info", mapping)]),
        )])
    }
}

/// The serializable debug info of a Sierra function.
#[derive(Serialize, Deserialize)]
pub struct SerializableFunctionDebugInfo {
    /// Path to the user file the function comes from.
    pub function_file_path: SourceFileFullPath,
    /// Span of the function in the user file it comes from.
    pub function_code_span: SourceCodeSpan,
    /// Mapping from a Sierra variable to a cairo variable (its name and definition span).
    /// The Sierra variable value corresponds to the cairo variable value at some point during
    /// execution of the function code.
    pub sierra_to_cairo_variable: HashMap<SierraVarId, (CairoVariableName, SourceCodeSpan)>,
}

/// An id of a Sierra function - equivalent of `id` field of [`cairo_lang_sierra::ids::FunctionId`].
/// Used to make serialization of a hashmap with id as a key possible.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SierraFunctionId(pub u64);

/// An id of a Sierra variable - equivalent of `id` field of [`cairo_lang_sierra::ids::VarId`].
/// Used to make serialization of a hashmap with id as a key possible.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SierraVarId(pub u64);

pub type CairoVariableName = String;
