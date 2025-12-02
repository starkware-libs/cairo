use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::ids::{FunctionId, VarId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

use crate::debug_info::{SourceCodeSpan, SourceFileFullPath};

/// The serializable debug info of all sierra functions in the program.
pub struct SerializableAllFunctionsDebugInfo(
    pub(super) HashMap<FunctionId, SerializableFunctionDebugInfo>,
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

/// The serializable debug info of a sierra function.
#[derive(Serialize, Deserialize)]
pub struct SerializableFunctionDebugInfo {
    /// Path to the user file the function comes from.
    pub function_file_path: SourceFileFullPath,
    /// Span of the function in the user file it comes from.
    pub function_code_span: SourceCodeSpan,
    /// Mapping from a sierra variable to a cairo variable (its name and definition span).
    /// The sierra variable value corresponds to the cairo variable value at some point during
    /// execution of the function code.
    pub sierra_to_cairo_variable: HashMap<VarId, (CairoVariableName, SourceCodeSpan)>,
}

pub type CairoVariableName = String;
