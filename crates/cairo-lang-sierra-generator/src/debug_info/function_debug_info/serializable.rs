use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::ids::{FunctionId, VarId};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

use crate::debug_info::{SourceCodeSpan, SourceFileFullPath};

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
    /// Mapping from a cairo variable - its name and definition span - to a vector
    /// with sierra variables which values correspond to values of the cairo variable during
    /// execution of the function code.
    pub cairo_variable_to_sierra_variables:
        HashMap<(CairoVariableName, SourceCodeSpan), Vec<VarId>>,
}

pub type CairoVariableName = String;
