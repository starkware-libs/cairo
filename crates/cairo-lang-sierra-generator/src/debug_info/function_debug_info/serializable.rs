#![expect(clippy::disallowed_types)]

use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

use crate::debug_info::{SourceCodeSpan, SourceFileFullPath};

/// The serializable debug info of all sierra functions in the program.
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

/// The serializable debug info of a sierra function.
#[derive(Serialize, Deserialize)]
pub struct SerializableFunctionDebugInfo {
    /// Path to the user file the function comes from.
    pub function_file_path: SourceFileFullPath,
    /// Span of the function in the user file it comes from.
    pub function_code_span: SourceCodeSpan,
    /// Mapping from a sierra variable to the list of cairo variables (their names and definition
    /// spans) that the sierra variable's value corresponds to at some point during execution of
    /// the function code.
    pub sierra_to_cairo_variables: HashMap<SierraVarId, Vec<(CairoVariableName, SourceCodeSpan)>>,
    /// Debug info of the function's parameters, in declaration order.
    pub parameters: Vec<SerializableParameterInfo>,
}

/// The serializable debug info of a single function parameter.
#[derive(Serialize, Deserialize)]
pub struct SerializableParameterInfo {
    /// The sierra variable id assigned to this parameter.
    pub sierra_var_id: SierraVarId,
    /// The name of the parameter in the source code.
    pub name: CairoVariableName,
    /// The span of the parameter name in the source code.
    pub definition_span: SourceCodeSpan,
}

/// An id of a sierra function - equivalent of `id` field of [`cairo_lang_sierra::ids::FunctionId`].
/// Used to make serialization of a hashmap with id as a key possible.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SierraFunctionId(pub u64);

/// An id of a sierra variable - equivalent of `id` field of [`cairo_lang_sierra::ids::VarId`].
/// Used to make serialization of a hashmap with id as a key possible.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct SierraVarId(pub u64);

pub type CairoVariableName = String;
