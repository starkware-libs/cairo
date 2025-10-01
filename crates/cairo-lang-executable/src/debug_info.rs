use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

/// Debug information for an Executable.
#[derive(Clone, Debug, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct DebugInfo {
    /// Non-crucial information about the program, for use by external libraries and tools.
    ///
    /// See [`Annotations`] type documentation for more information about this field.
    #[serde(default, skip_serializing_if = "Annotations::is_empty")]
    pub annotations: Annotations,
}

/// Store for non-crucial information about the program, for use by external libraries and tools.
///
/// Keys represent tool namespaces, and values are tool-specific annotations themselves.
/// Annotation values are JSON values, so they can be arbitrarily complex.
///
/// ## Namespaces
///
/// In order to avoid collisions between tools, namespaces should be URL-like, contain tool name.
/// It is not required for namespace URLs to exist, but it is preferable nonetheless.
///
/// A single tool might want to use multiple namespaces, for example to group together annotations
/// coming from different subcomponents of the tool. In such case, namespaces should use path-like
/// notation (e.g. `example.com/sub-namespace`).
///
/// For future-proofing, it might be a good idea to version namespaces, e.g. `example.com/v1`.
///
/// ### Example well-formed namespaces
///
/// - `scarb.swmansion.com`
/// - `scarb.swmansion.com/v1`
/// - `scarb.swmansion.com/build-info/v1`
pub type Annotations = OrderedHashMap<String, serde_json::Value>;

/// Program offsets information, for use by the profiler.
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProgramInformation {
    /// The bytecode offset of the first CASM instruction after all added headers.
    /// It is the offset of the first CASM instruction of the original CASM program (the one that
    /// is a direct result of Sierra compilation).
    pub program_offset: usize,
}

impl From<ProgramInformation> for Annotations {
    fn from(value: ProgramInformation) -> Self {
        let mapping = serde_json::to_value(value).unwrap();
        OrderedHashMap::from([(
            "github.com/software-mansion/cairo-profiler".to_string(),
            serde_json::Value::from_iter([("program_info", mapping)]),
        )])
    }
}
