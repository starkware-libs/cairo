use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

const CAIRO_PROFILER_ANNOTATION: &str = "github.com/software-mansion/cairo-profiler";

/// The mapping from sierra statement index to fully qualified Cairo path of the Cairo function
/// (if obtainable) which caused the statement to be generated. Should be created using
/// [`crate::statements_locations::StatementsLocations::extract_statements_functions`].
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct StatementsFunctions {
    pub statements_to_functions_map: HashMap<StatementIdx, Vec<String>>,
}

impl StatementsFunctions {
    pub fn from_annotations(annotations: &Annotations) -> Option<Self> {
        Some(Self {
            statements_to_functions_map: annotations.get(CAIRO_PROFILER_ANNOTATION).and_then(
                |value| {
                    let inner_value = value.as_object()?.get("statements_functions")?;

                    serde_json::from_value(inner_value.clone()).ok()
                },
            )?,
        })
    }
}

impl From<StatementsFunctions> for Annotations {
    fn from(value: StatementsFunctions) -> Self {
        let mapping = serde_json::to_value(value.statements_to_functions_map).unwrap();
        OrderedHashMap::from([(
            CAIRO_PROFILER_ANNOTATION.into(),
            serde_json::Value::from_iter([("statements_functions", mapping)]),
        )])
    }
}
