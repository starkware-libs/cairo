use std::collections::HashMap;

use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use serde::{Deserialize, Serialize};

pub type FilePath = String; 

pub type LineNumber = u32;

/// The mapping from sierra statement index to fully qualified Cairo path of the Cairo function TODO
/// (if obtainable) which caused the statement to be generated. Should be created using
/// [`crate::statements_locations::StatementsLocations::extract_statements_functions`].
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct StatementsLines {
    pub statements_to_lines_map: HashMap<StatementIdx, Vec<(FilePath, LineNumber)>>,
}

impl From<StatementsLines> for Annotations {
    fn from(value: StatementsLines) -> Self {
        let mapping = serde_json::to_value(value.statements_to_lines_map).unwrap();
        OrderedHashMap::from([(
            "github.com/software-mansion/cairo-coverage".to_string(),
            serde_json::Value::from_iter([("statements_lines", mapping)]),
        )])
    }
}
