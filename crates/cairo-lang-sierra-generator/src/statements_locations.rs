use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;

use crate::db::SierraGenGroup;
#[cfg(test)]
#[path = "statements_locations_test.rs"]
mod test;

/// Returns an identifier of the function that contains the given [StableLocation]. It is composed
/// of the file name, and the path (modules and impls) to the function in the file.
pub fn containing_function_identifier(
    db: &dyn SierraGenGroup,
    location: Option<StableLocation>,
) -> String {
    match location {
        Some(location) => {
            let syntax_db = db.upcast();
            let mut result: Vec<String> = vec![];
            let mut syntax_node = location.syntax_node(db.upcast());
            loop {
                // TODO(Gil): Extract this function into a trait of syntax kind to support future
                // function containing items (specifically trait functions).
                match syntax_node.kind(syntax_db) {
                    cairo_lang_syntax::node::kind::SyntaxKind::FunctionWithBody => {
                        let function_name =
                            cairo_lang_syntax::node::ast::FunctionWithBody::from_syntax_node(
                                syntax_db,
                                syntax_node.clone(),
                            )
                            .declaration(syntax_db)
                            .name(syntax_db)
                            .text(syntax_db);
                        result.push(function_name.to_string());
                    }
                    cairo_lang_syntax::node::kind::SyntaxKind::ItemImpl => {
                        let impl_name = cairo_lang_syntax::node::ast::ItemImpl::from_syntax_node(
                            syntax_db,
                            syntax_node.clone(),
                        )
                        .name(syntax_db)
                        .text(syntax_db);
                        result.push(impl_name.to_string());
                    }
                    cairo_lang_syntax::node::kind::SyntaxKind::ItemModule => {
                        let module_name =
                            cairo_lang_syntax::node::ast::ItemModule::from_syntax_node(
                                syntax_db,
                                syntax_node.clone(),
                            )
                            .name(syntax_db)
                            .text(syntax_db);
                        result.push(module_name.to_string());
                    }
                    _ => {}
                }
                if let Some(parent) = syntax_node.parent() {
                    syntax_node = parent;
                } else {
                    break;
                }
            }
            let file_name = location.file_id(db.upcast()).file_name(db.upcast());
            result.push(file_name);
            result.iter().rev().join("::")
        }
        None => "unknown".to_string(),
    }
}

/// The location of the Cairo source code which caused a statement to be generated.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct StatementsLocations {
    pub locations: UnorderedHashMap<StatementIdx, StableLocation>,
}

impl StatementsLocations {
    /// Creates a new [StatementsLocations] object from a list of [`Option<StableLocation>`].
    pub fn from_locations_vec(locations_vec: &[Option<StableLocation>]) -> Self {
        let mut locations = UnorderedHashMap::default();
        for (idx, location) in locations_vec.iter().enumerate() {
            if let Some(location) = location {
                locations.insert(StatementIdx(idx), *location);
            }
        }
        Self { locations }
    }
    /// Builds a map between each Sierra statement index and a string representation of the Cairo
    /// function that it was generated from. The function representation is composed of the function
    /// name and the path (modules and impls) to the function in the file. It is used for places
    /// without db access such as the profiler.
    // TODO(Gil): Add a db access to the profiler and remove this function.
    pub fn get_statements_functions_map(
        &self,
        db: &dyn SierraGenGroup,
    ) -> UnorderedHashMap<StatementIdx, String> {
        self.locations.map(|s| containing_function_identifier(db, Some(*s)))
    }
}
