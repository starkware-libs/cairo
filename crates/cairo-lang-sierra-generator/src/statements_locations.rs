use std::ops::Add;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::{FileId, FileLongId, VirtualFile};
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use cairo_lang_utils::LookupIntern;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use itertools::Itertools;

use crate::statements_code_locations::{
    SourceCodeLocation, SourceCodeSpan, SourceFileFullPath, StatementsSourceCodeLocations,
};
use crate::statements_functions::StatementsFunctions;

#[cfg(test)]
#[path = "statements_locations_test.rs"]
mod test;

/// Returns an identifier of the function that contains the given [StableLocation].
/// It is a fully qualified path to the function which contains:
/// - fully qualified path to the file module,
/// - relative path to the function in the file module.
pub fn maybe_containing_function_identifier(
    db: &dyn DefsGroup,
    location: StableLocation,
) -> Option<String> {
    let file_id = location.file_id(db.upcast());
    let absolute_semantic_path_to_file_module = file_module_absolute_identifier(db, file_id)?;

    let relative_semantic_path = function_identifier_relative_to_file_module(db, location);
    if relative_semantic_path.is_empty() {
        // In some cases the stable location maps to a code that is a statement like a function call
        // directly in a file module, e.g. `Self::eq(lhs, rhs)` in `core::traits`. This brings no
        // information about the function it was called from.
        None
    } else {
        Some(absolute_semantic_path_to_file_module.add("::").add(&relative_semantic_path))
    }
}

/// Returns an identifier of the function that contains the given [StableLocation].
/// It is a fully qualified path to the function which contains:
/// - fully qualified path to the file module,
/// - relative path to the function in the file module.
///
/// In case the fully qualified path to the file module cannot be found
/// it is replaced in the fully qualified function path by the file name.
pub fn maybe_containing_function_identifier_for_tests(
    db: &dyn DefsGroup,
    location: StableLocation,
) -> Option<String> {
    let file_id = location.file_id(db.upcast());
    let absolute_semantic_path_to_file_module = file_module_absolute_identifier(db, file_id)
        .unwrap_or_else(|| file_id.file_name(db.upcast()));

    let relative_semantic_path = function_identifier_relative_to_file_module(db, location);
    if relative_semantic_path.is_empty() {
        // In some cases the stable location maps to a code that is a statement like a function call
        // directly in a file module, e.g. `Self::eq(lhs, rhs)` in `core::traits`. This brings no
        // information about the function it was called from. It is especially relevant for corelib
        // tests where the first stable location may map to this kind of code.
        None
    } else {
        Some(absolute_semantic_path_to_file_module.add("::").add(&relative_semantic_path))
    }
}

/// Returns the path (modules and impls) to the function in the file.
/// The path is relative to the file module.
pub fn function_identifier_relative_to_file_module(
    db: &dyn DefsGroup,
    location: StableLocation,
) -> String {
    let syntax_db = db.upcast();
    let mut relative_semantic_path_segments: Vec<String> = vec![];
    let mut syntax_node = location.syntax_node(db);
    let mut statement_located_in_function = false;
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

                if relative_semantic_path_segments.is_empty() {
                    statement_located_in_function = true;
                }

                relative_semantic_path_segments.push(function_name.to_string());
            }
            cairo_lang_syntax::node::kind::SyntaxKind::ItemImpl => {
                let impl_name = cairo_lang_syntax::node::ast::ItemImpl::from_syntax_node(
                    syntax_db,
                    syntax_node.clone(),
                )
                .name(syntax_db)
                .text(syntax_db);
                relative_semantic_path_segments.push(impl_name.to_string());
            }
            cairo_lang_syntax::node::kind::SyntaxKind::ItemModule => {
                let module_name = cairo_lang_syntax::node::ast::ItemModule::from_syntax_node(
                    syntax_db,
                    syntax_node.clone(),
                )
                .name(syntax_db)
                .text(syntax_db);
                relative_semantic_path_segments.push(module_name.to_string());
            }
            _ => {}
        }
        if let Some(parent) = syntax_node.parent() {
            syntax_node = parent;
        } else {
            break;
        }
    }

    // If the statement is not located in a function, and it is located a generated file it is
    // probably located in a code block generated by an inline macro such as `array` or `panic`.
    let file_id = location.file_id(db.upcast());
    if !statement_located_in_function
        && matches!(
            file_id.lookup_intern(db),
            FileLongId::Virtual(VirtualFile { parent: Some(_), .. })
        )
    {
        relative_semantic_path_segments.insert(0, file_id.file_name(db.upcast()));
    }

    relative_semantic_path_segments.into_iter().rev().join("::")
}

/// Returns a location in the user file corresponding to the given [StableLocation].
/// It consists of a full path to the file, a text span in the file and a boolean indicating
/// if the location is a part of a macro expansion.
pub fn maybe_code_location(
    db: &dyn DefsGroup,
    location: StableLocation,
) -> Option<(SourceFileFullPath, SourceCodeSpan, bool)> {
    let is_macro = matches!(
        location.file_id(db).lookup_intern(db),
        FileLongId::Virtual(_) | FileLongId::External(_)
    );
    let location = location.diagnostic_location(db).user_location(db.upcast());
    let file_full_path = location.file_id.full_path(db.upcast());
    let position = location.span.position_in_file(db.upcast(), location.file_id)?;
    let source_location = SourceCodeSpan {
        start: SourceCodeLocation { col: position.start.col, line: position.start.line },
        end: SourceCodeLocation { col: position.end.col, line: position.end.line },
    };

    Some((SourceFileFullPath(file_full_path), source_location, is_macro))
}

/// This function returns a fully qualified path to the file module.
/// `None` should be returned only for compiler tests where files of type `VirtualFile` may be non
/// generated files.
pub fn file_module_absolute_identifier(db: &dyn DefsGroup, mut file_id: FileId) -> Option<String> {
    // `VirtualFile` is a generated file (e.g., by macros like `#[starknet::contract]`)
    // that won't have a matching file module in the db. Instead, we find its non generated parent
    // which is in the same module and have a matching file module in the db.
    while let FileLongId::Virtual(VirtualFile { parent: Some(parent), .. }) =
        file_id.lookup_intern(db)
    {
        file_id = parent;
    }

    let file_modules = db.file_modules(file_id).to_option()?;
    let full_path = file_modules.first().unwrap().full_path(db.upcast());

    Some(full_path)
}

/// The locations in the Cairo source code which caused a statement to be generated.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct StatementsLocations {
    pub locations: UnorderedHashMap<StatementIdx, Vec<StableLocation>>,
}

impl StatementsLocations {
    /// Creates a new [StatementsLocations] object from a list of [`Option<StableLocation>`].
    pub fn from_locations_vec(locations_vec: &[Vec<StableLocation>]) -> Self {
        let mut locations = UnorderedHashMap::default();
        for (idx, stmt_locations) in locations_vec.iter().enumerate() {
            if !stmt_locations.is_empty() {
                locations.insert(StatementIdx(idx), stmt_locations.clone());
            }
        }
        Self { locations }
    }
    /// Builds a map between each Sierra statement index and a string representation of the Cairo
    /// function that it was generated from. It is used for places
    /// without db access such as the profiler.
    // TODO(Gil): Add a db access to the profiler and remove this function.
    pub fn get_statements_functions_map_for_tests(
        &self,
        db: &dyn DefsGroup,
    ) -> UnorderedHashMap<StatementIdx, String> {
        self.locations
            .iter_sorted()
            .filter_map(|(statement_idx, stable_locations)| {
                maybe_containing_function_identifier_for_tests(
                    db,
                    *stable_locations.first().unwrap(),
                )
                .map(|function_identifier| (*statement_idx, function_identifier))
            })
            .collect()
    }

    /// Creates a new [StatementsFunctions] struct using [StatementsLocations] and [DefsGroup].
    pub fn extract_statements_functions(&self, db: &dyn DefsGroup) -> StatementsFunctions {
        StatementsFunctions {
            statements_to_functions_map: self
                .locations
                .iter_sorted()
                .map(|(statement_idx, stable_locations)| {
                    (
                        *statement_idx,
                        stable_locations
                            .iter()
                            .filter_map(|s| maybe_containing_function_identifier(db, *s))
                            .collect(),
                    )
                })
                .collect(),
        }
    }

    /// Creates a new [StatementsSourceCodeLocations] struct using [StatementsLocations] and
    /// [DefsGroup].
    pub fn extract_statements_source_code_locations(
        &self,
        db: &dyn DefsGroup,
    ) -> StatementsSourceCodeLocations {
        StatementsSourceCodeLocations {
            statements_to_code_location_map: self
                .locations
                .iter_sorted()
                .map(|(statement_idx, stable_locations)| {
                    (
                        *statement_idx,
                        stable_locations
                            .iter()
                            .filter_map(|s| maybe_code_location(db, *s))
                            .collect(),
                    )
                })
                .collect(),
        }
    }
}
