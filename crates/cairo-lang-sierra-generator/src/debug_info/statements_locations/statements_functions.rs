use std::collections::HashMap;
use std::ops::Add;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_diagnostics::ToOption;
use cairo_lang_filesystem::ids::{FileId, FileLongId, VirtualFile};
use cairo_lang_sierra::debug_info::Annotations;
use cairo_lang_sierra::program::StatementIdx;
use cairo_lang_syntax::node::ast::FunctionWithBody;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use itertools::Itertools;
use salsa::Database;
use serde::{Deserialize, Serialize};

/// The mapping from Sierra statement index to fully qualified Cairo path of the Cairo function
/// (if obtainable) which caused the statement to be generated.
///
/// Should be created using
/// [`crate::debug_info::StatementsLocations::extract_statements_functions`].
#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct StatementsFunctions {
    pub statements_to_functions_map: HashMap<StatementIdx, Vec<String>>,
}

impl From<StatementsFunctions> for Annotations {
    fn from(value: StatementsFunctions) -> Self {
        let mapping = serde_json::to_value(value.statements_to_functions_map).unwrap();
        OrderedHashMap::from([(
            "github.com/software-mansion/cairo-profiler".to_string(),
            serde_json::Value::from_iter([("statements_functions", mapping)]),
        )])
    }
}
/// Returns an identifier of the function that contains the given [StableLocation].
/// It is a fully qualified path to the function which contains:
/// - fully qualified path to the file module,
/// - relative path to the function in the file module.
pub fn maybe_containing_function_identifier(
    db: &dyn Database,
    location: StableLocation<'_>,
) -> Option<String> {
    let file_id = location.file_id(db);
    let absolute_semantic_path_to_file_module = file_module_absolute_identifier(db, file_id)?;

    let relative_semantic_path = function_identifier_relative_to_file_module(db, location);
    if relative_semantic_path.is_empty() {
        // In some cases the stable location maps to a code that is a statement like a function call
        // directly in a file module, e.g., `Self::eq(lhs, rhs)` in `core::traits`. This brings no
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
    db: &dyn Database,
    location: StableLocation<'_>,
) -> Option<String> {
    let file_id = location.file_id(db);
    let absolute_semantic_path_to_file_module = file_module_absolute_identifier(db, file_id)
        .unwrap_or_else(|| file_id.file_name(db).to_string(db));

    let relative_semantic_path = function_identifier_relative_to_file_module(db, location);
    if relative_semantic_path.is_empty() {
        // In some cases the stable location maps to a code that is a statement like a function call
        // directly in a file module, e.g., `Self::eq(lhs, rhs)` in `core::traits`. This brings no
        // information about the function it was called from. It is especially relevant for corelib
        // tests where the first stable location may map to this kind of code.
        None
    } else {
        Some(absolute_semantic_path_to_file_module.add("::").add(&relative_semantic_path))
    }
}

/// Returns the path (modules and impls) to the function in the file.
/// The path is relative to the file module.
fn function_identifier_relative_to_file_module(
    db: &dyn Database,
    location: StableLocation<'_>,
) -> String {
    let mut relative_semantic_path_segments: Vec<String> = vec![];
    let mut syntax_node = location.syntax_node(db);
    let mut statement_located_in_function = false;
    loop {
        // TODO(Gil): Extract this function into a trait of syntax kind to support future
        // function containing items (specifically trait functions).
        match syntax_node.kind(db) {
            cairo_lang_syntax::node::kind::SyntaxKind::FunctionWithBody => {
                let function_name = FunctionWithBody::from_syntax_node(db, syntax_node)
                    .declaration(db)
                    .name(db)
                    .text(db);

                if relative_semantic_path_segments.is_empty() {
                    statement_located_in_function = true;
                }

                relative_semantic_path_segments.push(function_name.to_string(db));
            }
            cairo_lang_syntax::node::kind::SyntaxKind::ItemImpl => {
                let impl_name =
                    cairo_lang_syntax::node::ast::ItemImpl::from_syntax_node(db, syntax_node)
                        .name(db)
                        .text(db);
                relative_semantic_path_segments.push(impl_name.to_string(db));
            }
            cairo_lang_syntax::node::kind::SyntaxKind::ItemModule => {
                let module_name =
                    cairo_lang_syntax::node::ast::ItemModule::from_syntax_node(db, syntax_node)
                        .name(db)
                        .text(db);
                relative_semantic_path_segments.push(module_name.to_string(db));
            }
            _ => {}
        }
        if let Some(parent) = syntax_node.parent(db) {
            syntax_node = parent;
        } else {
            break;
        }
    }

    // If the statement is not located in a function, and it is located a generated file it is
    // probably located in a code block generated by an inline macro such as `array` or `panic`.
    let file_id = location.file_id(db);
    if !statement_located_in_function
        && matches!(file_id.long(db), FileLongId::Virtual(VirtualFile { parent: Some(_), .. }))
    {
        relative_semantic_path_segments.insert(0, file_id.file_name(db).to_string(db));
    }

    relative_semantic_path_segments.into_iter().rev().join("::")
}

/// This function returns a fully qualified path to the file module.
/// `None` should be returned only for compiler tests where files of type `VirtualFile` may be non
/// generated files.
fn file_module_absolute_identifier<'db>(
    db: &'db dyn Database,
    mut file_id: FileId<'db>,
) -> Option<String> {
    // `VirtualFile` is a generated file (e.g., by macros like `#[starknet::contract]`)
    // that will not have a matching file module in the DB. Instead, we find its non-generated
    // parent which is in the same module and has a matching file module in the DB.
    while let FileLongId::Virtual(VirtualFile { parent: Some(parent), .. }) = file_id.long(db) {
        file_id = parent.file_id;
    }

    let file_modules = db.file_modules(file_id).to_option()?;
    let full_path = file_modules.first().unwrap().full_path(db);

    Some(full_path)
}
