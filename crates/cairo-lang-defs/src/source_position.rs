//! Comparison of definitions by their position in the source code, independent of interning order.

use std::cmp::Ordering;

use cairo_lang_filesystem::ids::{CrateId, CrateLongId, FileId, SpanInFile};
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use salsa::Database;

use crate::db::DefsGroup;
use crate::ids::{LanguageElementId, ModuleId};

/// Compares two language elements by the positions of their definitions: by crate, then by the
/// declaration order of the enclosing modules, then by the position of the definition.
pub fn cmp_elements<'db>(
    db: &'db dyn Database,
    a: &impl LanguageElementId<'db>,
    b: &impl LanguageElementId<'db>,
) -> Ordering {
    cmp_paths(db, &element_path(db, a), &element_path(db, b))
}

/// Compares two syntax nodes by the positions of the user code they originate from, then by their
/// offsets in their own files if those are generated files.
pub fn cmp_nodes<'db>(
    db: &'db dyn Database,
    a: SyntaxStablePtrId<'db>,
    b: SyntaxStablePtrId<'db>,
) -> Ordering {
    cmp_paths(db, &node_path(db, a), &node_path(db, b))
}

/// A position of a syntax node within a module: the index of its file among the module's files,
/// and its offset in that file.
type FilePosition = (Option<usize>, TextOffset);

/// The owning crate, if within a module, and the declaration of each enclosing module from the
/// crate root inwards, then the node itself.
type Path<'db> = (Option<CrateId<'db>>, Vec<FilePosition>);

/// Compares paths, ordering crates by name and discriminator.
fn cmp_paths<'db>(db: &'db dyn Database, a: &Path<'db>, b: &Path<'db>) -> Ordering {
    let crate_key = |crate_id: &Option<CrateId<'db>>| {
        crate_id.map(|crate_id| match crate_id.long(db) {
            CrateLongId::Real { name, discriminator } => (name.long(db), discriminator.as_ref()),
            CrateLongId::Virtual { name, .. } => (name.long(db), None),
        })
    };
    let crates = if a.0 == b.0 { Ordering::Equal } else { crate_key(&a.0).cmp(&crate_key(&b.0)) };
    crates.then_with(|| a.1.cmp(&b.1))
}

fn element_path<'db>(db: &'db dyn Database, element: &impl LanguageElementId<'db>) -> Path<'db> {
    node_in_module_path(db, element.parent_module(db), element.untyped_stable_ptr(db))
}

fn node_path<'db>(db: &'db dyn Database, stable_ptr: SyntaxStablePtrId<'db>) -> Path<'db> {
    let node = stable_ptr.lookup(db);
    let file_id = stable_ptr.file_id(db);
    let user_location = SpanInFile { file_id, span: node.span(db) }.user_location(db);
    let module = db.file_modules(user_location.file_id).ok().and_then(|modules| modules.first());
    let (crate_id, mut path) = match module {
        Some(module) => module_path(db, *module),
        None => (None, vec![]),
    };
    let file_index = module.and_then(|module| file_index(db, *module, user_location.file_id));
    path.push((file_index, user_location.span.start));
    if user_location.file_id != file_id {
        path.push((None, node.offset(db)));
    }
    (crate_id, path)
}

fn node_in_module_path<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    stable_ptr: SyntaxStablePtrId<'db>,
) -> Path<'db> {
    let (crate_id, mut path) = module_path(db, module);
    path.push((file_index(db, module, stable_ptr.file_id(db)), stable_ptr.lookup(db).offset(db)));
    (crate_id, path)
}

/// The path of the declaration of a module, or of just the crate for a root module.
fn module_path<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> Path<'db> {
    let (parent, stable_ptr) = match module {
        ModuleId::CrateRoot(crate_id) => return (Some(crate_id), vec![]),
        ModuleId::Submodule(id) => (id.parent_module(db), id.untyped_stable_ptr(db)),
        ModuleId::MacroCall { id, .. } => (id.parent_module(db), id.untyped_stable_ptr(db)),
    };
    node_in_module_path(db, parent, stable_ptr)
}

/// The index of a file among the files of a module.
fn file_index<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    file_id: FileId<'db>,
) -> Option<usize> {
    db.module_files(module).ok().and_then(|files| files.iter().position(|f| *f == file_id))
}
