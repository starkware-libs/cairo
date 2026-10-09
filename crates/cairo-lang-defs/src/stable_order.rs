//! Keys of definitions that are consistent across builds, unlike their ids: by their position in
//! the source code.

use cairo_lang_filesystem::db::{get_parent_and_mapping, translate_location};
use cairo_lang_filesystem::ids::{CrateId, CrateLongId, FileId};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use salsa::Database;
use smol_str::SmolStr;

use crate::db::DefsGroup;
use crate::ids::{LanguageElementId, ModuleId};

/// Separates the positions of the nodes in the path of a key.
const SEPARATOR: usize = usize::MAX;

/// The position of a definition or a syntax node: its crate, then the declaration of each
/// enclosing module from the crate root inwards, then the node itself.
#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, salsa::SalsaValue)]
pub struct ElementKey<'db> {
    /// The name and discriminator of the owning crate, if within a module.
    /// SAFETY: references into an interned crate id, which is retained for the lifetime of the
    /// database.
    #[salsa_value(unsafe(prove_safe_to_retain_manually))]
    crate_key: Option<(&'db SmolStr, Option<&'db str>)>,
    /// The positions of the nodes, each the index of its file among its module's files and then
    /// the index of each ancestor among its siblings from the file root down to the node, with a
    /// [SEPARATOR] between the positions.
    path: Vec<usize>,
}

impl<'db> ElementKey<'db> {
    /// Appends the position of a node in a file with the given index in its module.
    fn push_position(
        &mut self,
        db: &'db dyn Database,
        file_index: Option<usize>,
        mut node: SyntaxNode<'db>,
    ) {
        self.path.push(SEPARATOR);
        self.path.extend(file_index);
        let start = self.path.len();
        while let Some(parent) = node.parent(db) {
            self.path
                .push(parent.get_children(db).iter().position(|child| *child == node).unwrap());
            node = parent;
        }
        self.path[start..].reverse();
    }
}

/// The key of a language element's definition.
pub fn element_key<'db>(
    db: &'db dyn Database,
    element: &impl LanguageElementId<'db>,
) -> ElementKey<'db> {
    node_in_module_key(db, element.parent_module(db), element.untyped_stable_ptr(db).lookup(db))
}

/// The key of a syntax node: its position in its file, or if the file is generated within a file
/// of a module, the key of the node it originates from, then its position in the generated file.
pub fn node_key<'db>(db: &'db dyn Database, stable_ptr: SyntaxStablePtrId<'db>) -> ElementKey<'db> {
    let node = stable_ptr.lookup(db);
    let file_id = node.file_id(db);
    if let Ok(modules) = db.file_modules(file_id)
        && let Some(module) = modules.first()
    {
        return node_in_module_key(db, *module, node);
    }
    let mut key = match get_parent_and_mapping(db, file_id) {
        Some((parent, code_mappings)) => {
            let span = translate_location(code_mappings, node.span(db)).unwrap_or(parent.span);
            db.file_syntax(parent.file_id)
                .map(|root| node_key(db, root.lookup_offset(db, span.start).stable_ptr(db)))
                .unwrap_or_default()
        }
        None => ElementKey::default(),
    };
    key.push_position(db, None, node);
    key
}

/// The key of a syntax node in a file of the given module.
fn node_in_module_key<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    node: SyntaxNode<'db>,
) -> ElementKey<'db> {
    let mut key = module_key(db, module);
    key.push_position(db, file_index(db, module, node.file_id(db)), node);
    key
}

/// The key of the declaration of a module, or of just the crate for a root module.
fn module_key<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> ElementKey<'db> {
    let (parent, stable_ptr) = match module {
        ModuleId::CrateRoot(crate_id) => {
            return ElementKey { crate_key: Some(crate_key(db, crate_id)), path: vec![] };
        }
        ModuleId::Submodule(id) => (id.parent_module(db), id.untyped_stable_ptr(db)),
        ModuleId::MacroCall { id, .. } => (id.parent_module(db), id.untyped_stable_ptr(db)),
    };
    node_in_module_key(db, parent, stable_ptr.lookup(db))
}

/// The name and discriminator of a crate.
fn crate_key<'db>(
    db: &'db dyn Database,
    crate_id: CrateId<'db>,
) -> (&'db SmolStr, Option<&'db str>) {
    match crate_id.long(db) {
        CrateLongId::Real { name, discriminator } => (name.long(db), discriminator.as_deref()),
        CrateLongId::Virtual { name, .. } => (name.long(db), None),
    }
}

/// The index of a file among the files of a module.
fn file_index<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    file_id: FileId<'db>,
) -> Option<usize> {
    db.module_files(module).ok().and_then(|files| files.iter().position(|f| *f == file_id))
}
