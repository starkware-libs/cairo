//! Comparison of definitions that is consistent across builds, unlike the order of their ids:
//! by crate, then by the enclosing modules, then by the nodes' kinds and key fields in their
//! file. Only the syntax tree structure is read, never offsets or ids.

use std::cmp::Ordering;

use cairo_lang_filesystem::ids::{CrateId, CrateLongId, FileId, FileLongId, VirtualFile};
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::green::GreenNodeDetails;
use cairo_lang_syntax::node::ids::{GreenId, SyntaxStablePtrId};
use cairo_lang_syntax::node::key_fields::key_fields_range;
use salsa::Database;

use crate::db::DefsGroup;
use crate::ids::{LanguageElementId, ModuleId};

/// Compares two language elements by the positions of their definitions.
pub fn cmp_elements<'db>(
    db: &'db dyn Database,
    a: &impl LanguageElementId<'db>,
    b: &impl LanguageElementId<'db>,
) -> Ordering {
    cmp_nodes_in_modules(
        db,
        (Some(a.parent_module(db)), a.untyped_stable_ptr(db).lookup(db)),
        (Some(b.parent_module(db)), b.untyped_stable_ptr(db).lookup(db)),
    )
}

/// Compares two syntax nodes by their positions. Nodes in code generated for an inline macro are
/// positioned at the macro call site.
pub fn cmp_nodes<'db>(
    db: &'db dyn Database,
    a: SyntaxStablePtrId<'db>,
    b: SyntaxStablePtrId<'db>,
) -> Ordering {
    let (a, b) = (a.lookup(db), b.lookup(db));
    if a.file_id(db) == b.file_id(db) {
        return cmp_in_file(db, a, b);
    }
    cmp_nodes_in_modules(db, anchor(db, a), anchor(db, b))
}

/// A node with the module of its file, or if the file was generated for an inline macro, the node
/// at the macro call site in the user's code.
fn anchor<'db>(
    db: &'db dyn Database,
    mut node: SyntaxNode<'db>,
) -> (Option<ModuleId<'db>>, SyntaxNode<'db>) {
    loop {
        let file_id = node.file_id(db);
        if let Some(module) = db.file_modules(file_id).ok().and_then(|modules| modules.first()) {
            return (Some(*module), node);
        }
        let FileLongId::Virtual(VirtualFile { parent: Some(call_site), .. }) = file_id.long(db)
        else {
            return (None, node);
        };
        let Ok(root) = db.file_syntax(call_site.file_id) else {
            return (None, node);
        };
        node = root.lookup_offset(db, call_site.span.start);
    }
}

fn cmp_nodes_in_modules<'db>(
    db: &'db dyn Database,
    (a_module, a): (Option<ModuleId<'db>>, SyntaxNode<'db>),
    (b_module, b): (Option<ModuleId<'db>>, SyntaxNode<'db>),
) -> Ordering {
    let (Some(a_module), Some(b_module)) = (a_module, b_module) else {
        return a_module.is_some().cmp(&b_module.is_some()).then_with(|| {
            if a.file_id(db) == b.file_id(db) { cmp_in_file(db, a, b) } else { Ordering::Equal }
        });
    };
    cmp_crates(db, a_module.owning_crate(db), b_module.owning_crate(db))
        .then_with(|| cmp_chains(db, (a_module, a), (b_module, b)))
}

/// A node in a module, standing for the chain of the declarations of the enclosing modules
/// followed by the node itself.
type Chain<'db> = (ModuleId<'db>, SyntaxNode<'db>);

/// Compares chains of the same crate lexicographically.
fn cmp_chains<'db>(db: &'db dyn Database, a: Chain<'db>, b: Chain<'db>) -> Ordering {
    let (a_len, b_len) = (module_depth(db, a.0) + 1, module_depth(db, b.0) + 1);
    let len = a_len.min(b_len);
    let prefix = |(module, node): Chain<'db>, chain_len: usize| {
        if chain_len == len {
            (module, node)
        } else {
            // The declaration of the module whose chain is `len` long.
            declaration(db, nth_parent_module(db, module, chain_len - len - 1))
        }
    };
    cmp_same_len_chains(db, prefix(a, a_len), prefix(b, b_len)).then_with(|| a_len.cmp(&b_len))
}

/// Compares chains of the same length and crate.
fn cmp_same_len_chains<'db>(db: &'db dyn Database, a: Chain<'db>, b: Chain<'db>) -> Ordering {
    debug_assert_eq!(module_depth(db, a.0), module_depth(db, b.0));
    if a.1 == b.1 {
        return Ordering::Equal;
    }
    let prefixes = match (a.0, b.0) {
        (ModuleId::CrateRoot(_), ModuleId::CrateRoot(_)) => Ordering::Equal,
        _ => cmp_same_len_chains(db, declaration(db, a.0), declaration(db, b.0)),
    };
    // Equal prefixes mean the nodes are in the same module.
    prefixes.then_with(|| {
        let (a_file, b_file) = (a.1.file_id(db), b.1.file_id(db));
        if a_file == b_file {
            cmp_in_file(db, a.1, b.1)
        } else {
            file_index(db, a.0, a_file).cmp(&file_index(db, a.0, b_file))
        }
    })
}

/// Compares nodes of the same file: an ancestor before its descendants, then by the first
/// differing ancestors, which are siblings: by kind, then by key fields, then by position.
fn cmp_in_file<'db>(db: &'db dyn Database, a: SyntaxNode<'db>, b: SyntaxNode<'db>) -> Ordering {
    let (a_depth, b_depth) = (depth(db, a), depth(db, b));
    let a = a.nth_parent(db, a_depth.saturating_sub(b_depth));
    let b = b.nth_parent(db, b_depth.saturating_sub(a_depth));
    if a == b {
        return a_depth.cmp(&b_depth);
    }
    let (Some(a_parent), Some(b_parent)) = (a.parent(db), b.parent(db)) else {
        unreachable!("Distinct nodes of one file have parents.");
    };
    cmp_in_file(db, a_parent, b_parent)
        .then_with(|| (a.kind(db) as usize).cmp(&(b.kind(db) as usize)))
        .then_with(|| cmp_greens(db, a.key_fields(db), b.key_fields(db)))
        .then_with(|| {
            let index = |parent: SyntaxNode<'db>, node| {
                parent.get_children(db).iter().position(|child| *child == node)
            };
            index(a_parent, a).cmp(&index(b_parent, b))
        })
}

fn cmp_greens<'db>(db: &'db dyn Database, a: &[GreenId<'db>], b: &[GreenId<'db>]) -> Ordering {
    a.iter()
        .zip(b)
        .map(|(a, b)| cmp_green(db, *a, *b))
        .find(|ordering| ordering.is_ne())
        .unwrap_or(a.len().cmp(&b.len()))
}

/// Compares green nodes by kind and then by their text, for tokens, or key fields, for nodes.
/// Trivia is ignored.
fn cmp_green<'db>(db: &'db dyn Database, a: GreenId<'db>, b: GreenId<'db>) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    let (a, b) = (a.long(db), b.long(db));
    (a.kind as usize).cmp(&(b.kind as usize)).then_with(|| match (&a.details, &b.details) {
        (GreenNodeDetails::Token(a), GreenNodeDetails::Token(b)) => a.long(db).cmp(b.long(db)),
        (
            GreenNodeDetails::Node { children: a_children, .. },
            GreenNodeDetails::Node { children: b_children, .. },
        ) => {
            // A terminal is its token between its trivia.
            let range = if a.kind.is_terminal() { 1..2 } else { key_fields_range(a.kind) };
            cmp_greens(db, &a_children[range.clone()], &b_children[range])
        }
        _ => unreachable!("Green nodes of the same kind have the same shape."),
    })
}

fn cmp_crates<'db>(db: &'db dyn Database, a: CrateId<'db>, b: CrateId<'db>) -> Ordering {
    if a == b {
        return Ordering::Equal;
    }
    let key = |crate_id: CrateId<'db>| match crate_id.long(db) {
        CrateLongId::Real { name, discriminator } => (name.long(db), discriminator.as_ref()),
        CrateLongId::Virtual { name, .. } => (name.long(db), None),
    };
    key(a).cmp(&key(b))
}

fn depth<'db>(db: &'db dyn Database, node: SyntaxNode<'db>) -> usize {
    std::iter::successors(node.parent(db), |node| node.parent(db)).count()
}

fn module_depth<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> usize {
    std::iter::successors(Some(module), |module| {
        (!matches!(module, ModuleId::CrateRoot(_))).then(|| parent_module(db, *module))
    })
    .count()
        - 1
}

fn nth_parent_module<'db>(db: &'db dyn Database, module: ModuleId<'db>, n: usize) -> ModuleId<'db> {
    (0..n).fold(module, |module, _| parent_module(db, module))
}

/// The parent of a non-root module.
fn parent_module<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> ModuleId<'db> {
    declaration(db, module).0
}

/// The declaration of a non-root module, as a node in its parent module.
fn declaration<'db>(db: &'db dyn Database, module: ModuleId<'db>) -> Chain<'db> {
    let (parent, stable_ptr) = match module {
        ModuleId::CrateRoot(_) => unreachable!("Root modules have no declaration."),
        ModuleId::Submodule(id) => (id.parent_module(db), id.untyped_stable_ptr(db)),
        ModuleId::MacroCall { id, .. } => (id.parent_module(db), id.untyped_stable_ptr(db)),
    };
    (parent, stable_ptr.lookup(db))
}

/// The index of a file among the files of a module.
fn file_index<'db>(
    db: &'db dyn Database,
    module: ModuleId<'db>,
    file_id: FileId<'db>,
) -> Option<usize> {
    db.module_files(module).ok().and_then(|files| files.iter().position(|f| *f == file_id))
}
