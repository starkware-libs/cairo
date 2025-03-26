use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::{Intern, LookupIntern, Upcast};

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::key_fields::get_key_fields;
use super::stable_ptr::SyntaxStablePtr;
use super::{SyntaxNode, SyntaxNodeLongId};

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> {
    #[salsa::interned]
    fn intern_green(&self, field: Arc<GreenNode>) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;
    #[salsa::interned]
    fn intern_syntax_node(&self, field: SyntaxNodeLongId) -> SyntaxNode;

    /// Returns the children of the given node.
    fn get_children(&self, node: SyntaxNode) -> Arc<[SyntaxNode]>;
}

fn get_children(db: &dyn SyntaxGroup, node: SyntaxNode) -> Arc<[SyntaxNode]> {
    let mut res = Vec::new();

    let mut offset = node.offset(db);
    let mut key_map = UnorderedHashMap::<_, usize>::default();
    for green_id in node.green_node(db).children() {
        let green = green_id.lookup_intern(db);
        let width = green.width();
        let kind = green.kind;
        let key_fields: Vec<GreenId> = get_key_fields(kind, green.children());
        let key_count = key_map.entry((kind, key_fields.clone())).or_default();
        let stable_ptr = SyntaxStablePtr::Child {
            parent: node.stable_ptr(db),
            kind,
            key_fields,
            index: *key_count,
        }
        .intern(db);
        *key_count += 1;
        // Create the SyntaxNode view for the child.
        res.push(
            SyntaxNodeLongId { green: *green_id, offset, parent: Some(node), stable_ptr }
                .intern(db),
        );

        offset = offset.add_width(width);
    }
    res.into()
}
