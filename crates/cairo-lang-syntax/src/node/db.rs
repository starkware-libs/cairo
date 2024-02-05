use std::sync::Arc;

use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::Upcast;

use super::green::GreenNode;
use super::ids::{GreenId, SyntaxStablePtrId};
use super::key_fields::get_key_fields;
use super::stable_ptr::SyntaxStablePtr;
use super::{SyntaxNode, SyntaxNodeInner};

// Salsa database interface.
#[salsa::query_group(SyntaxDatabase)]
pub trait SyntaxGroup: FilesGroup + Upcast<dyn FilesGroup> {
    #[salsa::interned]
    fn intern_green(&self, field: Arc<GreenNode>) -> GreenId;
    #[salsa::interned]
    fn intern_stable_ptr(&self, field: SyntaxStablePtr) -> SyntaxStablePtrId;

    /// Returns the children of the given node.
    fn get_children(&self, node: SyntaxNode) -> Arc<Vec<SyntaxNode>>;
}

fn get_children(db: &dyn SyntaxGroup, node: SyntaxNode) -> Arc<Vec<SyntaxNode>> {
    let mut res = Vec::new();

    let mut offset = node.offset();
    let mut key_map = UnorderedHashMap::<_, usize>::default();
    for green_id in node.green_node(db).children() {
        let green = db.lookup_intern_green(*green_id);
        let width = green.width();
        let kind = green.kind;
        let key_fields: Vec<GreenId> = get_key_fields(kind, green.children());
        let key_count = key_map.entry((kind, key_fields.clone())).or_default();
        let stable_ptr = db.intern_stable_ptr(SyntaxStablePtr::Child {
            parent: node.0.stable_ptr,
            kind,
            key_fields,
            index: *key_count,
        });
        *key_count += 1;
        // Create the SyntaxNode view for the child.
        res.push(SyntaxNode(Arc::new(SyntaxNodeInner {
            green: *green_id,
            offset,
            parent: Some(node.clone()),
            stable_ptr,
        })));

        offset = offset.add_width(width);
    }
    Arc::new(res)
}
