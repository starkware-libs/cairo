use cairo_lang_filesystem::ids::FileId;
use cairo_lang_utils::interner::{InternedElement, PtrIndex};

use super::db::SyntaxGroup;
use super::green::GreenId;
use super::kind::SyntaxKind;
use super::SyntaxNode;

// Green node. Underlying untyped representation of the syntax tree.
#[derive(Debug, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct SyntaxStablePtrHeader([u8]);
#[allow(clippy::uninit_vec)]
impl SyntaxStablePtrHeader {
    pub fn new_root(
        buffer: &mut Vec<u8>,
        file_id: FileId,
        green_id: GreenId,
    ) -> &SyntaxStablePtrHeader {
        let total_size = 4 + 4 + 4;
        buffer.reserve(total_size);
        unsafe {
            buffer.set_len(total_size);
            std::ptr::write(buffer.as_mut_ptr() as *mut u32, 0);
            std::ptr::copy_nonoverlapping(
                &file_id as *const _ as *const u8,
                buffer.as_mut_ptr().add(4),
                4,
            );
            std::ptr::copy_nonoverlapping(
                &green_id as *const _ as *const u8,
                buffer.as_mut_ptr().add(8),
                4,
            );
            std::mem::transmute(&buffer[..total_size])
        }
    }
    pub fn new_child<'a>(
        buffer: &'a mut Vec<u8>,
        parent: SyntaxStablePtrId,
        kind: SyntaxKind,
        index: u16,
        key_fields: &[GreenId],
    ) -> &'a SyntaxStablePtrHeader {
        let total_size = 4 + 2 + 2 + std::mem::size_of_val(key_fields);
        buffer.reserve(total_size);
        unsafe {
            buffer.set_len(total_size);
            std::ptr::copy_nonoverlapping(&parent as *const _ as *const u8, buffer.as_mut_ptr(), 4);
            std::ptr::copy_nonoverlapping(
                &kind as *const _ as *const u8,
                buffer.as_mut_ptr().add(4),
                2,
            );
            std::ptr::copy_nonoverlapping(
                &index as *const _ as *const u8,
                buffer.as_mut_ptr().add(6),
                2,
            );
            std::ptr::copy_nonoverlapping(
                key_fields.as_ptr() as *const u8,
                buffer.as_mut_ptr().add(8),
                std::mem::size_of_val(key_fields),
            );
            std::mem::transmute(&buffer[..total_size])
        }
    }
    pub fn parent(&self) -> Option<SyntaxStablePtrId> {
        let parent: SyntaxStablePtrId =
            unsafe { std::mem::transmute(self.0.as_ptr().cast::<u32>().read()) };
        if parent.0.is_empty() { None } else { Some(parent) }
    }
    pub fn kind(&self) -> Option<SyntaxKind> {
        self.parent()?;
        let kind: SyntaxKind =
            unsafe { std::mem::transmute(self.0.as_ptr().add(4).cast::<u16>().read()) };
        Some(kind)
    }
    pub fn key_fields(&self) -> Option<&[GreenId]> {
        self.parent()?;
        let key_fields: &[GreenId] = unsafe {
            let key_fields_bytes = self.0.get_unchecked(8..);
            core::slice::from_raw_parts(
                key_fields_bytes.as_ptr().cast::<GreenId>(),
                key_fields_bytes.len() / std::mem::size_of::<GreenId>(),
            )
        };
        Some(key_fields)
    }
    fn file_id_for_root(&self) -> FileId {
        let file_id: FileId =
            unsafe { std::mem::transmute(self.0.as_ptr().add(4).cast::<u32>().read()) };
        file_id
    }
    fn green_id_for_root(&self) -> GreenId {
        let green_id: GreenId =
            unsafe { std::mem::transmute(self.0.as_ptr().add(8).cast::<u32>().read()) };
        green_id
    }
}
impl InternedElement for SyntaxStablePtrHeader {
    fn from_u8s(buf: &[u8]) -> &Self {
        unsafe { std::mem::transmute(buf) }
    }
}
pub struct SyntaxStablePtr<'a>(pub &'a SyntaxStablePtrHeader);
impl<'a> SyntaxStablePtr<'a> {
    pub fn new_root(buffer: &'a mut Vec<u8>, file_id: FileId, green_id: GreenId) -> Self {
        let header = SyntaxStablePtrHeader::new_root(buffer, file_id, green_id);
        Self(header)
    }
    pub fn new_child(
        buffer: &'a mut Vec<u8>,
        parent: SyntaxStablePtrId,
        kind: SyntaxKind,
        index: u16,
        key_fields: &[GreenId],
    ) -> Self {
        let header = SyntaxStablePtrHeader::new_child(buffer, parent, kind, index, key_fields);
        Self(header)
    }
    pub fn parent(&self) -> Option<SyntaxStablePtrId> {
        self.0.parent()
    }
    pub fn kind(&self) -> Option<SyntaxKind> {
        self.0.kind()
    }
    pub fn key_fields(&self) -> Option<&[GreenId]> {
        self.0.key_fields()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct SyntaxStablePtrId(pub PtrIndex);
impl SyntaxStablePtrId {
    /// Lookups a syntax node using a stable syntax pointer.
    /// Should only be called on the root from which the stable pointer was generated.
    pub fn lookup(&self, db: &dyn SyntaxGroup) -> SyntaxNode {
        let ptr = db.lookup_intern_stable_ptr(*self);
        if let Some(parent) = ptr.parent() {
            let parent = parent.lookup(db);
            for child in parent.children(db) {
                if child.stable_ptr() == *self {
                    return child;
                }
            }
            unreachable!();
        }
        let file_id = ptr.0.file_id_for_root();
        let green_id = ptr.0.green_id_for_root();
        SyntaxNode::new_root(db, file_id, green_id)
    }
    pub fn file_id(&self, db: &dyn SyntaxGroup) -> FileId {
        let ptr = db.lookup_intern_stable_ptr(*self);
        if let Some(parent) = ptr.parent() { parent.file_id(db) } else { ptr.0.file_id_for_root() }
    }
}
