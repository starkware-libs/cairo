use cairo_lang_filesystem::span::TextWidth;
use cairo_lang_utils::interner::{InternedElement, PtrIndex, UnsizedInterner};

use super::db::SyntaxGroup;
use super::kind::SyntaxKind;
use super::stable_ptr::{SyntaxStablePtr, SyntaxStablePtrHeader, SyntaxStablePtrId};

/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Debug, Hash, PartialEq, Eq)]
#[repr(transparent)]
pub struct GreenNodeHeader([u8]);
#[allow(clippy::uninit_vec)]
impl GreenNodeHeader {
    pub fn new_token<'a>(
        buffer: &'a mut Vec<u8>,
        kind: SyntaxKind,
        text: &str,
    ) -> &'a GreenNodeHeader {
        debug_assert!(kind.is_token(), "kind {:?} is not a token", kind);
        let width = TextWidth::from_str(text).0;
        let kind = kind as u16;
        let total_size = 4 + 2 + text.len();
        buffer.reserve(total_size);
        unsafe {
            buffer.set_len(total_size);
            std::ptr::copy_nonoverlapping(&width as *const _ as *const u8, buffer.as_mut_ptr(), 4);
            std::ptr::copy_nonoverlapping(
                &kind as *const _ as *const u8,
                buffer.as_mut_ptr().add(4),
                2,
            );
            std::ptr::copy_nonoverlapping(text.as_ptr(), buffer.as_mut_ptr().add(6), text.len());
            std::mem::transmute(&buffer[..total_size])
        }
    }
    pub fn new_inner<'a>(
        buffer: &'a mut Vec<u8>,
        kind: SyntaxKind,
        width: TextWidth,
        children: &[GreenId],
    ) -> &'a GreenNodeHeader {
        debug_assert!(!kind.is_token(), "kind {:?} is a token", kind);
        let width = width.0;
        let kind = kind as u16;
        let total_size = 4 + 4 + std::mem::size_of_val(children);
        buffer.reserve(total_size);
        unsafe {
            buffer.set_len(total_size);
            std::ptr::copy_nonoverlapping(&width as *const _ as *const u8, buffer.as_mut_ptr(), 4);
            std::ptr::copy_nonoverlapping(
                &kind as *const _ as *const u8,
                buffer.as_mut_ptr().add(4),
                2,
            );
            std::ptr::copy_nonoverlapping(
                children.as_ptr() as *const u8,
                buffer.as_mut_ptr().add(8),
                std::mem::size_of_val(children),
            );
            std::mem::transmute(&buffer[..total_size])
        }
    }
    pub fn kind(&self) -> SyntaxKind {
        unsafe { std::mem::transmute(self.0.as_ptr().add(4).cast::<u16>().read()) }
    }
    pub fn width(&self) -> TextWidth {
        unsafe { std::mem::transmute(self.0.as_ptr().cast::<u32>().read()) }
    }
    pub fn children(&self) -> &[GreenId] {
        if self.kind().is_token() {
            &[]
        } else {
            let children: &[GreenId] = unsafe {
                let children_bytes = self.0.get_unchecked(8..);
                core::slice::from_raw_parts(
                    children_bytes.as_ptr().cast::<GreenId>(),
                    children_bytes.len() / std::mem::size_of::<GreenId>(),
                )
            };
            children
        }
    }
    pub fn token_text(&self) -> Option<&str> {
        if self.kind().is_token() {
            let text: &str = unsafe { std::str::from_utf8_unchecked(self.0.get_unchecked(6..)) };
            Some(text)
        } else {
            None
        }
    }
}
impl InternedElement for GreenNodeHeader {
    fn from_u8s(buf: &[u8]) -> &Self {
        unsafe { std::mem::transmute(buf) }
    }
}
pub struct GreenNode<'a>(pub &'a GreenNodeHeader);
impl<'a> GreenNode<'a> {
    pub fn new_token(buffer: &'a mut Vec<u8>, kind: SyntaxKind, text: &'a str) -> Self {
        Self(GreenNodeHeader::new_token(buffer, kind, text))
    }
    pub fn new_inner(
        buffer: &'a mut Vec<u8>,
        kind: SyntaxKind,
        width: TextWidth,
        children: &[GreenId],
    ) -> Self {
        Self(GreenNodeHeader::new_inner(buffer, kind, width, children))
    }
    pub fn kind(&self) -> SyntaxKind {
        self.0.kind()
    }
    pub fn width(&self) -> TextWidth {
        self.0.width()
    }
    pub fn children(&self) -> &'a [GreenId] {
        self.0.children()
    }
    pub fn token_text(&self) -> Option<&'a str> {
        self.0.token_text()
    }
}

const INITIAL_CAPACITY: usize = 65536;

pub struct SyntaxInterner {
    green_interner: UnsizedInterner<GreenNodeHeader>,
    stable_interner: UnsizedInterner<SyntaxStablePtrHeader>,
}
impl SyntaxInterner {
    pub fn new() -> Self {
        Self {
            green_interner: UnsizedInterner::new(INITIAL_CAPACITY),
            stable_interner: UnsizedInterner::new(INITIAL_CAPACITY),
        }
    }
    pub fn intern_green(&self, node: &GreenNode<'_>) -> GreenId {
        GreenId(self.green_interner.intern(node.0))
    }
    pub fn lookup_green(&self, id: GreenId) -> GreenNode<'static> {
        GreenNode(self.green_interner.lookup(id.0))
    }
    pub fn intern_stable(&self, ptr: &SyntaxStablePtr<'_>) -> SyntaxStablePtrId {
        SyntaxStablePtrId(self.stable_interner.intern(ptr.0))
    }
    pub fn lookup_stable(&self, id: SyntaxStablePtrId) -> SyntaxStablePtr<'static> {
        SyntaxStablePtr(self.stable_interner.lookup(id.0))
    }
}
impl Default for SyntaxInterner {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenId(PtrIndex);
impl GreenId {
    pub fn kind(&self, db: &dyn SyntaxGroup) -> SyntaxKind {
        db.lookup_intern_green(*self).kind()
    }
    pub fn width(&self, db: &dyn SyntaxGroup) -> TextWidth {
        db.lookup_intern_green(*self).width()
    }
    pub fn children(&self, db: &dyn SyntaxGroup) -> &'static [GreenId] {
        db.lookup_intern_green(*self).children()
    }
    pub fn token_text(&self, db: &dyn SyntaxGroup) -> Option<&'static str> {
        db.lookup_intern_green(*self).token_text()
    }
}
