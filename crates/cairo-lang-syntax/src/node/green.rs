use cairo_lang_filesystem::span::TextWidth;
use fxhash::FxHashMap;
use itertools::chain;
use smol_str::SmolStr;

use super::db::SyntaxGroup;
use super::kind::SyntaxKind;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GreenNodeDetails {
    Token(SmolStr),
    Node { children: Vec<GreenId>, width: TextWidth },
}
/// Green node. Underlying untyped representation of the syntax tree.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenNode {
    pub kind: SyntaxKind,
    pub details: GreenNodeDetails,
}
impl GreenNode {
    pub fn width(&self) -> TextWidth {
        match &self.details {
            GreenNodeDetails::Token(text) => TextWidth::from_str(text),
            GreenNodeDetails::Node { width, .. } => *width,
        }
    }
    pub fn children(self) -> Vec<GreenId> {
        match self.details {
            GreenNodeDetails::Token(_text) => Vec::new(),
            GreenNodeDetails::Node { children, .. } => children,
        }
    }
}

const INITIAL_CAPACITY: usize = 65536;
pub struct StringInterner {
    mapping: FxHashMap<&'static str, u32>,
    ptrs: Vec<&'static str>,
    buf: String,
    bufs: Vec<String>,
}
impl StringInterner {
    pub fn new() -> Self {
        Self {
            mapping: FxHashMap::default(),
            ptrs: Vec::with_capacity(INITIAL_CAPACITY),
            buf: String::with_capacity(INITIAL_CAPACITY),
            bufs: Vec::new(),
        }
    }
    pub fn intern(&mut self, text: &str) -> u32 {
        if let Some(ptr) = self.mapping.get(text) {
            return *ptr;
        }
        let current_capacity = self.buf.capacity();
        if self.buf.len() + text.len() > current_capacity {
            let capacity = (std::cmp::max(current_capacity, text.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(capacity);
            self.bufs.push(std::mem::replace(&mut self.buf, new_buf));
        };
        let start = self.buf.len();
        self.buf.push_str(text);
        let ptr = unsafe { &*(&self.buf[start..] as *const str) };
        let id = self.ptrs.len() as u32;
        self.mapping.insert(ptr, id);
        self.ptrs.push(ptr);
        id
    }

    pub fn lookup(&self, ptr: u32) -> &'static str {
        self.ptrs[ptr as usize]
    }
}
impl Default for StringInterner {
    fn default() -> Self {
        Self::new()
    }
}

pub struct GreenInterner {
    string_interner: StringInterner,
    mapping: FxHashMap<&'static [u32], u32>,
    ptrs: Vec<&'static [u32]>,
    buf: Vec<u32>,
    bufs: Vec<Vec<u32>>,
}
impl GreenInterner {
    pub fn new() -> Self {
        Self {
            string_interner: StringInterner::new(),
            mapping: FxHashMap::default(),
            ptrs: Vec::with_capacity(INITIAL_CAPACITY),
            buf: Vec::with_capacity(INITIAL_CAPACITY),
            bufs: Vec::new(),
        }
    }
    pub fn intern(&mut self, node: &GreenNode) -> GreenId {
        let vec = match &node.details {
            GreenNodeDetails::Token(s) => vec![node.kind as u32, self.string_interner.intern(s)],
            GreenNodeDetails::Node { children, width } => chain!(
                [node.kind as u32 + (1 << 31), width.0, children.len() as u32,],
                children.iter().map(|&id| id.0),
            )
            .collect(),
        };
        if let Some(ptr) = self.mapping.get(&vec[..]) {
            return GreenId(*ptr);
        }
        let current_capacity = self.buf.capacity();
        if self.buf.len() + vec.len() > current_capacity {
            let capacity = (std::cmp::max(current_capacity, vec.len()) + 1).next_power_of_two();
            let new_buf = Vec::with_capacity(capacity);
            self.bufs.push(std::mem::replace(&mut self.buf, new_buf));
        };
        let start = self.buf.len();
        self.buf.extend_from_slice(&vec);
        let ptr = unsafe { &*(&self.buf[start..] as *const [u32]) };
        let id = self.ptrs.len() as u32;
        self.mapping.insert(ptr, id);
        self.ptrs.push(ptr);
        GreenId(id)
    }

    pub fn lookup(&self, ptr: GreenId) -> GreenNode {
        let res = self.ptrs[ptr.0 as usize];
        if res[0] & (1 << 31) == 0 {
            return GreenNode {
                kind: unsafe { std::mem::transmute(res[0] as u16) },
                details: GreenNodeDetails::Token(self.string_interner.lookup(res[1]).into()),
            };
        }
        let width = TextWidth(res[1]);
        let children_len = res[2] as usize;
        let children = res[3..].iter().take(children_len).copied().map(GreenId);
        let children = children.collect();
        GreenNode {
            kind: unsafe { std::mem::transmute((res[0] & !(1 << 31)) as u16) },
            details: GreenNodeDetails::Node { children, width },
        }
    }
}
impl Default for GreenInterner {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct GreenId(u32);
impl GreenId {
    /// Returns the width of the node of this green id.
    pub fn width(&self, db: &dyn SyntaxGroup) -> TextWidth {
        match db.lookup_intern_green(*self).details {
            super::green::GreenNodeDetails::Token(text) => TextWidth::from_str(&text),
            super::green::GreenNodeDetails::Node { width, .. } => width,
        }
    }
}
