use crate::span::TextSpan;

#[derive(Debug, PartialEq, Eq)]
pub struct Patch {
    pub span: TextSpan,
    pub origin_span: TextSpan,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct Patches {
    pub patches: Vec<Patch>,
}
impl Patches {
    pub fn translate(&self, span: TextSpan) -> Option<TextSpan> {
        for Patch { span: patch_span, origin_span } in &self.patches {
            if patch_span.contains(span) {
                let start = origin_span.start.add_width(span.start - patch_span.start);
                return Some(TextSpan { start, end: start.add_width(span.end - span.start) });
            }
        }
        None
    }
}
