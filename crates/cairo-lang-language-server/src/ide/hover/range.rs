use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_utils::Upcast;
use tower_lsp::lsp_types::Range;

use crate::lang::db::AnalysisDatabase;
use crate::lang::lsp::ToLsp;

pub(super) trait HoverRange {
    fn range(&self, db: &AnalysisDatabase, file_id: FileId) -> Option<Range>;
}

impl<T: TypedSyntaxNode> HoverRange for T {
    fn range(&self, db: &AnalysisDatabase, file_id: FileId) -> Option<Range> {
        self.as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|p| p.to_lsp())
    }
}
