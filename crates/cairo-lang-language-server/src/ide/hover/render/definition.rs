use cairo_lang_defs::db::DefsGroup;
use cairo_lang_doc::db::DocGroup;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::node::ast::TerminalIdentifier;
use cairo_lang_utils::Upcast;
use lsp_types::Hover;

use crate::ide::hover::markdown_contents;
use crate::ide::hover::render::markdown::{RULE, fenced_code_block};
use crate::lang::db::AnalysisDatabase;
use crate::lang::inspect::defs::{MemberDef, SymbolDef};
use crate::lang::lsp::ToLsp;

/// Get declaration and documentation "definition" of an item referred by the given identifier.
pub fn definition(
    db: &AnalysisDatabase,
    identifier: &TerminalIdentifier,
    file_id: FileId,
) -> Option<Hover> {
    let symbol = SymbolDef::find(db, identifier)?;

    let md = match &symbol {
        SymbolDef::Item(item) => {
            let mut md = String::new();
            md += &fenced_code_block(&item.definition_path(db));
            md += &fenced_code_block(&item.signature(db));
            if let Some(doc) = item.documentation(db) {
                md += RULE;
                md += &doc;
            }
            md
        }

        SymbolDef::Module(module) => {
            let mut md = String::new();
            md += &fenced_code_block(&module.definition_path());
            md += &fenced_code_block(&module.signature());
            if let Some(doc) = module.documentation(db) {
                md += RULE;
                md += &doc;
            }
            md
        }

        SymbolDef::Variable(var) => fenced_code_block(&var.signature(db)),
        SymbolDef::ExprInlineMacro(macro_name) => {
            let mut md = fenced_code_block(macro_name);
            if let Some(doc) = db.inline_macro_plugins().get(macro_name)?.documentation() {
                md += RULE;
                md += &doc;
            }
            md
        }
        SymbolDef::Member(MemberDef { member, structure }) => {
            let mut md = String::new();

            // Signature is the signature of the struct, so it makes sense that the definition
            // path is too.
            md += &fenced_code_block(&structure.definition_path(db));
            md += &fenced_code_block(&structure.signature(db));

            if let Some(doc) = db.get_item_documentation((*member).into()) {
                md += RULE;
                md += &doc;
            }
            md
        }
    };

    Some(Hover {
        contents: markdown_contents(md),
        range: identifier
            .as_syntax_node()
            .span_without_trivia(db.upcast())
            .position_in_file(db.upcast(), file_id)
            .map(|p| p.to_lsp()),
    })
}
