use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_syntax::node::ast::{
    AttributeListGreen, ExprInlineMacro, ExprPathGreen, ItemInlineMacro, LegacyExprInlineMacro,
    LegacyItemInlineMacro, TerminalIdentifierGreen, TerminalNotGreen, TerminalSemicolonGreen,
    TokenTreeNode, WrappedArgListGreen,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::ParserDiagnostic;
use crate::diagnostic::ParserDiagnosticKind;
use crate::lexer::Lexer;
use crate::parser::{Parser, SkippedError};
use crate::recovery::is_of_kind;

pub fn token_tree_as_wrapped_arg_list(
    token_tree: TokenTreeNode,
    db: &dyn SyntaxGroup,
) -> Option<WrappedArgListGreen> {
    let mut diagnostics: DiagnosticsBuilder<ParserDiagnostic> = DiagnosticsBuilder::default();
    let node_text = token_tree.as_syntax_node().get_text(db);
    let file_id = token_tree.stable_ptr().0.file_id(db);
    let mut lexer = Lexer::from_text(db, &node_text);
    let next_terminal = lexer.next().unwrap();
    // println!("Offset: {:?}", token_tree.stable_ptr().0.lookup(db).offset());
    let mut parser = Parser {
        db,
        file_id,
        lexer,
        next_terminal,
        pending_trivia: Vec::new(),
        offset: Default::default(),
        current_width: Default::default(),
        last_trivia_length: Default::default(),
        diagnostics: &mut diagnostics,
        pending_skipped_token_diagnostics: Default::default(),
    };
    let wrapped_arg_list_green = parser.parse_wrapped_arg_list();
    if let Err(SkippedError(span)) = parser.skip_until(is_of_kind!()) {
        parser.diagnostics.add(ParserDiagnostic {
            file_id: parser.file_id,
            kind: ParserDiagnosticKind::SkippedElement { element_name: "end arg list".into() },
            span,
        });
    };
    let diagnostics = diagnostics.build();
    if !diagnostics.get_all().is_empty() {
        return None;
    }
    Some(wrapped_arg_list_green)
}

pub trait AsLegacyInlineMacro {
    type LegacyType;
    fn as_legacy_inline_macro(&self, db: &dyn SyntaxGroup) -> Option<Self::LegacyType>;
}

impl AsLegacyInlineMacro for ExprInlineMacro {
    type LegacyType = LegacyExprInlineMacro;

    fn as_legacy_inline_macro(&self, db: &dyn SyntaxGroup) -> Option<Self::LegacyType> {
        let green_node = self.as_syntax_node().green_node(db);
        let [macro_name, bang, _arguments] = green_node.children() else {
            return None;
        };
        let macro_name = ExprPathGreen(*macro_name);
        let bang = TerminalNotGreen(*bang);
        let wrapped_arg_list = token_tree_as_wrapped_arg_list(self.arguments(db), db)?;
        let legacy_green = LegacyExprInlineMacro::new_green(db, macro_name, bang, wrapped_arg_list);
        let file_id = self.stable_ptr().0.file_id(db);
        let offset = self.stable_ptr().0.lookup(db).offset();
        Some(LegacyExprInlineMacro::from_syntax_node(
            db,
            SyntaxNode::new_root_with_offset(db, file_id, legacy_green.0, offset),
        ))
    }
}

impl AsLegacyInlineMacro for ItemInlineMacro {
    type LegacyType = LegacyItemInlineMacro;

    fn as_legacy_inline_macro(&self, db: &dyn SyntaxGroup) -> Option<Self::LegacyType> {
        let green_node = self.as_syntax_node().green_node(db);
        let [attributes, macro_name, bang, _arguments, semicolon] = green_node.children() else {
            return None;
        };
        let attributes = AttributeListGreen(*attributes);
        let macro_name = TerminalIdentifierGreen(*macro_name);
        let bang = TerminalNotGreen(*bang);
        let wrapped_arg_list = token_tree_as_wrapped_arg_list(self.arguments(db), db)?;
        let semicolon = TerminalSemicolonGreen(*semicolon);
        let legacy_green = LegacyItemInlineMacro::new_green(
            db,
            attributes,
            macro_name,
            bang,
            wrapped_arg_list,
            semicolon,
        );
        let file_id = self.stable_ptr().0.file_id(db);
        let offset = self.stable_ptr().0.lookup(db).offset();
        Some(LegacyItemInlineMacro::from_syntax_node(
            db,
            SyntaxNode::new_root_with_offset(db, file_id, legacy_green.0, offset),
        ))
    }
}
