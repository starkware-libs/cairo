use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_syntax::node::ast::{
    self, AttributeListGreen, ExprInlineMacro, ExprPathGreen, ItemInlineMacro,
    LegacyExprInlineMacro, LegacyItemInlineMacro, TerminalIdentifierGreen, TerminalNotGreen,
    TerminalSemicolonGreen, TokenTree, TokenTreeNode, WrappedArgListGreen,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

use crate::ParserDiagnostic;
use crate::diagnostic::ParserDiagnosticKind;
use crate::parser::{Parser, SkippedError};
use crate::recovery::is_of_kind;

/// Takes a token tree syntax node, which is assumed to be parsable as a wrapped argument list, try
/// to parse it as such and return the result.
pub fn token_tree_as_wrapped_arg_list<'a>(
    token_tree: TokenTreeNode<'a>,
    db: &'a dyn SyntaxGroup,
) -> Option<WrappedArgListGreen<'a>> {
    let mut diagnostics: DiagnosticsBuilder<ParserDiagnostic<'a>> = DiagnosticsBuilder::default();
    let node_text = token_tree.as_syntax_node().get_text(db);
    let file_id = token_tree.stable_ptr(db).0.file_id(db);
    let mut parser = Parser::new(db, file_id, &node_text, &mut diagnostics);
    let wrapped_arg_list_green: WrappedArgListGreen<'a> = parser.parse_wrapped_arg_list();
    if let Err(SkippedError(span)) = parser.skip_until(is_of_kind!()) {
        parser.add_diagnostic(
            ParserDiagnosticKind::SkippedElement { element_name: "end arg list".into() },
            span,
        );
    };
    let diagnostics = diagnostics.build();
    if !diagnostics.get_all().is_empty() {
        return None;
    }
    Some(wrapped_arg_list_green)
}

/// Takes a token tree syntax node, which is assumed to be parsable as an expression (it assumes
/// that the prefix is an expr, not the whole iterator), tries to parse it as such, and returns the
/// result. The token tree iterator is consumed entirely.
pub fn as_expr_macro_token_tree<'a>(
    token_tree: impl Iterator<Item = TokenTree<'a>>,
    file_id: FileId<'a>,
    db: &'a dyn SyntaxGroup,
) -> Option<ast::Expr<'a>> {
    let mut diagnostics: DiagnosticsBuilder<ParserDiagnostic> = DiagnosticsBuilder::default();
    let node_text: String = token_tree
        .map(|token| token.as_syntax_node().get_text(db).to_string())
        .collect::<Vec<String>>()
        .join("");
    let mut parser = Parser::new(db, file_id, &node_text, &mut diagnostics);
    let expr_green = parser.parse_expr();
    let expr = ast::Expr::from_syntax_node(db, SyntaxNode::new_root(db, file_id, expr_green.0));
    Some(expr)
}

/// Trait for converting inline macros with token tree syntax as the argument to legacy inline which
/// must have a wrapped argument list syntax node.
pub trait AsLegacyInlineMacro<'a> {
    /// The corresponding legacy inline macro type.
    type LegacyType;
    /// Converts the inline macro to the legacy inline macro.
    fn as_legacy_inline_macro(&self, db: &'a dyn SyntaxGroup) -> Option<Self::LegacyType>;
}

impl<'a> AsLegacyInlineMacro<'a> for ExprInlineMacro<'a> {
    type LegacyType = LegacyExprInlineMacro<'a>;

    fn as_legacy_inline_macro(&self, db: &'a dyn SyntaxGroup) -> Option<Self::LegacyType> {
        let green_node = self.as_syntax_node().green_node(db);
        let [macro_name, bang, _arguments] = green_node.children() else {
            return None;
        };
        let macro_name = ExprPathGreen(*macro_name);
        let bang = TerminalNotGreen(*bang);
        let wrapped_arg_list = token_tree_as_wrapped_arg_list(self.arguments(db), db)?;
        let legacy_green = LegacyExprInlineMacro::new_green(db, macro_name, bang, wrapped_arg_list);
        let file_id = self.stable_ptr(db).0.file_id(db);
        let offset = self.stable_ptr(db).0.lookup(db).offset(db);
        Some(LegacyExprInlineMacro::from_syntax_node(
            db,
            SyntaxNode::new_root_with_offset(db, file_id, legacy_green.0, Some(offset)),
        ))
    }
}

impl<'a> AsLegacyInlineMacro<'a> for ItemInlineMacro<'a> {
    type LegacyType = LegacyItemInlineMacro<'a>;

    fn as_legacy_inline_macro(&self, db: &'a dyn SyntaxGroup) -> Option<Self::LegacyType> {
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
        let file_id = self.stable_ptr(db).0.file_id(db);
        let offset = self.stable_ptr(db).0.lookup(db).offset(db);
        Some(LegacyItemInlineMacro::from_syntax_node(
            db,
            SyntaxNode::new_root_with_offset(db, file_id, legacy_green.0, Some(offset)),
        ))
    }
}
