use cairo_lang_syntax::node::TypedSyntaxNode;

use crate::plugins::InlineMacro;

pub struct ArrayMacro;
impl InlineMacro for ArrayMacro {
    fn append_macro_code(
        &self,
        macro_expander_data: &mut crate::plugins::InlineMacroExpanderData,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        macro_arguments: &cairo_lang_syntax::node::ast::ExprList,
    ) {
        let args = macro_arguments.elements(db);
        let mut expanded_code = "{
            let mut __array_builder_macro_result__ = ArrayTrait::new();"
            .to_string();
        for arg in args {
            expanded_code.push_str(&format!(
                "\n            __array_builder_macro_result__.append({});",
                arg.as_syntax_node().get_text(db)
            ));
        }
        expanded_code.push_str(
            "\n            __array_builder_macro_result__
        }",
        );
        macro_expander_data.result_code.push_str(&expanded_code);
        macro_expander_data.code_changed = true;
    }

    fn is_bracket_type_allowed(
        &self,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        macro_ast: &cairo_lang_syntax::node::ast::ExprInlineMacro,
    ) -> bool {
        matches!(
            macro_ast.arguments(db),
            cairo_lang_syntax::node::ast::WrappedExprList::BracketedExprList(_)
        )
    }
}
