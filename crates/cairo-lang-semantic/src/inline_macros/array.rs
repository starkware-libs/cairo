use cairo_lang_defs::plugin::{InlineMacroExprPlugin, InlinePluginResult, PluginGeneratedFile};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

use super::unsupported_bracket_diagnostic;

#[derive(Debug)]
pub struct ArrayMacro;
impl InlineMacroExprPlugin for ArrayMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let ast::WrappedExprList::BracketedExprList(args) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let args = args.expressions(db).elements(db);
        let mut code = "{
            let mut __array_builder_macro_result__ = ArrayTrait::new();"
            .to_string();
        for arg in args {
            code.push_str(&format!(
                "\n            array::ArrayTrait::append(ref __array_builder_macro_result__, {});",
                arg.as_syntax_node().get_text(db)
            ));
        }
        code.push_str(
            "\n            __array_builder_macro_result__
        }",
        );
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "array_inline_macro".into(),
                content: code,
                diagnostics_mappings: vec![],
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
