use cairo_lang_defs::patcher::PatchBuilder;
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
        let mut builder = PatchBuilder::new(db);
        let args = args.expressions(db).elements(db);
        builder.add_str(
            "{
            let mut __array_builder_macro_result__ = ArrayTrait::new();",
        );
        for arg in args {
            builder.add_str(
                "\n            array::ArrayTrait::append(ref __array_builder_macro_result__, ",
            );
            builder.add_node(arg.as_syntax_node());
            builder.add_str(");");
        }
        builder.add_str(
            "\n            __array_builder_macro_result__
        }",
        );
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "array_inline_macro".into(),
                content: builder.code,
                diagnostics_mappings: builder.diagnostics_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
