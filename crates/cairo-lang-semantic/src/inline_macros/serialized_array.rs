use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::unsupported_bracket_diagnostic;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

#[derive(Debug, Default)]
pub struct SerializedArrayMacro;
impl NamedPlugin for SerializedArrayMacro {
    const NAME: &'static str = "serialized_array";
}
impl InlineMacroExprPlugin for SerializedArrayMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> InlinePluginResult {
        let ast::WrappedArgList::BracketedArgList(args) = syntax.arguments(db) else {
            return unsupported_bracket_diagnostic(db, syntax);
        };
        let mut builder = PatchBuilder::new(db);
        builder.add_str(
            "{
            let mut __serialized_array_builder_macro_result__ = core::array::ArrayTrait::new();",
        );
        for arg in args.arguments(db).elements(db) {
            builder.add_str("\n            core::Serde::serialize(@");
            builder.add_node(arg.as_syntax_node());
            builder.add_str(", ref __serialized_array_builder_macro_result__);");
        }
        builder.add_str(
            "\n            __serialized_array_builder_macro_result__
        }",
        );
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "serialized_array_inline_macro".into(),
                content: builder.code,
                code_mappings: builder.code_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
