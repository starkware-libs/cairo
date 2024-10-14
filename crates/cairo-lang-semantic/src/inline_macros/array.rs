use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{
    PluginResultTrait, not_legacy_macro_diagnostic, unsupported_bracket_diagnostic,
};
use cairo_lang_parser::macro_helpers::AsLegacyInlineMacro;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};

#[derive(Debug, Default)]
pub struct ArrayMacro;
impl NamedPlugin for ArrayMacro {
    const NAME: &'static str = "array";
}
impl InlineMacroExprPlugin for ArrayMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        let Some(legacy_inline_macro) = syntax.as_legacy_inline_macro(db) else {
            return InlinePluginResult::diagnostic_only(not_legacy_macro_diagnostic(
                syntax.as_syntax_node().stable_ptr(),
            ));
        };
        let ast::WrappedArgList::BracketedArgList(args) = legacy_inline_macro.arguments(db) else {
            return unsupported_bracket_diagnostic(db, &legacy_inline_macro, syntax);
        };
        let mut builder = PatchBuilder::new(db, syntax);
        builder.add_str(
            "{
            let mut __array_builder_macro_result__ = core::array::ArrayTrait::new();",
        );
        for arg in args.arguments(db).elements(db) {
            builder.add_str(
                "\n            core::array::ArrayTrait::append(ref __array_builder_macro_result__,",
            );
            builder.add_node(arg.as_syntax_node());
            builder.add_str(");");
        }
        builder.add_str(
            "\n            __array_builder_macro_result__
        }",
        );
        let (content, code_mappings) = builder.build();
        InlinePluginResult {
            code: Some(PluginGeneratedFile {
                name: "array_inline_macro".into(),
                content,
                code_mappings,
                aux_data: None,
            }),
            diagnostics: vec![],
        }
    }
}
