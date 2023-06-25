use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::TypedSyntaxNode;

use crate::plugins::InlineMacro;

pub struct IdentityMacro;
impl InlineMacro for IdentityMacro {
    fn append_macro_code(
        &self,
        macro_expander_data: &mut crate::plugins::MacroExpanderData,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        macro_ast: &cairo_lang_syntax::node::ast::ExprInlineMacro,
    ) {
        let args = macro_ast.arguments(db).args(db).elements(db);
        if args.len() != 1 {
            macro_expander_data.diagnostics.push(PluginDiagnostic {
                stable_ptr: macro_ast.stable_ptr().untyped(),
                message: "identity macro must have a single unnamed argument.".to_string(),
            });
            return;
        }

        if let cairo_lang_syntax::node::ast::ArgClause::Unnamed(arg) = args[0].arg_clause(db) {
            macro_expander_data.result_code.push_str(&arg.as_syntax_node().get_text(db));
        }
        macro_expander_data.code_changed = true;
    }
}
