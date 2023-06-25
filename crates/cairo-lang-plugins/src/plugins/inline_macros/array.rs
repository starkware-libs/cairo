use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::TypedSyntaxNode;

use crate::plugins::InlineMacro;

pub struct ArrayMacro;
impl InlineMacro for ArrayMacro {
    fn append_macro_code(
        &self,
        macro_expander_data: &mut crate::plugins::MacroExpanderData,
        db: &dyn cairo_lang_syntax::node::db::SyntaxGroup,
        macro_ast: &cairo_lang_syntax::node::ast::ExprInlineMacro,
    ) {
        let args = macro_ast.arguments(db).args(db).elements(db);
        let mut expanded_code = "{
                let mut _temp_array = ArrayTrait::new();
            "
        .to_string();
        for arg in args {
            match arg.arg_clause(db) {
                cairo_lang_syntax::node::ast::ArgClause::Unnamed(arg) => {
                    expanded_code.push_str(&format!(
                        "   _temp_array.append({});
                        ",
                        arg.as_syntax_node().get_text(db)
                    ));
                }
                _ => {
                    macro_expander_data.diagnostics.push(PluginDiagnostic {
                        stable_ptr: macro_ast.stable_ptr().untyped(),
                        message: "array macro can only have unnamed arguments.".to_string(),
                    });
                    return;
                }
            }
        }
        expanded_code.push_str(
            "
                _temp_array
            }",
        );
        macro_expander_data.result_code.push_str(&expanded_code);
        macro_expander_data.code_changed = true;
    }
}
