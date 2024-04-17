use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_syntax::attribute::consts::EXECUTABLE_ATTR;
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

/// Plugin that validates the `executable` attribute arguments format.
/// The attributes will be utilized later on.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ExecutablePlugin;

impl MacroPlugin for ExecutablePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let mut diagnostics = vec![];
        match item_ast {
            ast::ModuleItem::FreeFunction(function_with_body) => {
                function_with_body.query_attr(db, EXECUTABLE_ATTR).iter().for_each(|attr| {
                    let structured = attr.clone().structurize(db);
                    if structured.args.len() > 1 {
                        diagnostics.push(PluginDiagnostic::error(
                            attr.as_syntax_node().stable_ptr(),
                            format!(
                                "The `{EXECUTABLE_ATTR}` attribute can have at most one argument, \
                                 found `{}`.",
                                structured.args.len()
                            ),
                        ));
                    }
                    if let Some(arg) = structured.args.first() {
                        if !matches!(arg.variant, AttributeArgVariant::Unnamed { .. }) {
                            diagnostics.push(PluginDiagnostic::error(
                                attr.as_syntax_node().stable_ptr(),
                                format!(
                                    "The `{EXECUTABLE_ATTR}` attribute can only accept an unnamed \
                                     argument, found `{}`.",
                                    arg.text(db)
                                ),
                            ));
                        }
                    }
                });
            }
            item_ast => {
                // Executables can only be used on free functions.
                // Emit errors for all other uses.
                let attrs = item_ast.query_attr(db, EXECUTABLE_ATTR);
                for attr in attrs {
                    diagnostics.push(PluginDiagnostic::error(
                        attr.as_syntax_node().stable_ptr(),
                        format!(
                            "The `{EXECUTABLE_ATTR}` attribute is only allowed on free functions."
                        ),
                    ));
                }
            }
        }
        PluginResult { code: None, diagnostics, remove_original_item: false }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![EXECUTABLE_ATTR.to_string()]
    }
}
