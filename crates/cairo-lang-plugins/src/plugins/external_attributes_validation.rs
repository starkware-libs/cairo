use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use itertools::Itertools;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ExternalAttributesValidationPlugin;

pub const DOC_ATTR: &str = "doc";
const HIDDEN_ATTR: &str = "hidden";
pub const HIDDEN_ATTR_SYNTAX: &str = "#[doc(hidden)]";
const GROUP_ATTR: &str = "group";
const GROUP_ATTR_SYNTAX: &str = "#[doc(group: \"group name\")]";

impl MacroPlugin for ExternalAttributesValidationPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        match get_diagnostics(db, &item_ast) {
            Some(diagnostics) => {
                PluginResult { code: None, remove_original_item: false, diagnostics }
            }
            None => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![DOC_ATTR.to_string()]
    }
}

fn get_diagnostics<Item: QueryAttrs>(
    db: &dyn SyntaxGroup,
    item: &Item,
) -> Option<Vec<PluginDiagnostic>> {
    let mut diagnostics: Vec<PluginDiagnostic> = Vec::new();
    item.query_attr(db, DOC_ATTR).for_each(|attr| {
        let args = attr.clone().structurize(db).args;
        if args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(db),
                format!("Expected arguments. Supported args: {HIDDEN_ATTR}, {GROUP_ATTR}."),
            ));
            return;
        }
        args.iter().for_each(|arg| match &arg.variant {
            AttributeArgVariant::Unnamed(value) => {
                let ast::Expr::Path(path) = value else {
                    diagnostics.push(PluginDiagnostic::error(
                        value.stable_ptr(db),
                        format!("Expected identifier. Supported identifiers: {HIDDEN_ATTR}."),
                    ));
                    return;
                };
                let Some([ast::PathSegment::Simple(segment)]) =
                    path.segments(db).elements(db).collect_array()
                else {
                    diagnostics.push(PluginDiagnostic::error(
                        path.stable_ptr(db),
                        format!(
                            "Wrong type of argument. Currently only {HIDDEN_ATTR_SYNTAX} is \
                             supported."
                        ),
                    ));
                    return;
                };
                if segment.ident(db).text(db) != HIDDEN_ATTR {
                    diagnostics.push(PluginDiagnostic::error(
                        path.stable_ptr(db),
                        format!(
                            "Wrong type of argument. Currently only: {HIDDEN_ATTR_SYNTAX}, \
                             {GROUP_ATTR_SYNTAX}  are supported."
                        ),
                    ));
                }
            }
            AttributeArgVariant::Named { name, value } => match value {
                ast::Expr::String(_) => {
                    if name.text != GROUP_ATTR {
                        diagnostics.push(PluginDiagnostic::error(
                            arg.arg.stable_ptr(db),
                            format!(
                                "This argument is not supported. Supported args: {HIDDEN_ATTR}, \
                                 {GROUP_ATTR}."
                            ),
                        ));
                    }
                }
                _ => {
                    diagnostics.push(PluginDiagnostic::error(
                        value.stable_ptr(db),
                        format!(
                            "Wrong type of argument. Currently only {GROUP_ATTR_SYNTAX} is \
                             supported."
                        ),
                    ));
                }
            },
            _ => diagnostics.push(PluginDiagnostic::error(
                arg.arg.stable_ptr(db),
                format!(
                    "This argument is not supported. Supported args: {HIDDEN_ATTR}, {GROUP_ATTR}."
                ),
            )),
        });
    });
    if diagnostics.is_empty() { None } else { Some(diagnostics) }
}
