use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ExternalAttributesValidationPlugin;

const DOC_ATTR: &str = "doc";
const SUPPORTED_ARGS_ERROR_MESSAGE: &str = "hidden";

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
    item.query_attr(db, DOC_ATTR).into_iter().for_each(|attr| {
        let args = attr.clone().structurize(db).args;
        if args.is_empty() {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr(),
                format!("Expected arguments. Supported args: {}", SUPPORTED_ARGS_ERROR_MESSAGE),
            ));
            return;
        }
        args.iter().for_each(|arg| match &arg.variant {
            AttributeArgVariant::Unnamed(value) => {
                let ast::Expr::Path(path) = value else {
                    diagnostics.push(PluginDiagnostic::error(
                        value,
                        format!(
                            "Expected identifier. Supported identifiers: {}",
                            SUPPORTED_ARGS_ERROR_MESSAGE
                        ),
                    ));
                    return;
                };
                let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                    diagnostics.push(PluginDiagnostic::error(
                        path,
                        format!(
                            "Expected simple path. Supported paths: {}",
                            SUPPORTED_ARGS_ERROR_MESSAGE
                        ),
                    ));
                    return;
                };
                if segment.ident(db).text(db) != "hidden" {
                    diagnostics.push(PluginDiagnostic::error(
                        path,
                        format!(
                            "This argument is not supported. Supported args: {}",
                            SUPPORTED_ARGS_ERROR_MESSAGE
                        ),
                    ));
                }
            }
            _ => diagnostics.push(PluginDiagnostic::error(
                &arg.arg,
                format!(
                    "This argument is not supported. Supported args: {}",
                    SUPPORTED_ARGS_ERROR_MESSAGE
                ),
            )),
        });
    });
    if diagnostics.is_empty() { None } else { Some(diagnostics) }
}
