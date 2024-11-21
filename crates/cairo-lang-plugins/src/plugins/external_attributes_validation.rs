use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};

#[derive(Debug, Default, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub struct ExternalAttributesValidationPlugin;

const DOC_ATTR: &str = "doc";
const HIDDEN_ATTR: &str = "hidden";

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
                format!("Expected arguments. Supported args: {}", HIDDEN_ATTR),
            ));
            return;
        }
        args.iter().for_each(|arg| match &arg.variant {
            AttributeArgVariant::Unnamed(value) => {
                let ast::Expr::Path(path) = value else {
                    diagnostics.push(PluginDiagnostic::error(
                        value,
                        format!("Expected identifier. Supported identifiers: {}", HIDDEN_ATTR),
                    ));
                    return;
                };
                let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                    diagnostics.push(PluginDiagnostic::error(
                        path,
                        "Wrong type of argument. Currently only #[doc(hidden)] is supported."
                            .to_owned(),
                    ));
                    return;
                };
                if segment.ident(db).text(db) != HIDDEN_ATTR {
                    diagnostics.push(PluginDiagnostic::error(
                        path,
                        "Wrong type of argument. Currently only #[doc(hidden)] is supported."
                            .to_owned(),
                    ));
                }
            }
            _ => diagnostics.push(PluginDiagnostic::error(
                &arg.arg,
                format!("This argument is not supported. Supported args: {}", HIDDEN_ATTR),
            )),
        });
    });
    if diagnostics.is_empty() { None } else { Some(diagnostics) }
}
