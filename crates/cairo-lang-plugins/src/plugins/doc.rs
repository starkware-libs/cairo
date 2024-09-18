use cairo_lang_defs::plugin::{MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginResult};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct DocPlugin;

const DOC_ATTR: &str = "doc";

impl MacroPlugin for DocPlugin {
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
            diagnostics
                .push(PluginDiagnostic::error(attr.stable_ptr(), "Expected arguments.".into()));
            return;
        }
        args.iter().for_each(|arg| match &arg.variant {
            AttributeArgVariant::Unnamed(value) => {
                let ast::Expr::Path(path) = value else {
                    diagnostics.push(PluginDiagnostic::error(value, "Expected identifier.".into()));
                    return;
                };
                let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] else {
                    diagnostics.push(PluginDiagnostic::error(path, "Expected simple path.".into()));
                    return;
                };
                if segment.ident(db).text(db) != "hidden" {
                    diagnostics.push(PluginDiagnostic::error(
                        path,
                        "This argument is not supported.".into(),
                    ));
                }
            }
            _ => diagnostics
                .push(PluginDiagnostic::error(&arg.arg, "This argument is not supported.".into())),
        });
    });
    if diagnostics.is_empty() { None } else { Some(diagnostics) }
}
