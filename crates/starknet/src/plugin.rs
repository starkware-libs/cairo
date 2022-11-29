use defs::db::{MacroPlugin, PluginResult};
use syntax::node::ast::MaybeImplBody;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

static CONTRACT_IMPL_ATTR: &str = "ContractImpl";

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        if let ast::Item::Impl(impl_ast) = item_ast {
            let attrs = impl_ast.attributes(db).elements(db);
            for attr in &attrs {
                if attr.attr(db).text(db) == CONTRACT_IMPL_ATTR {
                    if let MaybeImplBody::Some(body) = impl_ast.body(db) {
                        return PluginResult {
                            code: Some((
                                "entry_points".into(),
                                body.items(db).as_syntax_node().get_text(db),
                            )),
                            diagnostics: vec![],
                        };
                    }
                }
            }
        }

        PluginResult { code: None, diagnostics: vec![] }
    }
}
