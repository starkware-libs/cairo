use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_starknet::contract::starknet_keccak;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

pub struct Query {
    pub rewrite_nodes: Vec<RewriteNode>,
    pub diagnostics: Vec<PluginDiagnostic>,
}

impl Query {
    pub fn from_expr(db: &dyn SyntaxGroup, expr: ast::Expr) -> Self {
        let diagnostics = vec![];
        let rewrite_nodes: Vec<RewriteNode> = vec![];
        let mut query = Query { diagnostics, rewrite_nodes };

        match expr {
            ast::Expr::Path(path) => match &path.elements(db)[0] {
                ast::PathSegment::WithGenericArgs(segment) => {
                    let generic = segment.generic_args(db);
                    let parameters = generic.generic_args(db).elements(db);
                    for parameter in parameters {
                        query.handle_parameter(db, parameter);
                    }
                }
                _ => {
                    query.diagnostics.push(PluginDiagnostic {
                        message: "Invalid query type".to_string(),
                        stable_ptr: path.stable_ptr().untyped(),
                    });
                }
            },
            _ => {
                query.diagnostics.push(PluginDiagnostic {
                    message: "Invalid query type".to_string(),
                    stable_ptr: expr.stable_ptr().untyped(),
                });
            }
        }
        query
    }

    fn handle_parameter(&mut self, db: &dyn SyntaxGroup, parameter: ast::Expr) {
        match parameter {
            ast::Expr::Tuple(tuple) => {
                for element in tuple.expressions(db).elements(db) {
                    self.handle_parameter(db, element);
                }
            }

            ast::Expr::Path(path) => {
                let var_prefix = match path.elements(db).last() {
                    Some(segment) => segment.as_syntax_node().get_text(db).to_ascii_lowercase(),
                    None => {
                        return self.diagnostics.push(PluginDiagnostic {
                            message: "Resolving query name.".to_string(),
                            stable_ptr: path.stable_ptr().untyped(),
                        });
                    }
                };

                // TODO: Properly compute component id
                let component_id = format!(
                    "{:#x}",
                    starknet_keccak(path.as_syntax_node().get_text(db).as_bytes())
                );

                self.rewrite_nodes.push(RewriteNode::interpolate_patched(
                    "let $var_prefix$_ids = IWorld.lookup(world, $component_address$);",
                    HashMap::from([
                        ("var_prefix".to_string(), RewriteNode::Text(var_prefix)),
                        ("component_address".to_string(), RewriteNode::Text(component_id)),
                    ]),
                ))
            }
            _ => {
                return self.diagnostics.push(PluginDiagnostic {
                    message: "Unsupported query type. Must be tuple or single struct.".to_string(),
                    stable_ptr: parameter.stable_ptr().untyped(),
                });
            }
        }
    }
}
