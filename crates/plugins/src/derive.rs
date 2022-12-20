use defs::plugin::{
    DynDiagnosticMapper, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
    TrivialMapper,
};
use syntax::node::ast::AttributeList;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

#[derive(Debug)]
pub struct DerivePlugin {}

impl MacroPlugin for DerivePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Struct(struct_ast) => {
                generate_derive_code_for_type(db, struct_ast.name(db), struct_ast.attributes(db))
            }
            ast::Item::Enum(enum_ast) => {
                generate_derive_code_for_type(db, enum_ast.name(db), enum_ast.attributes(db))
            }
            _ => PluginResult { code: None, diagnostics: vec![] },
        }
    }
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(
    db: &dyn SyntaxGroup,
    ident: ast::TerminalIdentifier,
    attributes: AttributeList,
) -> PluginResult {
    let mut impls = vec![];
    for attr in attributes.elements(db) {
        if attr.attr(db).text(db) == "derive" {
            // TODO(orizi): Add diagnostics for all the unexpected cases.
            if let ast::OptionAttributeArgs::AttributeArgs(args) = attr.args(db) {
                for arg in args.arg_list(db).elements(db) {
                    if let ast::Expr::Path(expr) = arg {
                        if let [ast::PathSegment::Simple(segment)] = &expr.elements(db)[..] {
                            let name = ident.text(db);
                            let derived = segment.ident(db).text(db);
                            impls.push(format!("impl {name}{derived} of {derived}::<{name}>;\n"));
                        } else {
                            return PluginResult {
                                code: None,
                                diagnostics: vec![PluginDiagnostic {
                                    stable_ptr: expr.stable_ptr().untyped(),
                                    message: "Expected a single segment.".into(),
                                }],
                            };
                        }
                    } else {
                        return PluginResult {
                            code: None,
                            diagnostics: vec![PluginDiagnostic {
                                stable_ptr: arg.stable_ptr().untyped(),
                                message: "Expected path.".into(),
                            }],
                        };
                    }
                }
            } else {
                return PluginResult {
                    code: None,
                    diagnostics: vec![PluginDiagnostic {
                        stable_ptr: attr.args(db).stable_ptr().untyped(),
                        message: "Expected args.".into(),
                    }],
                };
            }
        }
    }
    if impls.is_empty() {
        PluginResult { code: None, diagnostics: vec![] }
    } else {
        PluginResult {
            code: Some(PluginGeneratedFile {
                name: "impls".into(),
                content: impls.join(""),
                diagnostic_mapper: DynDiagnosticMapper::new(TrivialMapper {}),
            }),
            diagnostics: vec![],
        }
    }
}
