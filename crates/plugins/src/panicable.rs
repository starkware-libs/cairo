use defs::db::{MacroPlugin, PluginDiagnostic, PluginResult};
use itertools::Itertools;
use syntax::node::ast::AttributeList;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::try_extract_matches;

#[derive(Debug)]
pub struct PanicablePlugin {}

impl MacroPlugin for PanicablePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::ExternFunction(extern_func_ast) => generate_panicable_code(
                db,
                extern_func_ast.name(db),
                extern_func_ast.signature(db),
                extern_func_ast.attributes(db),
            ),
            ast::Item::FreeFunction(free_func_ast) => generate_panicable_code(
                db,
                free_func_ast.name(db),
                free_func_ast.signature(db),
                free_func_ast.attributes(db),
            ),
            _ => PluginResult { code: None, diagnostics: vec![] },
        }
    }
}

/// Adds an implementation for all requested derives for the type.
fn generate_panicable_code(
    db: &dyn SyntaxGroup,
    ident: ast::TerminalIdentifier,
    signature: ast::FunctionSignature,
    attributes: AttributeList,
) -> PluginResult {
    for attr in attributes.elements(db) {
        if attr.attr(db).text(db) != "panic_with" {
            continue;
        }
        // TODO(orizi): Add diagnostics for all the unexpected cases.
        if !matches!(
            signature.optional_no_panic(db),
            ast::OptionTerminalNoPanic::TerminalNoPanic(_)
        ) {
            // Only nonpanic functions can be wrapped.
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: signature.stable_ptr().untyped(),
                    message: "Only nonpanic functions can be wrapped".into(),
                }],
            };
        }

        let Some(inner_ty_text) = extract_option_ty(db, &signature) else {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: signature.stable_ptr().untyped(),
                    message: "Currently only wrapping functions returning an Option<T>".into(),
                }],
            };
        };

        let Some((err_value, panicable_name)) = try_extract_matches!(attr.args(db), ast::OptionAttributeArgs::AttributeArgs).and_then(
            |args| {
            if let [ast::Expr::Literal(err_value), ast::Expr::Path(name)] = &args.arg_list(db).elements(db)[..] {
                if let [ast::PathSegment::Simple(segment)] = &name.elements(db)[..] {
                    Some((err_value.text(db), segment.ident(db).text(db)))
                } else {
                    None
                }
            } else {
                None
            }
        }) else {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: signature.stable_ptr().untyped(),
                    message: "Failed to extract panic data attribute".into(),
                }],
            };
        };

        let function_name = ident.text(db);
        let params = signature.parameters(db).as_syntax_node().get_text(db);
        let args = signature
            .parameters(db)
            .elements(db)
            .into_iter()
            .map(|param| param.name(db).as_syntax_node().get_text(db))
            .join(", ");
        return PluginResult {
            code: Some((
                "panicable".into(),
                indoc::formatdoc!(
                    r#"
                    func {panicable_name}({params}) -> {inner_ty_text} {{
                        match {function_name}({args}) {{
                            Option::Some (v) => {{
                                v
                            }},
                            Option::None (v) => {{
                                let data = array_new::<felt>();
                                array_append::<felt>(data, {err_value});
                                panic(data)
                            }},
                        }}
                    }}
                "#
                ),
            )),
            diagnostics: vec![],
        };
    }
    PluginResult { code: None, diagnostics: vec![] }
}

/// Given a function signature, if it returns `Option::<T>`, returns T. Otherwise, returns None.
fn extract_option_ty(db: &dyn SyntaxGroup, signature: &ast::FunctionSignature) -> Option<String> {
    let ret_ty_expr =
        try_extract_matches!(signature.ret_ty(db), ast::OptionReturnTypeClause::ReturnTypeClause)?
            .ty(db);
    let ret_ty_path = try_extract_matches!(ret_ty_expr, ast::Expr::Path)?;

    // Currently only wrapping functions returning an Option<T>.
    let [ast::PathSegment::WithGenericArgs(segment)] = &ret_ty_path.elements(db)[..] else {
        return None;
    };
    if segment.ident(db).text(db) != "Option" {
        return None;
    }

    Some(segment.generic_args(db).generic_args(db).as_syntax_node().get_text(db))
}
