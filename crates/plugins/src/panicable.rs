use defs::db::MacroPlugin;
use itertools::Itertools;
use parser::utils::get_node_text;
use syntax::node::ast::AttributeList;
use syntax::node::db::SyntaxGroup;
use syntax::node::{ast, Terminal, TypedSyntaxNode};
use utils::try_extract_matches;

#[derive(Debug)]
pub struct PanicablePlugin {}

impl MacroPlugin for PanicablePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::Item,
    ) -> Option<(smol_str::SmolStr, String)> {
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
            _ => None,
        }
    }
}

/// Adds an implementation for all requested derives for the type.
fn generate_panicable_code(
    db: &dyn SyntaxGroup,
    ident: ast::TerminalIdentifier,
    signature: ast::FunctionSignature,
    attributes: AttributeList,
) -> Option<(smol_str::SmolStr, String)> {
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
            return None;
        }
        let ret_ty_expr = try_extract_matches!(
            signature.ret_ty(db),
            ast::OptionReturnTypeClause::ReturnTypeClause
        )?
        .ty(db);
        let ret_ty_path = try_extract_matches!(ret_ty_expr, ast::Expr::Path)?;

        // Currently only wrapping functions returning an Option<T>.
        let [ast::PathSegment::WithGenericArgs(segment)] = &ret_ty_path.elements(db)[..] else {
            return None;
        };
        if segment.ident(db).text(db) != "Option" {
            return None;
        }

        let ast::OptionAttributeArgs::AttributeArgs(args) = attr.args(db) else {
            return None;
        };
        let [ast::Expr::Literal(err_value)] = &args.arg_list(db).elements(db)[..] else {
            return None;
        };
        let function_name = ident.text(db);
        let params = get_node_text(db, &signature.parameters(db).as_syntax_node());
        let inner_ty_text =
            get_node_text(db, &segment.generic_args(db).generic_args(db).as_syntax_node());
        let args = signature
            .parameters(db)
            .elements(db)
            .into_iter()
            .map(|param| get_node_text(db, &param.name(db).as_syntax_node()))
            .join(", ");
        let err_value = err_value.text(db);
        return Some((
            "panicable".into(),
            indoc::formatdoc!(
                r#"
                    func {function_name}({params}) -> {inner_ty_text} {{
                        match super::{function_name}({args}) {{
                            Option::Some (v) => {{
                                v
                            }},
                            Option::None (v) => {{
                                let data = array_new::<felt>();
                                array_append::<felt>(data, {err_value});
                                panic(data);
                            }},
                        }}
                    }}
                "#
            ),
        ));
    }
    None
}
