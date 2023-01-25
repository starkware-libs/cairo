use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialMapper};
use cairo_lang_syntax::node::ast::AttributeList;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;

#[derive(Debug)]
pub struct PanicablePlugin {}

impl MacroPlugin for PanicablePlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let (declaration, attributes) = match item_ast {
            ast::Item::ExternFunction(extern_func_ast) => {
                (extern_func_ast.declaration(db), extern_func_ast.attributes(db))
            }
            ast::Item::FreeFunction(free_func_ast) => {
                (free_func_ast.declaration(db), free_func_ast.attributes(db))
            }
            _ => return PluginResult::default(),
        };

        generate_panicable_code(db, declaration, attributes)
    }
}
impl AsDynMacroPlugin for PanicablePlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for PanicablePlugin {}

/// Adds an implementation for all requested derives for the type.
fn generate_panicable_code(
    db: &dyn SyntaxGroup,
    declaration: ast::FunctionDeclaration,
    attributes: AttributeList,
) -> PluginResult {
    let remove_original_item = false;
    for attr in attributes.elements(db) {
        if attr.attr(db).text(db) != "panic_with" {
            continue;
        }
        let signature = declaration.signature(db);
        if !matches!(
            signature.optional_no_panic(db),
            ast::OptionTerminalNoPanic::TerminalNoPanic(_)
        ) {
            // Only nopanic functions can be wrapped.
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: attr.stable_ptr().untyped(),
                    message: "Only nopanic functions can be wrapped".into(),
                }],
                remove_original_item,
            };
        }

        let Some((inner_ty_text, success_variant, failure_variant)) =
            extract_success_ty_and_variants(db, &signature) else {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    stable_ptr: signature.ret_ty(db).stable_ptr().untyped(),
                    message: "Currently only wrapping functions returning an Option<T> or \
                                Result<T, E>"
                        .into(),
                }],
                remove_original_item,
            };
        };

        let Some((err_value, panicable_name)) = try_extract_matches!(attr.args(db), ast::OptionAttributeArgs::AttributeArgs).and_then(
            |args| {
            if let [ast::Expr::ShortString(err_value), ast::Expr::Path(name)] = &args.arg_list(db).elements(db)[..] {
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
                    stable_ptr: attr.stable_ptr().untyped(),
                    message: "Failed to extract panic data attribute".into(),
                }],
                remove_original_item,
            };
        };
        let generics_params = declaration.generic_params(db).as_syntax_node().get_text(db);

        let function_name = declaration.name(db).text(db);
        let params = signature.parameters(db).as_syntax_node().get_text(db);
        let args = signature
            .parameters(db)
            .elements(db)
            .into_iter()
            .map(|param| {
                format!(
                    "{}{}",
                    if matches!(&param.modifiers(db).elements(db)[..], [ast::Modifier::Ref(_)]) {
                        "ref "
                    } else {
                        ""
                    },
                    param.name(db).as_syntax_node().get_text(db)
                )
            })
            .join(", ");
        return PluginResult {
            code: Some(PluginGeneratedFile {
                name: "panicable".into(),
                content: indoc::formatdoc!(
                    r#"
                    fn {panicable_name}{generics_params}({params}) -> {inner_ty_text} {{
                        match {function_name}({args}) {{
                            {success_variant} (v) => {{
                                v
                            }},
                            {failure_variant} (v) => {{
                                let mut data = array_new::<felt>();
                                array_append::<felt>(ref data, {err_value});
                                panic(data)
                            }},
                        }}
                    }}
                "#
                ),
                aux_data: DynGeneratedFileAuxData(Arc::new(TrivialMapper {})),
            }),
            diagnostics: vec![],
            remove_original_item,
        };
    }
    PluginResult::default()
}

/// Given a function signature, if it returns `Option::<T>` or `Result::<T, E>`, returns T and the
/// variant match strings. Otherwise, returns None.
fn extract_success_ty_and_variants(
    db: &dyn SyntaxGroup,
    signature: &ast::FunctionSignature,
) -> Option<(String, String, String)> {
    let ret_ty_expr =
        try_extract_matches!(signature.ret_ty(db), ast::OptionReturnTypeClause::ReturnTypeClause)?
            .ty(db);
    let ret_ty_path = try_extract_matches!(ret_ty_expr, ast::Expr::Path)?;

    // Currently only wrapping functions returning an Option<T>.
    let [ast::PathSegment::WithGenericArgs(segment)] = &ret_ty_path.elements(db)[..] else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == "Option" {
        let [inner] = &segment.generic_args(db).generic_args(db).elements(db)[..] else { return None; };
        Some((
            inner.as_syntax_node().get_text(db),
            "Option::Some".to_owned(),
            "Option::None".to_owned(),
        ))
    } else if ty == "Result" {
        let [inner, _err] = &segment.generic_args(db).generic_args(db).elements(db)[..] else { return None; };
        Some((
            inner.as_syntax_node().get_text(db),
            "Result::Ok".to_owned(),
            "Result::Err".to_owned(),
        ))
    } else {
        None
    }
}
