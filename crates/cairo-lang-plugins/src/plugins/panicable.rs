use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::attribute::structured::{
    Attribute, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;
use itertools::Itertools;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct PanicablePlugin;

const PANIC_WITH_ATTR: &str = "panic_with";

impl MacroPlugin for PanicablePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let (declaration, attributes, visibility) = match item_ast {
            ast::ModuleItem::ExternFunction(extern_func_ast) => (
                extern_func_ast.declaration(db),
                extern_func_ast.attributes(db),
                extern_func_ast.visibility(db),
            ),
            ast::ModuleItem::FreeFunction(free_func_ast) => (
                free_func_ast.declaration(db),
                free_func_ast.attributes(db),
                free_func_ast.visibility(db),
            ),
            _ => return PluginResult::default(),
        };

        generate_panicable_code(db, declaration, attributes, visibility)
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![PANIC_WITH_ATTR.to_string()]
    }
}

/// Generate code defining a panicable variant of a function marked with `#[panic_with]` attribute.
fn generate_panicable_code(
    db: &dyn SyntaxGroup,
    declaration: ast::FunctionDeclaration,
    attributes: ast::AttributeList,
    visibility: ast::Visibility,
) -> PluginResult {
    let mut attrs = attributes.query_attr(db, PANIC_WITH_ATTR);
    let Some(attr) = attrs.next() else {
        // No `#[panic_with]` attribute found.
        return PluginResult::default();
    };
    let mut diagnostics = vec![];
    if let Some(extra_attr) = attrs.next() {
        diagnostics.push(PluginDiagnostic::error(
            extra_attr.stable_ptr(db),
            "`#[panic_with]` cannot be applied multiple times to the same item.".into(),
        ));
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    }

    let signature = declaration.signature(db);
    let Some((inner_ty, success_variant, failure_variant)) =
        extract_success_ty_and_variants(db, &signature)
    else {
        diagnostics.push(PluginDiagnostic::error(
            signature.ret_ty(db).stable_ptr(db),
            "Currently only wrapping functions returning an Option<T> or Result<T, E>".into(),
        ));
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    };

    let mut builder = PatchBuilder::new(db, &attr);
    let attr = attr.structurize(db);

    let Some((err_value, panicable_name)) = parse_arguments(db, &attr) else {
        diagnostics.push(PluginDiagnostic::error(
            attr.stable_ptr,
            "Failed to extract panic data attribute".into(),
        ));
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    };
    builder.add_node(visibility.as_syntax_node());
    builder.add_node(declaration.function_kw(db).as_syntax_node());
    builder.add_modified(RewriteNode::from_ast_trimmed(&panicable_name));
    builder.add_node(declaration.generic_params(db).as_syntax_node());
    builder.add_node(signature.lparen(db).as_syntax_node());
    builder.add_node(signature.parameters(db).as_syntax_node());
    builder.add_node(signature.rparen(db).as_syntax_node());
    let args = signature
        .parameters(db)
        .elements(db)
        .map(|param| {
            let ref_kw = if let Some([ast::Modifier::Ref(_)]) =
                param.modifiers(db).elements(db).collect_array()
            {
                "ref "
            } else {
                ""
            };
            format!("{}{}", ref_kw, param.name(db).as_syntax_node().get_text(db))
        })
        .join(", ");
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            r#"
                -> $inner_ty$ {{
                    match $function_name$({args}) {{
                        {success_variant}(v) => v,
                        {failure_variant}(_) => core::panic_with_const_felt252::<$err_value$>(),
                    }}
                }}
            "#
        ),
        &[
            ("inner_ty".to_string(), RewriteNode::from_ast_trimmed(&inner_ty)),
            ("function_name".to_string(), RewriteNode::from_ast_trimmed(&declaration.name(db))),
            ("err_value".to_string(), RewriteNode::from_ast_trimmed(&err_value)),
        ]
        .into(),
    ));

    let (content, code_mappings) = builder.build();
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "panicable".into(),
            content,
            code_mappings,
            aux_data: None,
            diagnostics_note: Default::default(),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Given a function signature, if it returns `Option::<T>` or `Result::<T, E>`, returns T and the
/// variant match strings. Otherwise, returns None.
fn extract_success_ty_and_variants(
    db: &dyn SyntaxGroup,
    signature: &ast::FunctionSignature,
) -> Option<(ast::GenericArg, String, String)> {
    let ret_ty_expr =
        try_extract_matches!(signature.ret_ty(db), ast::OptionReturnTypeClause::ReturnTypeClause)?
            .ty(db);
    let ret_ty_path = try_extract_matches!(ret_ty_expr, ast::Expr::Path)?;

    // Currently only wrapping functions returning an Option<T>.
    let Some([ast::PathSegment::WithGenericArgs(segment)]) =
        ret_ty_path.segments(db).elements(db).collect_array()
    else {
        return None;
    };
    let ty = segment.ident(db).text(db);
    if ty == "Option" {
        let [inner] = segment.generic_args(db).generic_args(db).elements(db).collect_array()?;
        Some((inner.clone(), "Option::Some".to_owned(), "Option::None".to_owned()))
    } else if ty == "Result" {
        let [inner, _err] =
            segment.generic_args(db).generic_args(db).elements(db).collect_array()?;
        Some((inner.clone(), "Result::Ok".to_owned(), "Result::Err".to_owned()))
    } else {
        None
    }
}

/// Parse `#[panic_with(...)]` attribute arguments and return a tuple with error value and
/// panicable function name.
fn parse_arguments(
    db: &dyn SyntaxGroup,
    attr: &Attribute,
) -> Option<(ast::TerminalShortString, ast::TerminalIdentifier)> {
    let [
        AttributeArg {
            variant: AttributeArgVariant::Unnamed(ast::Expr::ShortString(err_value)),
            ..
        },
        AttributeArg { variant: AttributeArgVariant::Unnamed(ast::Expr::Path(name)), .. },
    ] = &attr.args[..]
    else {
        return None;
    };

    let Some([ast::PathSegment::Simple(segment)]) = name.segments(db).elements(db).collect_array()
    else {
        return None;
    };

    Some((err_value.clone(), segment.ident(db)))
}
