use std::iter::zip;

use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GenericParamEx, QueryAttrs};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct GenerateTraitPlugin;

const GENERATE_TRAIT_ATTR: &str = "generate_trait";

impl MacroPlugin for GenerateTraitPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Impl(impl_ast) => generate_trait_for_impl(db, impl_ast),
            _ => PluginResult::default(),
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![GENERATE_TRAIT_ATTR.to_string()]
    }
}

fn generate_trait_for_impl(db: &dyn SyntaxGroup, impl_ast: ast::ItemImpl) -> PluginResult {
    let Some(attr) = impl_ast.attributes(db).find_attr(db, GENERATE_TRAIT_ATTR) else {
        return PluginResult::default();
    };
    let trait_ast = impl_ast.trait_path(db);
    let [trait_ast_segment] = &trait_ast.elements(db)[..] else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic::error(
                trait_ast.stable_ptr().untyped(),
                "Generated trait must have a single element path.".to_string(),
            )],
            remove_original_item: false,
        };
    };

    let mut diagnostics = vec![];
    let mut builder = PatchBuilder::new(db);
    let leading_trivia = impl_ast
        .attributes(db)
        .elements(db)
        .first()
        .unwrap()
        .hash(db)
        .leading_trivia(db)
        .as_syntax_node()
        .get_text(db);
    let extra_ident = leading_trivia.split('\n').last().unwrap_or_default();
    for attr_arg in attr.structurize(db).args {
        match attr_arg.variant {
            AttributeArgVariant::Unnamed { value: ast::Expr::FunctionCall(attr_arg), .. }
                if attr_arg.path(db).as_syntax_node().get_text_without_trivia(db)
                    == "trait_attrs" =>
            {
                for arg in attr_arg.arguments(db).arguments(db).elements(db) {
                    builder.add_modified(RewriteNode::interpolate_patched(
                        &format!("{extra_ident}#[$attr$]\n"),
                        &[("attr".to_string(), RewriteNode::new_trimmed(arg.as_syntax_node()))]
                            .into(),
                    ));
                }
            }
            _ => {
                diagnostics.push(PluginDiagnostic::error(
                    attr_arg.arg_stable_ptr.untyped(),
                    "Expected an argument with the name `trait_attrs`.".to_string(),
                ));
            }
        }
    }
    builder.add_str(extra_ident);
    builder.add_node(impl_ast.visibility(db).as_syntax_node());
    builder.add_str("trait ");
    let impl_generic_params = impl_ast.generic_params(db);
    let generic_params_match = match trait_ast_segment {
        ast::PathSegment::WithGenericArgs(segment) => {
            builder.add_node(segment.ident(db).as_syntax_node());
            if let ast::OptionWrappedGenericParamList::WrappedGenericParamList(
                impl_generic_params,
            ) = impl_generic_params.clone()
            {
                // TODO(orizi): Support generic args that do not directly match the generic params.
                let trait_generic_args = segment.generic_args(db).generic_args(db).elements(db);
                let impl_generic_params = impl_generic_params.generic_params(db).elements(db);
                zip(trait_generic_args, impl_generic_params).all(
                    |(trait_generic_arg, impl_generic_param)| {
                        let ast::GenericArg::Unnamed(trait_generic_arg) = trait_generic_arg else {
                            return false;
                        };
                        let ast::GenericArgValue::Expr(trait_generic_arg) =
                            trait_generic_arg.value(db)
                        else {
                            return false;
                        };
                        let ast::Expr::Path(trait_generic_arg) = trait_generic_arg.expr(db) else {
                            return false;
                        };
                        let [ast::PathSegment::Simple(trait_generic_arg)] =
                            &trait_generic_arg.elements(db)[..]
                        else {
                            return false;
                        };
                        let trait_generic_arg_name = trait_generic_arg.ident(db);
                        let Some(impl_generic_param_name) = impl_generic_param.name(db) else {
                            return false;
                        };
                        trait_generic_arg_name.text(db) == impl_generic_param_name.text(db)
                    },
                )
            } else {
                false
            }
        }
        ast::PathSegment::Simple(segment) => {
            builder.add_node(segment.ident(db).as_syntax_node());
            matches!(impl_generic_params, ast::OptionWrappedGenericParamList::Empty(_))
        }
    };
    if !generic_params_match {
        diagnostics.push(PluginDiagnostic::error(
            trait_ast.stable_ptr().untyped(),
            "Generated trait must have generic args matching the impl's generic params."
                .to_string(),
        ));
    }
    match impl_ast.body(db) {
        ast::MaybeImplBody::None(semicolon) => {
            builder.add_modified(RewriteNode::new_trimmed(impl_generic_params.as_syntax_node()));
            builder.add_node(semicolon.as_syntax_node());
        }
        ast::MaybeImplBody::Some(body) => {
            builder.add_node(impl_generic_params.as_syntax_node());
            builder.add_node(body.lbrace(db).as_syntax_node());
            for item in body.items(db).elements(db) {
                let ast::ImplItem::Function(item) = item else {
                    // Only functions are supported as trait items for now.
                    continue;
                };
                let decl = item.declaration(db);
                let signature = decl.signature(db);
                builder.add_node(decl.function_kw(db).as_syntax_node());
                builder.add_node(decl.name(db).as_syntax_node());
                builder.add_node(decl.generic_params(db).as_syntax_node());
                builder.add_node(signature.lparen(db).as_syntax_node());
                for node in db.get_children(signature.parameters(db).node.clone()).iter().cloned() {
                    if node.kind(db) != SyntaxKind::Param {
                        builder.add_node(node);
                    } else {
                        let param = ast::Param::from_syntax_node(db, node);
                        for modifier in param.modifiers(db).elements(db) {
                            // `mut` modifiers are only relevant for impls, not traits.
                            if !matches!(modifier, ast::Modifier::Mut(_)) {
                                builder.add_node(modifier.as_syntax_node());
                            }
                        }
                        builder.add_node(param.name(db).as_syntax_node());
                        builder.add_node(param.type_clause(db).as_syntax_node());
                    }
                }
                let rparen = signature.rparen(db);
                let ret_ty = signature.ret_ty(db);
                let implicits_clause = signature.implicits_clause(db);
                let optional_no_panic = signature.optional_no_panic(db);
                let last_node = if matches!(
                    optional_no_panic,
                    ast::OptionTerminalNoPanic::TerminalNoPanic(_)
                ) {
                    builder.add_node(rparen.as_syntax_node());
                    builder.add_node(ret_ty.as_syntax_node());
                    builder.add_node(implicits_clause.as_syntax_node());
                    optional_no_panic.as_syntax_node()
                } else if matches!(implicits_clause, ast::OptionImplicitsClause::ImplicitsClause(_))
                {
                    builder.add_node(rparen.as_syntax_node());
                    builder.add_node(ret_ty.as_syntax_node());
                    implicits_clause.as_syntax_node()
                } else if matches!(ret_ty, ast::OptionReturnTypeClause::ReturnTypeClause(_)) {
                    builder.add_node(rparen.as_syntax_node());
                    ret_ty.as_syntax_node()
                } else {
                    rparen.as_syntax_node()
                };
                builder.add_modified(RewriteNode::Trimmed {
                    node: last_node,
                    trim_left: false,
                    trim_right: true,
                });
                builder.add_str(";\n");
            }
            builder.add_node(body.rbrace(db).as_syntax_node());
        }
    }
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "generate_trait".into(),
            content: builder.code,
            code_mappings: builder.code_mappings,
            aux_data: None,
        }),
        diagnostics,
        remove_original_item: false,
    }
}
