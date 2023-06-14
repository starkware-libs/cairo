use std::iter::zip;
use std::sync::Arc;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, MacroPlugin, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::plugin::{AsDynMacroPlugin, SemanticPlugin, TrivialPluginAuxData};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::ast::{
    Expr, GenericArg, ImplItem, ItemImpl, OptionWrappedGenericParamList,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use indoc::formatdoc;
use itertools::Itertools;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct GenerateTraitPlugin;

impl MacroPlugin for GenerateTraitPlugin {
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        match item_ast {
            ast::Item::Impl(impl_ast) => generate_trait_for_impl(db, impl_ast),
            _ => PluginResult::default(),
        }
    }
}
impl AsDynMacroPlugin for GenerateTraitPlugin {
    fn as_dyn_macro_plugin<'a>(self: Arc<Self>) -> Arc<dyn MacroPlugin + 'a>
    where
        Self: 'a,
    {
        self
    }
}
impl SemanticPlugin for GenerateTraitPlugin {}

fn generate_trait_for_impl(db: &dyn SyntaxGroup, impl_ast: ItemImpl) -> PluginResult {
    let Some(attr) = impl_ast.attributes(db).find_attr(db, "generate_trait") else {
        return PluginResult::default();
    };
    let trait_ast = impl_ast.trait_path(db);
    let [trait_ast_segment] = &trait_ast.elements(db)[..] else {
        return PluginResult {
            code: None,
            diagnostics: vec![
                PluginDiagnostic {
                    stable_ptr: trait_ast.stable_ptr().untyped(),
                    message: "Generated trait must have a single element path.".to_string(),
                }],
            remove_original_item: false,
        };
    };

    let mut diagnostics = vec![];
    let trait_attrs = attr
        .structurize(db)
        .args
        .into_iter()
        .flat_map(|attr_arg| match attr_arg.variant {
            AttributeArgVariant::Unnamed { value: Expr::FunctionCall(attr_arg), .. }
                if attr_arg.path(db).as_syntax_node().get_text_without_trivia(db)
                    == "trait_attrs" =>
            {
                attr_arg
                    .arguments(db)
                    .args(db)
                    .elements(db)
                    .into_iter()
                    .map(|arg| format!("#[{}]\n", arg.as_syntax_node().get_text(db)))
                    .collect()
            }
            _ => {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: attr_arg.arg_stable_ptr.untyped(),
                    message: "Expected an argument with the name `trait_attrs`.".to_string(),
                });
                vec![]
            }
        })
        .join("");

    let impl_generic_params = impl_ast.generic_params(db);
    let (trait_identifier, generic_params_match) = match trait_ast_segment {
        ast::PathSegment::WithGenericArgs(segment) => (
            segment.ident(db),
            if let OptionWrappedGenericParamList::WrappedGenericParamList(impl_generic_params) =
                impl_generic_params.clone()
            {
                let trait_generic_args = segment.generic_args(db).generic_args(db).elements(db);
                let impl_generic_params = impl_generic_params.generic_params(db).elements(db);
                zip(trait_generic_args, impl_generic_params).all(
                    |(trait_generic_arg, impl_generic_param)| {
                        let GenericArg::Expr(trait_generic_arg) = trait_generic_arg else {
                            return false;
                        };
                        let ast::Expr::Path(trait_generic_arg) = trait_generic_arg.value(db) else {
                            return false;
                        };
                        let [ast::PathSegment::Simple(trait_generic_arg)] =
                        &trait_generic_arg.elements(db)[..] else { return false; };
                        let trait_generic_arg_name = trait_generic_arg.ident(db);
                        let impl_generic_param_name = match impl_generic_param {
                            ast::GenericParam::Type(param) => param.name(db),
                            ast::GenericParam::Const(param) => param.name(db),
                            ast::GenericParam::Impl(param) => param.name(db),
                        };
                        trait_generic_arg_name.text(db) == impl_generic_param_name.text(db)
                    },
                )
            } else {
                false
            },
        ),
        ast::PathSegment::Simple(seg) => {
            (seg.ident(db), matches!(impl_generic_params, OptionWrappedGenericParamList::Empty(_)))
        }
    };
    let trait_identifier = trait_identifier.text(db);
    if !generic_params_match {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: trait_ast.stable_ptr().untyped(),
            message: "Generated trait must have generic args matching the impl's generic params."
                .to_string(),
        });
    }
    let signatures = match impl_ast.body(db) {
        ast::MaybeImplBody::Some(body) => body.items(db).elements(db),
        ast::MaybeImplBody::None(_) => vec![],
    }.into_iter().filter_map(|item| {
        match item {
            ImplItem::Function(item) => {
                let decl = item.declaration(db);
                let name = decl.name(db).text(db);
                let generic_params = decl.generic_params(db).as_syntax_node().get_text(db);
                let signature = decl.signature(db);
                let params = signature.parameters(db).elements(db).into_iter().map(|param| {
                    let modifiers = param.modifiers(db).elements(db).into_iter().filter_map(|modifier|{
                        // `mut` modifiers are only relevant for impls, not traits.
                        match modifier {
                            ast::Modifier::Ref(_) => Some("ref "),
                            ast::Modifier::Mut(_) => None,
                        }
                    }).join("");
                    let name = param.name(db).text(db);
                    let type_clause = param.type_clause(db).as_syntax_node().get_text(db);
                    format!("{modifiers}{name}{type_clause}")
                }).join(", ");
                let ret_ty = signature.ret_ty(db).as_syntax_node().get_text(db);
                let implicits_clause = signature.implicits_clause(db).as_syntax_node().get_text(db);
                let optional_no_panic = signature.optional_no_panic(db).as_syntax_node().get_text(db);
                Some(format!("    fn {name}{generic_params}({params}){ret_ty}{implicits_clause}{optional_no_panic};\n"))
            },
            // Only functions are supported as trait items for now.
            _ => None,
        }
    }).join("\n");
    let impl_generic_params = impl_generic_params.as_syntax_node().get_text(db);
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "generate_trait".into(),
            content: formatdoc! {"
            {trait_attrs}trait {trait_identifier}{impl_generic_params} {{
            {signatures}}}
        "},
            aux_data: DynGeneratedFileAuxData(Arc::new(TrivialPluginAuxData {})),
        }),
        diagnostics,
        remove_original_item: false,
    }
}
