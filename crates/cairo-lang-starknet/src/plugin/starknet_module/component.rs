use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::indoc;

use super::generation_data::{ComponentGenerationData, StarknetModuleCommonGenerationData};
use super::StarknetModuleKind;
use crate::plugin::consts::{INCLUDABLE_AS_ATTR, STORAGE_STRUCT_NAME};
use crate::plugin::storage::handle_storage_struct;

/// Accumulated data specific for component generation.
#[derive(Default)]
pub struct ComponentSpecificGenerationData {
    generated_impls: Vec<RewriteNode>,
}
impl ComponentSpecificGenerationData {
    pub fn into_rewrite_node(self) -> RewriteNode {
        RewriteNode::new_modified(self.generated_impls)
    }
}

/// Generates the specific code for a component.
pub(super) fn generate_component_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
) -> RewriteNode {
    let mut generation_data = ComponentGenerationData { common: common_data, ..Default::default() };
    for item in body.items(db).elements(db) {
        handle_component_item(db, diagnostics, &item, &mut generation_data);
    }
    generation_data.into_rewrite_node()
}

/// Handles a single item inside a component module.
fn handle_component_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    data: &mut ComponentGenerationData,
) {
    match &item {
        ast::Item::Impl(item_impl) => {
            handle_component_impl(db, diagnostics, item_impl, data);
        }
        ast::Item::Struct(item_struct) if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME => {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Component,
                &mut data.common,
            );
        }
        _ => {}
    }
}

/// Validates the `includable_as` attribute and returns the value of its unnamed argument.
fn get_includable_as_attr_value(db: &dyn SyntaxGroup, attr: &ast::Attribute) -> Option<ast::Expr> {
    let ast::OptionArgListParenthesized::ArgListParenthesized(attribute_args) = attr.arguments(db)
    else {
        return None;
    };

    let [arg] = &attribute_args.args(db).elements(db)[..] else {
        return None;
    };
    let AttributeArgVariant::Unnamed { value: attr_arg_value, .. } =
        AttributeArg::from_ast(arg.clone(), db).variant
    else {
        return None;
    };

    Some(attr_arg_value)
}

/// Validates the generic parameters of the impl marked with `includable_as` attribute and returns
/// them if valid.
fn get_includable_as_impl_generic_params(
    db: &dyn SyntaxGroup,
    item_impl: &ast::ItemImpl,
) -> Result<ast::GenericParamList, PluginDiagnostic> {
    let generic_params = item_impl.generic_params(db);
    let generic_params_ptr = generic_params.stable_ptr().untyped();
    let first_generic_param_diagnostic = |stable_ptr| PluginDiagnostic {
        message: "The first generic parameter of an impl with #[includable_as] should be \
                  `TContractState`."
            .to_string(),
        stable_ptr,
    };

    let ast::OptionWrappedGenericParamList::WrappedGenericParamList(params) = generic_params else {
        return Err(first_generic_param_diagnostic(item_impl.name(db).stable_ptr().untyped()));
    };
    let generic_params_node = params.generic_params(db);
    let mut generic_param_elements = generic_params_node.elements(db).into_iter();

    // Verify the first generic param is `TContractState`.
    let Some(first_generic_param) = generic_param_elements.next() else {
        return Err(first_generic_param_diagnostic(generic_params_ptr));
    };
    if !try_extract_matches!(first_generic_param, ast::GenericParam::Type)
        .map_or(false, |param| param.name(db).text(db) == "TContractState")
    {
        return Err(first_generic_param_diagnostic(generic_params_ptr));
    }

    // Verify there is another generic param which is an impl of HasComponent<TContractState>.
    // TODO(yg): do this better? (allow whitespaces...)
    let has_has_component_impl = generic_param_elements.any(|param| {
        let Some(imp) = try_extract_matches!(param, ast::GenericParam::Impl) else {
            return false;
        };
        imp.trait_path(db).as_syntax_node().get_text(db) == "HasComponent<TContractState>"
    });
    if !has_has_component_impl {
        return Err(PluginDiagnostic {
            message: "An impl with #[includable_as] should have a generic parameter which is an \
                      impl of `HasComponent<TContractState>`."
                .to_string(),
            stable_ptr: generic_params_ptr,
        });
    }

    Ok(generic_params_node)
}

/// The parameters relevant for handling an #[includable_as] impl.
struct IncludableAsImplParams {
    /// The value of the unnamed argument of the `includable_as` attribute.
    attr_arg_value: ast::Expr,
    /// The generic parameters of the impl.
    generic_params_node: ast::GenericParamList,
    /// The body of the impl.
    impl_body: ast::ImplBody,
}
impl IncludableAsImplParams {
    /// Extracts the parameters for an #[includable_as] impl, and validates them.
    fn from_impl(
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
        item_impl: &ast::ItemImpl,
        attr: ast::Attribute,
    ) -> Option<IncludableAsImplParams> {
        let Some(attr_arg_value) = get_includable_as_attr_value(db, &attr) else {
            diagnostics.push(PluginDiagnostic {
                message: "`includable_as` attribute must have a single unnamed argument for the \
                          generated impl name, e.g.: #[includable_as(MyImpl)]."
                    .into(),
                stable_ptr: attr.stable_ptr().untyped(),
            });
            return None;
        };

        let generic_params_node = match get_includable_as_impl_generic_params(db, item_impl) {
            Ok(generic_params_node) => generic_params_node,
            Err(diagnostic) => {
                diagnostics.push(diagnostic);
                return None;
            }
        };

        let impl_body = match item_impl.body(db) {
            ast::MaybeImplBody::Some(impl_body) => impl_body,
            ast::MaybeImplBody::None(semicolon) => {
                diagnostics.push(PluginDiagnostic {
                    message: "`includable_as` attribute is not supported for empty impls.".into(),
                    stable_ptr: semicolon.stable_ptr().untyped(),
                });
                return None;
            }
        };

        Some(IncludableAsImplParams { attr_arg_value, generic_params_node, impl_body })
    }
}

/// Handles an impl inside a component module.
fn handle_component_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item_impl: &ast::ItemImpl,
    data: &mut ComponentGenerationData,
) {
    let Some(attr) = item_impl.find_attr(db, INCLUDABLE_AS_ATTR) else {
        return;
    };

    let Some(params) = IncludableAsImplParams::from_impl(db, diagnostics, item_impl, attr) else {
        return;
    };

    let maybe_comma = if params.generic_params_node.has_tail(db) {
        RewriteNode::Text(",".to_string())
    } else {
        RewriteNode::empty()
    };

    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    let generated_trait_name = RewriteNode::interpolate_patched(
        "$impl_name$Trait",
        [("impl_name".to_string(), impl_name)].into(),
    );

    let mut trait_functions = vec![];
    let mut impl_functions = vec![];
    for item in params.impl_body.items(db).elements(db) {
        let Some((trait_function, impl_function)) =
            handle_component_includable_as_impl_function(db, diagnostics, item)
        else {
            continue;
        };
        trait_functions.push(RewriteNode::Text("\n    ".to_string()));
        trait_functions.push(trait_function);
        impl_functions.push(RewriteNode::Text("\n    ".to_string()));
        impl_functions.push(impl_function);
    }

    let generated_impl_node = RewriteNode::interpolate_patched(
        indoc! {"
        trait $generated_trait_name$<TContractState> {$trait_functions$
        }

        impl $generated_impl_name$<$generic_params$$maybe_comma$ impl TContractStatePanicDestruct: \
         PanicDestruct<TContractState>> of $generated_trait_name$<TContractState> {$impl_functions$
        }"},
        [
            ("generated_trait_name".to_string(), generated_trait_name),
            ("trait_functions".to_string(), RewriteNode::new_modified(trait_functions)),
            (
                "generated_impl_name".to_string(),
                RewriteNode::Copied(params.attr_arg_value.as_syntax_node()),
            ),
            (
                "generic_params".to_string(),
                RewriteNode::Copied(params.generic_params_node.as_syntax_node()),
            ),
            ("maybe_comma".to_string(), maybe_comma),
            ("impl_functions".to_string(), RewriteNode::new_modified(impl_functions)),
        ]
        .into(),
    );

    data.specific.generated_impls.push(generated_impl_node);
}

fn handle_component_includable_as_impl_function(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: ast::ImplItem,
) -> Option<(RewriteNode, RewriteNode)> {
    let ast::ImplItem::Function(item_function) = item else {
        return None;
    };

    let declaration = item_function.declaration(db);
    let signature = declaration.signature(db);
    let parameters = signature.parameters(db);

    let function_name = RewriteNode::new_trimmed(declaration.name(db).as_syntax_node());
    let parameters_elements = parameters.elements(db);
    let Some((first_param, rest_params)) = parameters_elements.split_first() else {
        diagnostics.push(PluginDiagnostic {
            message: "A function in an #[includable_as] impl in a component must have a first \
                      `self` parameter."
                .to_string(),
            stable_ptr: parameters.stable_ptr().untyped(),
        });
        return None;
    };
    // TODO(yg): do this better? (allow whitespaces...)
    let (self_param, get_component_call) = match first_param.as_syntax_node().get_text(db).as_str()
    {
        "self: @ComponentState<TContractState>" => {
            ("self: @TContractState", "let component = self.get_component();")
        }
        "ref self: ComponentState<TContractState>" => {
            ("ref self: TContractState", "let mut component = self.get_component_mut();")
        }
        _ => {
            diagnostics.push(PluginDiagnostic {
                message: "The first parameter of a function in an #[includable_as] impl in a \
                          component must be either `self: @TContractState` (for view functions) \
                          or `ref self: TContractState` (for external functions)."
                    .to_string(),
                stable_ptr: parameters.stable_ptr().untyped(),
            });
            return None;
        }
    };
    let rest_params_node = RewriteNode::new_modified(
        rest_params
            .into_iter()
            .flat_map(|p| {
                vec![RewriteNode::Text(", ".to_string()), RewriteNode::Copied(p.as_syntax_node())]
            })
            .collect(),
    );
    let args_node = RewriteNode::new_modified(
        rest_params
            .into_iter()
            .flat_map(|p| {
                vec![
                    RewriteNode::Copied(p.name(db).as_syntax_node()),
                    RewriteNode::Text(", ".to_string()),
                ]
            })
            .collect(),
    );
    let ret_ty = match signature.ret_ty(db) {
        ast::OptionReturnTypeClause::Empty(_) => RewriteNode::empty(),
        ast::OptionReturnTypeClause::ReturnTypeClause(x) => RewriteNode::interpolate_patched(
            " $ret_ty$",
            [("ret_ty".to_string(), RewriteNode::new_trimmed(x.as_syntax_node()))].into(),
        ),
    };

    let generated_function_sig = RewriteNode::interpolate_patched(
        format!("fn $function_name$({self_param}$rest_params_node$)$ret_ty$").as_str(),
        [
            ("function_name".to_string(), function_name.clone()),
            ("rest_params_node".to_string(), rest_params_node),
            ("ret_ty".to_string(), ret_ty),
        ]
        .into(),
    );

    let trait_function = RewriteNode::interpolate_patched(
        "$generated_function_sig$;",
        [("generated_function_sig".to_string(), generated_function_sig.clone())].into(),
    );

    let impl_function = RewriteNode::interpolate_patched(
        format!(
            "$generated_function_sig$ {{
        {get_component_call}
        component.$function_name$($args_node$)
    }}"
        )
        .as_str(),
        [
            ("generated_function_sig".to_string(), generated_function_sig),
            ("function_name".to_string(), function_name),
            ("args_node".to_string(), args_node),
        ]
        .into(),
    );

    Some((trait_function, impl_function))
}
