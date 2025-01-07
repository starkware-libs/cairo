use cairo_lang_defs::patcher::RewriteNode;
use cairo_lang_defs::plugin::{MacroPluginMetadata, PluginDiagnostic};
use cairo_lang_plugins::plugins::HasItemsInCfgEx;
use cairo_lang_syntax::attribute::structured::{AttributeArg, AttributeArgVariant};
use cairo_lang_syntax::node::ast::OptionTypeClause;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{PathSegmentEx, QueryAttrs};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{extract_matches, require, try_extract_matches};
use indoc::{formatdoc, indoc};
use itertools::chain;

use super::StarknetModuleKind;
use super::generation_data::{ComponentGenerationData, StarknetModuleCommonGenerationData};
use crate::plugin::consts::{
    COMPONENT_STATE_NAME, EMBEDDABLE_AS_ATTR, EVENT_TYPE_NAME, GENERIC_COMPONENT_STATE_NAME,
    GENERIC_CONTRACT_STATE_NAME, HAS_COMPONENT_TRAIT, STORAGE_STRUCT_NAME,
};
use crate::plugin::storage::handle_storage_struct;
use crate::plugin::utils::{AstPathExtract, GenericParamExtract, ParamEx};

/// Accumulated data specific for component generation.
#[derive(Default)]
pub struct ComponentSpecificGenerationData {
    has_component_trait: RewriteNode,
    generated_impls: Vec<RewriteNode>,
}
impl ComponentSpecificGenerationData {
    pub fn into_rewrite_node(
        self,
        _db: &dyn SyntaxGroup,
        _diagnostics: &mut [PluginDiagnostic],
    ) -> RewriteNode {
        RewriteNode::interpolate_patched(
            indoc! {"
            // TODO(Gil): This generates duplicate diagnostics because of the plugin system, squash the duplicates into one.
            #[deprecated(
                feature: \"deprecated_legacy_map\",
                note: \"Use `starknet::storage::Map` instead.\"
            )]
            #[allow(unused_imports)]
            use starknet::storage::Map as LegacyMap;
            $has_component_trait$

            $generated_impls$"},
            &[
                ("has_component_trait".to_string(), self.has_component_trait),
                ("generated_impls".to_string(), RewriteNode::new_modified(self.generated_impls)),
            ]
            .into(),
        )
    }
}

/// Generates the specific code for a component.
pub(super) fn generate_component_specific_code(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    common_data: StarknetModuleCommonGenerationData,
    body: &ast::ModuleBody,
    metadata: &MacroPluginMetadata<'_>,
) -> RewriteNode {
    let mut generation_data = ComponentGenerationData { common: common_data, ..Default::default() };
    generate_has_component_trait_code(&mut generation_data.specific);
    for item in body.iter_items_in_cfg(db, metadata.cfg_set) {
        handle_component_item(db, diagnostics, &item, metadata, &mut generation_data);
    }
    generation_data.into_rewrite_node(db, diagnostics)
}

/// Handles a single item inside a component module.
fn handle_component_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::ModuleItem,
    metadata: &MacroPluginMetadata<'_>,
    data: &mut ComponentGenerationData,
) {
    match &item {
        ast::ModuleItem::Impl(item_impl) => {
            handle_component_impl(db, diagnostics, item_impl, metadata, data);
        }
        ast::ModuleItem::Struct(item_struct)
            if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
        {
            handle_storage_struct(
                db,
                diagnostics,
                item_struct.clone(),
                StarknetModuleKind::Component,
                &mut data.common,
                metadata,
            );
        }
        _ => {}
    }
}

/// Validates the `embeddable_as` attribute and returns the value of its unnamed argument.
fn get_embeddable_as_attr_value(db: &dyn SyntaxGroup, attr: &ast::Attribute) -> Option<ast::Expr> {
    let ast::OptionArgListParenthesized::ArgListParenthesized(attribute_args) = attr.arguments(db)
    else {
        return None;
    };

    let [arg] = &attribute_args.arguments(db).elements(db)[..] else {
        return None;
    };
    let AttributeArgVariant::Unnamed(attr_arg_value) =
        AttributeArg::from_ast(arg.clone(), db).variant
    else {
        return None;
    };

    Some(attr_arg_value)
}

/// Validates the generic parameters of the impl marked with `embeddable_as` attribute and returns
/// them if valid.
fn get_embeddable_as_impl_generic_params(
    db: &dyn SyntaxGroup,
    item_impl: &ast::ItemImpl,
) -> Result<ast::GenericParamList, PluginDiagnostic> {
    let generic_params = item_impl.generic_params(db);
    let generic_params_ptr = generic_params.stable_ptr().untyped();
    let first_generic_param_diagnostic = |stable_ptr| {
        PluginDiagnostic::error(
            stable_ptr,
            format!(
                "The first generic parameter of an impl with #[{EMBEDDABLE_AS_ATTR}] should be \
                 `TContractState`."
            ),
        )
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
        .map_or(false, |param| param.name(db).text(db) == GENERIC_CONTRACT_STATE_NAME)
    {
        return Err(first_generic_param_diagnostic(generic_params_ptr));
    }

    // Verify there is another generic param which is an impl of HasComponent<TContractState>.
    let has_has_component_impl = generic_param_elements
        .any(|param| param.is_impl_of(db, HAS_COMPONENT_TRAIT, GENERIC_CONTRACT_STATE_NAME));
    if !has_has_component_impl {
        return Err(PluginDiagnostic::error(
            generic_params_ptr,
            format!(
                "An impl with #[{EMBEDDABLE_AS_ATTR}] should have a generic parameter which is an \
                 impl of `{HAS_COMPONENT_TRAIT}<{GENERIC_CONTRACT_STATE_NAME}>`."
            ),
        ));
    }

    Ok(generic_params_node)
}

/// The parameters relevant for handling an `#[embeddable_as]` impl.
struct EmbeddableAsImplParams {
    /// The value of the unnamed argument of the `embeddable_as` attribute.
    attr_arg_value: ast::Expr,
    /// The generic parameters of the impl.
    generic_params_node: ast::GenericParamList,
    /// The trait the impl implements.
    trait_path: ast::ExprPath,
    /// The body of the impl.
    impl_body: ast::ImplBody,
}
impl EmbeddableAsImplParams {
    /// Extracts the parameters for an `#[embeddable_as]` impl, and validates them.
    fn from_impl(
        db: &dyn SyntaxGroup,
        diagnostics: &mut Vec<PluginDiagnostic>,
        item_impl: &ast::ItemImpl,
        attr: ast::Attribute,
    ) -> Option<EmbeddableAsImplParams> {
        let Some(attr_arg_value) = get_embeddable_as_attr_value(db, &attr) else {
            diagnostics.push(PluginDiagnostic::error(
                attr.stable_ptr().untyped(),
                format!(
                    "`{EMBEDDABLE_AS_ATTR}` attribute must have a single unnamed argument for the \
                     generated impl name, e.g.: #[{EMBEDDABLE_AS_ATTR}(MyImpl)]."
                ),
            ));
            return None;
        };

        let generic_params_node = match get_embeddable_as_impl_generic_params(db, item_impl) {
            Ok(generic_params_node) => generic_params_node,
            Err(diagnostic) => {
                diagnostics.push(diagnostic);
                return None;
            }
        };

        let trait_path = item_impl.trait_path(db);

        let impl_body = match item_impl.body(db) {
            ast::MaybeImplBody::Some(impl_body) => impl_body,
            ast::MaybeImplBody::None(semicolon) => {
                diagnostics.push(PluginDiagnostic::error(
                    semicolon.stable_ptr().untyped(),
                    format!("`{EMBEDDABLE_AS_ATTR}` attribute is not supported for empty impls."),
                ));
                return None;
            }
        };

        Some(EmbeddableAsImplParams { attr_arg_value, generic_params_node, trait_path, impl_body })
    }
}

/// Handles an impl inside a component module.
fn handle_component_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item_impl: &ast::ItemImpl,
    metadata: &MacroPluginMetadata<'_>,
    data: &mut ComponentGenerationData,
) {
    let Some(attr) = item_impl.find_attr(db, EMBEDDABLE_AS_ATTR) else {
        return;
    };

    let Some(params) = EmbeddableAsImplParams::from_impl(db, diagnostics, item_impl, attr.clone())
    else {
        return;
    };
    for param in &params.generic_params_node.elements(db) {
        if param.is_impl_of(db, "Destruct", GENERIC_CONTRACT_STATE_NAME)
            || param.is_impl_of(db, "PanicDestruct", GENERIC_CONTRACT_STATE_NAME)
        {
            diagnostics.push(PluginDiagnostic::error(
                param.stable_ptr().untyped(),
                format!(
                    "`embeddable_as` impls can't have impl generic parameters of \
                     `Destruct<{GENERIC_CONTRACT_STATE_NAME}>` or \
                     `PanicDestruct<{GENERIC_CONTRACT_STATE_NAME}>`."
                ),
            ));
            return;
        }
    }
    let trait_path_without_generics = remove_generics_from_path(db, &params.trait_path);

    let mut impl_functions = vec![];
    for item in params.impl_body.iter_items_in_cfg(db, metadata.cfg_set) {
        let Some(impl_function) = handle_component_embeddable_as_impl_item(
            db,
            diagnostics,
            RewriteNode::from_ast_trimmed(&item_impl.name(db)),
            item,
        ) else {
            continue;
        };
        impl_functions.push(RewriteNode::text("\n    "));
        impl_functions.push(impl_function);
    }
    let has_drop_impl = params
        .generic_params_node
        .elements(db)
        .iter()
        .any(|param| param.is_impl_of(db, "Drop", GENERIC_CONTRACT_STATE_NAME));
    let maybe_drop_impl = if has_drop_impl {
        "".to_string()
    } else {
        format!(
            "{maybe_comma}impl {GENERIC_CONTRACT_STATE_NAME}Drop: \
             Drop<{GENERIC_CONTRACT_STATE_NAME}>",
            maybe_comma = if params.generic_params_node.has_tail(db) { ", " } else { "" }
        )
    };

    let generated_impl_node = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
        #[starknet::embeddable]
        pub impl $generated_impl_name$<
            $generic_params${maybe_drop_impl}
        > of $trait_path$<{GENERIC_CONTRACT_STATE_NAME}> {{$impl_functions$
        }}"
        ),
        &[
            ("trait_path".to_string(), trait_path_without_generics),
            (
                "generated_impl_name".to_string(),
                RewriteNode::Copied(params.attr_arg_value.as_syntax_node()),
            ),
            (
                "generic_params".to_string(),
                RewriteNode::Copied(params.generic_params_node.as_syntax_node()),
            ),
            ("impl_functions".to_string(), RewriteNode::new_modified(impl_functions)),
        ]
        .into(),
    );

    data.specific.generated_impls.push(generated_impl_node.mapped(db, &attr));
}

/// Returns a RewriteNode of a path similar to the given path, but without generic params.
fn remove_generics_from_path(db: &dyn SyntaxGroup, trait_path: &ast::ExprPath) -> RewriteNode {
    let elements = trait_path.elements(db);
    let (last, prefix) = elements.split_last().unwrap();
    let last_without_generics = RewriteNode::from_ast_trimmed(&last.identifier_ast(db));

    RewriteNode::interspersed(
        chain!(prefix.iter().map(RewriteNode::from_ast_trimmed), [last_without_generics]),
        RewriteNode::text("::"),
    )
}

/// Handles an item of an `#[embeddable_as]` impl inside a component module.
fn handle_component_embeddable_as_impl_item(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_path: RewriteNode,
    item: ast::ImplItem,
) -> Option<RewriteNode> {
    let ast::ImplItem::Function(item_function) = item else {
        return None;
    };

    let attributes = item_function.attributes(db);
    let declaration = item_function.declaration(db);
    let signature = declaration.signature(db);
    let parameters = signature.parameters(db);

    let function_name = RewriteNode::from_ast_trimmed(&declaration.name(db));
    let parameters_elements = parameters.elements(db);
    let Some((first_param, rest_params)) = parameters_elements.split_first() else {
        diagnostics.push(PluginDiagnostic::error(
            parameters.stable_ptr().untyped(),
            format!(
                "A function in an #[{EMBEDDABLE_AS_ATTR}] impl in a component must have a first \
                 `self` parameter."
            ),
        ));
        return None;
    };
    let Some((self_param, get_component_call, callsite_modifier)) =
        handle_first_param_for_embeddable_as(db, first_param)
    else {
        diagnostics.push(PluginDiagnostic::error(
            parameters.stable_ptr().untyped(),
            format!(
                "The first parameter of a function in an #[{EMBEDDABLE_AS_ATTR}] impl in a \
                 component must be either `self: @{GENERIC_COMPONENT_STATE_NAME}` (for view \
                 functions) or `ref self: {GENERIC_COMPONENT_STATE_NAME}` (for external \
                 functions)."
            ),
        ));
        return None;
    };
    let rest_params_node = RewriteNode::new_modified(
        rest_params
            .iter()
            .flat_map(|p| vec![RewriteNode::text(", "), RewriteNode::Copied(p.as_syntax_node())])
            .collect(),
    );
    let args_node = RewriteNode::interspersed(
        chain!(
            [RewriteNode::Text(format!("{callsite_modifier}component"))],
            rest_params.iter().map(|p| RewriteNode::Copied(p.name(db).as_syntax_node()))
        ),
        RewriteNode::text(", "),
    );
    let ret_ty = match signature.ret_ty(db) {
        ast::OptionReturnTypeClause::Empty(_) => RewriteNode::empty(),
        ast::OptionReturnTypeClause::ReturnTypeClause(x) => RewriteNode::interpolate_patched(
            " $ret_ty$",
            &[("ret_ty".to_string(), RewriteNode::from_ast_trimmed(&x))].into(),
        ),
    };

    let generated_function_sig = RewriteNode::interpolate_patched(
        &format!("$attributes$\n    fn $function_name$({self_param}$rest_params_node$)$ret_ty$"),
        &[
            ("attributes".to_string(), RewriteNode::from_ast_trimmed(&attributes)),
            ("function_name".to_string(), function_name.clone()),
            ("rest_params_node".to_string(), rest_params_node),
            ("ret_ty".to_string(), ret_ty),
        ]
        .into(),
    );

    let impl_function = RewriteNode::interpolate_patched(
        &format!(
            "$generated_function_sig$ {{
        {get_component_call}
        $impl_path$::$function_name$($args_node$)
    }}"
        ),
        &[
            ("generated_function_sig".to_string(), generated_function_sig),
            ("impl_path".to_string(), impl_path),
            ("function_name".to_string(), function_name),
            ("args_node".to_string(), args_node),
        ]
        .into(),
    )
    .mapped(db, &item_function);

    Some(impl_function)
}

/// Checks if the first parameter of a function in an impl is a valid value of an impl marked with
/// `#[embeddable_as]`, and returns the matching (wrapping function contract state param, code for
/// fetching the matching component state from it, callsite_modifier).
fn handle_first_param_for_embeddable_as(
    db: &dyn SyntaxGroup,
    param: &ast::Param,
) -> Option<(String, String, String)> {
    require(param.name(db).text(db) == "self")?;
    if param.is_ref_param(db) {
        return if extract_matches!(param.type_clause(db), OptionTypeClause::TypeClause)
            .ty(db)
            .is_name_with_arg(db, COMPONENT_STATE_NAME, GENERIC_CONTRACT_STATE_NAME)
        {
            Some((
                format!("ref self: {GENERIC_CONTRACT_STATE_NAME}"),
                format!("let mut component = {HAS_COMPONENT_TRAIT}::get_component_mut(ref self);"),
                "ref ".to_string(),
            ))
        } else {
            None
        };
    }
    if param.try_extract_snapshot(db)?.is_name_with_arg(
        db,
        COMPONENT_STATE_NAME,
        GENERIC_CONTRACT_STATE_NAME,
    ) {
        Some((
            format!("self: @{GENERIC_CONTRACT_STATE_NAME}"),
            format!("let component = {HAS_COMPONENT_TRAIT}::get_component(self);"),
            "".to_string(),
        ))
    } else {
        None
    }
}

/// Generates the code of the `HasComponent` trait inside a Starknet component.
fn generate_has_component_trait_code(data: &mut ComponentSpecificGenerationData) {
    data.has_component_trait = RewriteNode::Text(formatdoc!(
        "
        pub trait {HAS_COMPONENT_TRAIT}<{GENERIC_CONTRACT_STATE_NAME}> {{
            fn get_component(self: @{GENERIC_CONTRACT_STATE_NAME}) -> \
         @{GENERIC_COMPONENT_STATE_NAME};
            fn get_component_mut(ref self: {GENERIC_CONTRACT_STATE_NAME}) -> \
         {GENERIC_COMPONENT_STATE_NAME};
            fn get_contract(self: @{GENERIC_COMPONENT_STATE_NAME}) -> \
         @{GENERIC_CONTRACT_STATE_NAME};
            fn get_contract_mut(ref self: {GENERIC_COMPONENT_STATE_NAME}) -> \
         {GENERIC_CONTRACT_STATE_NAME};
            fn emit<S, impl IntoImp: core::traits::Into<S, {EVENT_TYPE_NAME}>>(ref self: \
         {GENERIC_COMPONENT_STATE_NAME}, event: S);
        }}"
    ));
}
