use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{BodyItems, GenericParamEx};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;
use itertools::chain;

use super::consts::{
    CONSTRUCTOR_MODULE, EXTERNAL_MODULE, GENERIC_CONTRACT_STATE_NAME, L1_HANDLER_MODULE,
};
use super::entry_point::{
    EntryPointGenerationParams, EntryPointKind, EntryPointsGenerationData, handle_entry_point,
};
use super::utils::{GenericParamExtract, forbid_attributes_in_impl};

/// Handles an embeddable impl, generating entry point wrappers and modules pointing to them.
pub fn handle_embeddable(db: &dyn SyntaxGroup, item_impl: ast::ItemImpl) -> PluginResult {
    let ast::MaybeImplBody::Some(body) = item_impl.body(db) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic::error(
                item_impl.stable_ptr().untyped(),
                "Making empty impls embeddable is disallowed.".to_string(),
            )],
            remove_original_item: false,
        };
    };
    let mut diagnostics = vec![];
    let generic_params = item_impl.generic_params(db);
    let impl_name = item_impl.name(db);
    let impl_name_str = impl_name.text(db);
    let impl_name = RewriteNode::from_ast_trimmed(&impl_name);
    let (is_valid_params, maybe_generic_args, generic_params_node) = match &generic_params {
        ast::OptionWrappedGenericParamList::Empty(_) => {
            (false, RewriteNode::empty(), RewriteNode::empty())
        }
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(params) => {
            let generic_params_node = params.generic_params(db);
            let elements = generic_params_node.elements(db);
            let has_drop_impl = elements
                .iter()
                .any(|param| param.is_impl_of(db, "Drop", GENERIC_CONTRACT_STATE_NAME));
            for param in &elements {
                if param.is_impl_of(db, "Destruct", GENERIC_CONTRACT_STATE_NAME)
                    || param.is_impl_of(db, "PanicDestruct", GENERIC_CONTRACT_STATE_NAME)
                {
                    diagnostics.push(PluginDiagnostic::error(
                        param.stable_ptr().untyped(),
                        format!(
                            "`embeddable` impls can't have impl generic parameters of \
                             `Destruct<{GENERIC_CONTRACT_STATE_NAME}>` or \
                             `PanicDestruct<{GENERIC_CONTRACT_STATE_NAME}>`."
                        ),
                    ));
                }
            }
            let mut elements = elements.into_iter();
            let first_generic_param = elements.next();
            let is_valid_params = first_generic_param
                .and_then(|param| try_extract_matches!(param, ast::GenericParam::Type))
                .is_some_and(|param| param.name(db).text(db) == GENERIC_CONTRACT_STATE_NAME);
            let generic_args = RewriteNode::interspersed(
                chain!(
                    [RewriteNode::text(GENERIC_CONTRACT_STATE_NAME)],
                    elements.map(|param| {
                        param
                            .name(db)
                            .as_ref()
                            .map(RewriteNode::from_ast_trimmed)
                            .unwrap_or_else(|| RewriteNode::text("_"))
                    })
                ),
                RewriteNode::text(", "),
            );
            let generic_args = RewriteNode::interpolate_patched(
                "::<$generic_args$>",
                &[("generic_args".to_string(), generic_args)].into(),
            );
            let maybe_comma = if generic_params_node.has_tail(db) { ", " } else { "" };
            let maybe_drop_impl = if has_drop_impl {
                "".to_string()
            } else {
                format!(", impl TContractStateDrop: Drop<{GENERIC_CONTRACT_STATE_NAME}>")
            };
            let generic_params_node = RewriteNode::interpolate_patched(
                &format!(
                    "<$generic_params${maybe_comma}impl UnsafeNewContractState: \
                     UnsafeNewContractStateTraitFor$impl_name$<{GENERIC_CONTRACT_STATE_NAME}>{maybe_drop_impl}>"
                ),
                &[
                    (
                        "generic_params".to_string(),
                        RewriteNode::from_ast_trimmed(&generic_params_node),
                    ),
                    ("impl_name".to_string(), impl_name.clone()),
                ]
                .into(),
            );
            (is_valid_params, generic_args, generic_params_node)
        }
    };
    if !is_valid_params {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr().untyped(),
            format!(
                "First generic parameter of an embeddable impl should be \
                 `{GENERIC_CONTRACT_STATE_NAME}`."
            ),
        ));
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    };
    let mut data = EntryPointsGenerationData::default();
    for item in body.items_vec(db) {
        // TODO(yuval): do the same in embeddable_As, to get a better diagnostic.
        forbid_attributes_in_impl(db, &mut diagnostics, &item, "#[embeddable]");
        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let function_name = item_function.declaration(db).name(db);
        let function_name_str = function_name.text(db);
        let function_name = RewriteNode::from_ast_trimmed(&function_name);
        let function_path = RewriteNode::interpolate_patched(
            "$impl_name$$maybe_generic_args$::$func_name$",
            &[
                ("impl_name".to_string(), impl_name.clone()),
                ("func_name".to_string(), function_name),
                ("maybe_generic_args".to_string(), maybe_generic_args.clone()),
            ]
            .into(),
        );
        let wrapper_identifier = format!("{impl_name_str}__{function_name_str}");
        handle_entry_point(
            db,
            EntryPointGenerationParams {
                entry_point_kind: EntryPointKind::External,
                item_function: &item_function,
                wrapped_function_path: function_path,
                wrapper_identifier,
                unsafe_new_contract_state_prefix: "UnsafeNewContractState::",
                generic_params: generic_params_node.clone(),
            },
            &mut diagnostics,
            &mut data,
        );
    }
    let code = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
            $visibility$trait UnsafeNewContractStateTraitFor$impl_name$<
                {GENERIC_CONTRACT_STATE_NAME}
            > {{
                fn unsafe_new_contract_state() -> {GENERIC_CONTRACT_STATE_NAME};
            }}

            $generated_wrapper_functions$

            $visibility$mod {EXTERNAL_MODULE}_$impl_name$ {{$external_functions$
            }}

            $visibility$mod {L1_HANDLER_MODULE}_$impl_name$ {{$l1_handler_functions$
            }}

            $visibility$mod {CONSTRUCTOR_MODULE}_$impl_name$ {{$constructor_functions$
            }}
        "
        ),
        &[
            (
                "visibility".to_string(),
                RewriteNode::Trimmed {
                    node: item_impl.visibility(db).as_syntax_node(),
                    trim_left: true,
                    trim_right: false,
                },
            ),
            ("impl_name".to_string(), impl_name),
            (
                "generated_wrapper_functions".to_string(),
                RewriteNode::new_modified(data.generated_wrapper_functions),
            ),
            ("external_functions".to_string(), RewriteNode::new_modified(data.external_functions)),
            (
                "l1_handler_functions".to_string(),
                RewriteNode::new_modified(data.l1_handler_functions),
            ),
            (
                "constructor_functions".to_string(),
                RewriteNode::new_modified(data.constructor_functions),
            ),
        ]
        .into(),
    );

    let mut builder = PatchBuilder::new(db, &item_impl);
    builder.add_modified(code);
    let (content, code_mappings) = builder.build();
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "embeddable".into(),
            content,
            code_mappings,
            aux_data: None,
            diagnostics_note: Default::default(),
        }),
        diagnostics,
        remove_original_item: false,
    }
}
