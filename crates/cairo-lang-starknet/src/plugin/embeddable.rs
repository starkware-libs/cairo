use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;
use itertools::chain;

use super::consts::{
    CONSTRUCTOR_MODULE, EXTERNAL_MODULE, GENERIC_CONTRACT_STATE_NAME, L1_HANDLER_MODULE,
};
use super::entry_point::{
    handle_entry_point, EntryPointGenerationParams, EntryPointKind, EntryPointsGenerationData,
};
use super::utils::is_name_with_arg;

/// Handles an embeddable impl, generating entry point wrappers and modules pointing to them.
pub fn handle_embeddable(db: &dyn SyntaxGroup, item_impl: ast::ItemImpl) -> PluginResult {
    let ast::MaybeImplBody::Some(body) = item_impl.body(db) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                stable_ptr: item_impl.stable_ptr().untyped(),
                message: "Making empty impls embeddable is disallowed.".to_string(),
            }],
            remove_original_item: false,
        };
    };
    let generic_params = item_impl.generic_params(db);
    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    let (is_valid_params, generic_args, generic_params_node) = match &generic_params {
        ast::OptionWrappedGenericParamList::Empty(_) => {
            (false, RewriteNode::empty(), RewriteNode::empty())
        }
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(params) => {
            let generic_params_node = params.generic_params(db);
            let elements = generic_params_node.elements(db);
            let has_drop_impl = elements.iter().any(|param| {
                if let ast::GenericParam::Impl(i) = param {
                    is_name_with_arg(db, &i.trait_path(db), "Drop", GENERIC_CONTRACT_STATE_NAME)
                } else {
                    false
                }
            });
            let mut elements = elements.into_iter();
            let first_generic_param = elements.next();
            let is_valid_params = first_generic_param
                .and_then(|param| try_extract_matches!(param, ast::GenericParam::Type))
                .map_or(false, |param| param.name(db).text(db) == GENERIC_CONTRACT_STATE_NAME);
            let generic_args = RewriteNode::new_modified(
                chain!(
                    [RewriteNode::Text("::<TContractState".to_string())],
                    elements.flat_map(|param| [
                        RewriteNode::Text(", ".to_string()),
                        RewriteNode::new_trimmed(match param {
                            ast::GenericParam::Type(t) => t.as_syntax_node(),
                            ast::GenericParam::Const(c) => c.name(db).as_syntax_node(),
                            ast::GenericParam::Impl(i) => i.name(db).as_syntax_node(),
                        }),
                    ]),
                    [RewriteNode::Text(">".to_string())],
                )
                .collect(),
            );
            let maybe_comma = if generic_params_node.has_tail(db) { ", " } else { "" };
            let maybe_drop_impl =
                if has_drop_impl { "" } else { ", impl TContractStateDrop: Drop<TContractState>" };
            let generic_params_node = RewriteNode::interpolate_patched(
                &format!(
                    "<$generic_params${maybe_comma}impl UnsafeNewContractState: \
                     UnsafeNewContractStateTraitFor$impl_name$<TContractState>{maybe_drop_impl}>"
                ),
                [
                    (
                        "generic_params".to_string(),
                        RewriteNode::new_trimmed(generic_params_node.as_syntax_node()),
                    ),
                    ("impl_name".to_string(), impl_name.clone()),
                ]
                .into(),
            );
            (is_valid_params, generic_args, generic_params_node)
        }
    };
    if !is_valid_params {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                stable_ptr: generic_params.stable_ptr().untyped(),
                message: "First generic parameter of an embeddable impl should be \
                          `TContractState`."
                    .to_string(),
            }],
            remove_original_item: false,
        };
    };
    let mut data = EntryPointsGenerationData::default();
    let mut diagnostics = vec![];
    for item in body.items(db).elements(db) {
        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let Some(entry_point_kind) = EntryPointKind::try_from_attrs(db, &item_function) else {
            continue;
        };
        let function_name =
            RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
        let function_path = RewriteNode::interpolate_patched(
            "$impl_name$$generic_args$::$func_name$",
            [
                ("impl_name".to_string(), impl_name.clone()),
                ("func_name".to_string(), function_name),
                ("generic_args".to_string(), generic_args.clone()),
            ]
            .into(),
        );
        handle_entry_point(
            db,
            EntryPointGenerationParams {
                entry_point_kind,
                item_function: &item_function,
                wrapped_function_path: function_path,
                unsafe_new_contract_state_prefix: "UnsafeNewContractState::",
                generic_params: generic_params_node.clone(),
            },
            &mut diagnostics,
            &mut data,
        );
    }
    let code = RewriteNode::interpolate_patched(
        formatdoc!(
            "
            trait UnsafeNewContractStateTraitFor$impl_name$<TContractState> {{
                fn unsafe_new_contract_state() -> TContractState;
            }}

            $generated_wrapper_functions$

            mod {EXTERNAL_MODULE}_$impl_name$ {{$external_functions$
            }}

            mod {L1_HANDLER_MODULE}_$impl_name$ {{$l1_handler_functions$
            }}

            mod {CONSTRUCTOR_MODULE}_$impl_name$ {{$constructor_functions$
            }}
        "
        )
        .as_str(),
        [
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

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(code);
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "embeddable".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics,
        remove_original_item: false,
    }
}
