use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{PluginDiagnostic, PluginGeneratedFile, PluginResult};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::try_extract_matches;
use indoc::formatdoc;
use itertools::chain;

use super::consts::{
    CONSTRUCTOR_ATTR, CONSTRUCTOR_MODULE, EXTERNAL_ATTR, EXTERNAL_MODULE, L1_HANDLER_ATTR,
    L1_HANDLER_MODULE,
};
use super::entry_point::{
    handle_entry_point, EntryPointGenerationParams, EntryPointKind, EntryPointsGenerationData,
};

/// Handles an includable impl, generating entry point wrappers and modules pointing to them.
pub fn handle_includable(db: &dyn SyntaxGroup, item_impl: ast::ItemImpl) -> PluginResult {
    let ast::MaybeImplBody::Some(body) = item_impl.body(db) else {
        return PluginResult::default();
    };
    let mut data = EntryPointsGenerationData::default();
    let mut diagnostics = vec![];
    let generic_params = item_impl.generic_params(db);
    let impl_name = RewriteNode::new_trimmed(item_impl.name(db).as_syntax_node());
    let (legal_params, generic_args, generic_params_node) = match &generic_params {
        ast::OptionWrappedGenericParamList::Empty(_) => {
            (false, RewriteNode::Text("".to_string()), RewriteNode::Text("".to_string()))
        }
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(params) => {
            let generic_params_node = params.generic_params(db);
            let mut generic_params = vec![
                RewriteNode::Text("<".to_string()),
                RewriteNode::new_trimmed(generic_params_node.as_syntax_node()),
            ];
            if generic_params_node.has_tail(db) {
                generic_params.push(RewriteNode::Text(", ".to_string()))
            }
            generic_params.push(RewriteNode::interpolate_patched(
                "impl UnsafeNewContractState: \
                 UnsafeNewContractStateTraitFor$impl_name$<TContractState>",
                [("impl_name".to_string(), impl_name.clone())].into(),
            ));
            generic_params.push(RewriteNode::Text(
                ", impl TContractStateDrop: Drop<TContractState>>".to_string(),
            ));
            let elements = generic_params_node.elements(db);
            (
                elements
                    .first()
                    .and_then(|param| try_extract_matches!(param, ast::GenericParam::Type))
                    .map_or(false, |param| param.name(db).text(db) == "TContractState"),
                RewriteNode::new_modified(
                    chain!(
                        [RewriteNode::Text("::<".to_string())],
                        elements.into_iter().map(|param| RewriteNode::new_trimmed(match param {
                            ast::GenericParam::Type(t) => t.as_syntax_node(),
                            ast::GenericParam::Const(c) => c.as_syntax_node(),
                            ast::GenericParam::Impl(i) => i.name(db).as_syntax_node(),
                        })),
                        [RewriteNode::Text(">".to_string())],
                    )
                    .collect(),
                ),
                RewriteNode::new_modified(generic_params),
            )
        }
    };
    if !legal_params {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: generic_params.stable_ptr().untyped(),
            message: "First argument of includable impl should be `TContractState`.".to_string(),
        });
    };
    let unsafe_new_contract_state = RewriteNode::interpolate_patched(
        "UnsafeNewContractState::unsafe_new_contract_state()",
        [("impl_name".to_string(), impl_name.clone())].into(),
    );
    for item in body.items(db).elements(db) {
        let ast::ImplItem::Function(item_function) = item else {
            continue;
        };
        let entry_point_kind = if item_function.has_attr(db, EXTERNAL_ATTR) {
            EntryPointKind::External
        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
            EntryPointKind::Constructor
        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
            EntryPointKind::L1Handler
        } else {
            continue;
        };
        let function_name =
            RewriteNode::new_trimmed(item_function.declaration(db).name(db).as_syntax_node());
        let function_name = RewriteNode::interpolate_patched(
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
                wrapped_function_path: function_name,
                unsafe_new_contract_state: unsafe_new_contract_state.clone(),
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

            mod {EXTERNAL_MODULE}$impl_name$ {{$external_functions$
            }}

            mod {L1_HANDLER_MODULE}$impl_name$ {{$l1_handler_functions$
            }}

            mod {CONSTRUCTOR_MODULE}$impl_name$ {{$constructor_functions$
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
            name: "includable".into(),
            content: builder.code,
            patches: builder.patches,
            aux_data: None,
        }),
        diagnostics,
        remove_original_item: false,
    }
}
