use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::{ModifiedNode, RewriteNode};
use cairo_lang_syntax::node::ast::{self, OptionReturnTypeClause, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};

use super::utils::is_ref_param;
use crate::contract::starknet_keccak;

/// Generates a function to emit an event and the corresponding ABI item.
/// On success, returns a RewriteNode for the event function and a RewriteNode for the ABI
/// declaration. On failure returns None. In addition, returns diagnostics.
pub fn handle_event(
    db: &dyn SyntaxGroup,
    function_ast: ast::FunctionWithBody,
) -> (Option<(RewriteNode, RewriteNode)>, Vec<PluginDiagnostic>) {
    let mut diagnostics = vec![];
    let declaration = function_ast.declaration(db);

    if let OptionWrappedGenericParamList::WrappedGenericParamList(generic_params) =
        declaration.generic_params(db)
    {
        diagnostics.push(PluginDiagnostic {
            message: "Event functions cannot have generic arguments".to_string(),
            stable_ptr: generic_params.stable_ptr().untyped(),
        })
    }

    let signature = declaration.signature(db);
    let ret_ty = declaration.signature(db).ret_ty(db);
    if matches!(ret_ty, OptionReturnTypeClause::ReturnTypeClause(_)) {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: ret_ty.stable_ptr().untyped(),
            message: "Event functions must not return a value.".to_string(),
        });
    }

    let mut param_serializations = Vec::new();
    for param in signature.parameters(db).elements(db) {
        // If we encounter errors with this parameter that don't allow us to serialize it, we skip
        // the serialization of it in the generated code.
        let mut skip_param_serialization = false;
        if is_ref_param(db, &param) {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                message: "`ref` parameters are not supported in contract events.".to_string(),
            });
            skip_param_serialization = true;
        }

        let param_name = param.name(db);
        let param_type_ast = param.type_clause(db).ty(db);
        let type_name = param_type_ast.as_syntax_node().get_text(db);
        if skip_param_serialization {
            continue;
        }

        // TODO(yuval): use panicable version of deserializations when supported.
        let param_serialization = RewriteNode::interpolate_patched(
            &format!("serde::Serde::<{type_name}>::serialize(ref __data, $param_name$);\n        "),
            HashMap::from([(
                "param_name".to_string(),
                RewriteNode::Trimmed(param_name.as_syntax_node()),
            )]),
        );
        param_serializations.push(param_serialization);
    }

    if !function_ast.body(db).statements(db).elements(db).is_empty() {
        diagnostics.push(PluginDiagnostic {
            stable_ptr: function_ast.body(db).statements(db).stable_ptr().untyped(),
            message: "Event function body must be empty.".to_string(),
        });
    }

    if !diagnostics.is_empty() {
        return (None, diagnostics);
    }

    let name = declaration.name(db).text(db);
    let event_key = format!("0x{:x}", starknet_keccak(name.as_bytes()));

    (
        Some((
            // Event function
            RewriteNode::interpolate_patched(
                &format!(
                    "
    $attrs$
    $declaration$ {{
        let mut __keys = array_new();
        array_append(ref __keys, {event_key});
        let mut __data = array_new();
        $param_serializations$
        starknet::emit_event_syscall(__keys, __data).unwrap_syscall()
    }}
            "
                ),
                HashMap::from([
                    // TODO(yuval): All the attributes are currently copied. Remove the #[event]
                    // attr.
                    (
                        "attrs".to_string(),
                        RewriteNode::Trimmed(function_ast.attributes(db).as_syntax_node()),
                    ),
                    ("declaration".to_string(), RewriteNode::Trimmed(declaration.as_syntax_node())),
                    (
                        "param_serializations".to_string(),
                        RewriteNode::Modified(ModifiedNode { children: param_serializations }),
                    ),
                ]),
            ),
            // ABI event
            RewriteNode::Modified(ModifiedNode {
                children: vec![
                    RewriteNode::Text("#[event]\n        ".to_string()),
                    RewriteNode::Trimmed(function_ast.declaration(db).as_syntax_node()),
                    RewriteNode::Text(";\n        ".to_string()),
                ],
            }),
        )),
        diagnostics,
    )
}
