use std::collections::HashMap;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{self, OptionReturnTypeClause, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::indoc;

use super::aux_data::StarkNetABIAuxData;
use super::utils::is_ref_param;
use crate::contract::starknet_keccak;

// TODO(spapini): Handle member and variant attributes for serde / event.
/// Derive the `Event` trait for structs annotated with `derive(starknet::Event)`.
pub fn handle_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
    if !derive_event_needed(&struct_ast, db) {
        return PluginResult::default();
    }

    let mut builder = PatchBuilder::new(db);
    let mut diagnostics = vec![];

    // TODO(spapini): Support generics.
    let generic_params = struct_ast.generic_params(db);
    let OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic {
            message: "Event structs with generic arguments are unsupported".to_string(),
            stable_ptr: generic_params.stable_ptr().untyped(),
        });
        return PluginResult{ code: None, diagnostics, remove_original_item: false };
    };

    let mut append_members = vec![];
    for member in struct_ast.members(db).elements(db) {
        let member_name = RewriteNode::new_trimmed(member.name(db).as_syntax_node());
        let append_member = RewriteNode::interpolate_patched(
            "
        starknet::Event::append_keys_and_values(
            self.$member_name$, ref keys, ref values
        );",
            HashMap::from([(String::from("member_name"), member_name)]),
        );
        append_members.push(append_member);
    }
    let append_members = RewriteNode::Modified(ModifiedNode { children: Some(append_members) });

    // Add an implementation for `Event<StructName>`.
    let struct_name = RewriteNode::new_trimmed(struct_ast.name(db).as_syntax_node());
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $struct_name$IsEvent of starknet::Event<$struct_name$> {
                fn append_keys_and_values(
                    self: $struct_name$, ref keys: Array<felt252>, ref values: Array<felt252>
                ) {$append_members$
                }
            }"},
        HashMap::from([
            (String::from("struct_name"), struct_name),
            (String::from("append_members"), append_members),
        ]),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetABIAuxData {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Derive the `Event` trait for enums annotated with `derive(starknet::Event)`.
pub fn handle_enum(db: &dyn SyntaxGroup, enum_ast: ast::ItemEnum) -> PluginResult {
    if !derive_event_needed(&enum_ast, db) {
        return PluginResult::default();
    }

    let mut builder = PatchBuilder::new(db);
    let mut diagnostics = vec![];
    let enum_name = RewriteNode::new_trimmed(enum_ast.name(db).as_syntax_node());

    // TODO(spapini): Support generics.
    let generic_params = enum_ast.generic_params(db);
    let OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic {
            message: "Event enums with generic arguments are unsupported".to_string(),
            stable_ptr: generic_params.stable_ptr().untyped(),
        });
        return PluginResult{ code: None, diagnostics, remove_original_item: false };
    };

    let mut append_variants = vec![];
    for member in enum_ast.variants(db).elements(db) {
        let variant_name = RewriteNode::new_trimmed(member.name(db).as_syntax_node());
        let name = member.name(db).text(db);
        let variant_selector = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let append_member = RewriteNode::interpolate_patched(
            "
            $enum_name$::$variant_name$(val) => {
                array::ArrayTrait::append(ref keys, $variant_selector$);
                starknet::Event::append_keys_and_values(val, ref keys, ref values);
            },",
            HashMap::from([
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name),
                (String::from("variant_selector"), RewriteNode::Text(variant_selector)),
            ]),
        );
        append_variants.push(append_member);
    }
    let append_variants = RewriteNode::Modified(ModifiedNode { children: Some(append_variants) });

    // Add an implementation for `Event<StructName>`.
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $enum_name$IsEvent of starknet::Event<$enum_name$> {
                fn append_keys_and_values(
                    self: $enum_name$, ref keys: Array<felt252>, ref values: Array<felt252>
                ) {
                    match self {$append_variants$
                    }
                }
            }
        "},
        HashMap::from([
            (String::from("enum_name"), enum_name),
            (String::from("append_variants"), append_variants),
        ]),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetABIAuxData {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Returns true if the type should be derived as an event.
fn derive_event_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
    with_attrs.query_attr(db, "derive").into_iter().any(|attr| {
        let attr = attr.structurize(db);
        for arg in &attr.args {
            let AttributeArg{
                variant: AttributeArgVariant::Unnamed {
                    value: ast::Expr::Path(path),
                    ..
                },
                ..
            } = arg else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == "starknet::Event" {
                return true;
            }
        }
        false
    })
}

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
                RewriteNode::new_trimmed(param_name.as_syntax_node()),
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
        let mut __keys = array::array_new();
        array::array_append(ref __keys, {event_key});
        let mut __data = array::array_new();
        $param_serializations$
        starknet::syscalls::emit_event_syscall(
            array::ArrayTrait::span(@__keys),
            array::ArrayTrait::span(@__data),
        ).unwrap_syscall()
    }}
            "
                ),
                HashMap::from([
                    // TODO(yuval): All the attributes are currently copied. Remove the #[event]
                    // attr.
                    (
                        "attrs".to_string(),
                        RewriteNode::new_trimmed(function_ast.attributes(db).as_syntax_node()),
                    ),
                    (
                        "declaration".to_string(),
                        RewriteNode::new_trimmed(declaration.as_syntax_node()),
                    ),
                    (
                        "param_serializations".to_string(),
                        RewriteNode::new_modified(param_serializations),
                    ),
                ]),
            ),
            // ABI event
            RewriteNode::new_modified(vec![
                RewriteNode::Text("#[event]\n        ".to_string()),
                RewriteNode::new_trimmed(function_ast.declaration(db).as_syntax_node()),
                RewriteNode::Text(";\n        ".to_string()),
            ]),
        )),
        diagnostics,
    )
}
