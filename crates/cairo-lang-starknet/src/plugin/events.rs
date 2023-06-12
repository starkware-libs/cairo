use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{self, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::indoc;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use super::aux_data::StarkNetEventAuxData;
use crate::contract::starknet_keccak;

/// Generated auxiliary data for the `#[derive(starknet::Event)]` attribute.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EventData {
    Struct { members: Vec<(SmolStr, EventFieldKind)> },
    Enum { variants: Vec<(SmolStr, EventFieldKind)> },
}

/// Describes how to serialize the event's field.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum EventFieldKind {
    // Serialize to `keys` using `Serde`.
    #[serde(rename = "key")]
    KeySerde,
    // Serialize to `data` using `Serde`.
    #[serde(rename = "data")]
    DataSerde,
    // Serialize as a nested event.
    #[serde(rename = "nested")]
    Nested,
}

// TODO(spapini): Avoid names collisions with `keys` and `data`.
/// Derive the `Event` trait for structs annotated with `derive(starknet::Event)`.
pub fn handle_struct(db: &dyn SyntaxGroup, struct_ast: ast::ItemStruct) -> PluginResult {
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

    // Generate append_keys_and_data() code.
    let mut append_members = vec![];
    let mut deserialize_members = vec![];
    let mut ctor = vec![];
    let mut members = vec![];
    for member in struct_ast.members(db).elements(db) {
        let member_name = RewriteNode::new_trimmed(member.name(db).as_syntax_node());
        let member_kind = get_field_kind(db, &mut diagnostics, &member, EventFieldKind::DataSerde);
        members.push((member.name(db).text(db), member_kind));

        let member_for_append = RewriteNode::interpolate_patched(
            "self.$member_name$",
            [(String::from("member_name"), member_name.clone())].into(),
        );
        let append_member = append_field(member_kind, member_for_append);
        let deserialize_member = deserialize_field(member_kind, member_name.clone());
        append_members.push(append_member);
        deserialize_members.push(deserialize_member);
        ctor.push(RewriteNode::interpolate_patched(
            "$member_name$, ",
            [(String::from("member_name"), member_name)].into(),
        ));
    }
    let event_data = EventData::Struct { members };
    let append_members = RewriteNode::Modified(ModifiedNode { children: Some(append_members) });
    let deserialize_members =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_members) });
    let ctor = RewriteNode::Modified(ModifiedNode { children: Some(ctor) });

    // Add an implementation for `Event<StructName>`.
    let struct_name = RewriteNode::new_trimmed(struct_ast.name(db).as_syntax_node());
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $struct_name$IsEvent of starknet::Event<$struct_name$> {
                fn append_keys_and_data(
                    self: @$struct_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {$append_members$
                }
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$struct_name$> {$deserialize_members$
                    Option::Some($struct_name$ {$ctor$})
                }
            }"},
        [
            (String::from("struct_name"), struct_name),
            (String::from("append_members"), append_members),
            (String::from("deserialize_members"), deserialize_members),
            (String::from("ctor"), ctor),
        ]
        .into(),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetEventAuxData {
                patches: builder.patches,
                event_data,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

fn get_field_kind(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    member: &ast::Member,
    default: EventFieldKind,
) -> EventFieldKind {
    let is_nested = member.has_attr(db, "nested");
    let is_key = member.has_attr(db, "key");
    let is_serde = member.has_attr(db, "serde");

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic {
            message: "Nested event fields are currently unsupported".to_string(),
            stable_ptr: member.stable_ptr().untyped(),
        });
    }
    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic {
            message: "Serde event fields are currently unsupported".to_string(),
            stable_ptr: member.stable_ptr().untyped(),
        });
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
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
    let mut deserialize_variants = vec![];
    let mut variants = vec![];
    for variant in enum_ast.variants(db).elements(db) {
        let variant_name = RewriteNode::new_trimmed(variant.name(db).as_syntax_node());
        let name = variant.name(db).text(db);
        let variant_selector = format!("0x{:x}", starknet_keccak(name.as_bytes()));
        let member_kind = get_field_kind(db, &mut diagnostics, &variant, EventFieldKind::Nested);
        variants.push((name, member_kind));
        let append_member = append_field(member_kind, RewriteNode::Text("val".into()));
        let append_variant = RewriteNode::interpolate_patched(
            "
            $enum_name$::$variant_name$(val) => {
                array::ArrayTrait::append(ref keys, $variant_selector$);$append_member$
            },",
            [
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name.clone()),
                (String::from("variant_selector"), RewriteNode::Text(variant_selector.clone())),
                (String::from("append_member"), append_member),
            ]
            .into(),
        );
        let deserialize_member = deserialize_field(member_kind, RewriteNode::Text("val".into()));
        let deserialize_variant = RewriteNode::interpolate_patched(
            "
            if selector == $variant_selector$ {$deserialize_member$
                return Option::Some($enum_name$::$variant_name$(val));
            }",
            [
                (String::from("enum_name"), enum_name.clone()),
                (String::from("variant_name"), variant_name),
                (String::from("variant_selector"), RewriteNode::Text(variant_selector)),
                (String::from("deserialize_member"), deserialize_member),
            ]
            .into(),
        );
        append_variants.push(append_variant);
        deserialize_variants.push(deserialize_variant);
    }
    let event_data = EventData::Enum { variants };
    let append_variants = RewriteNode::Modified(ModifiedNode { children: Some(append_variants) });
    let deserialize_variants =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_variants) });

    // Add an implementation for `Event<StructName>`.
    let event_impl = RewriteNode::interpolate_patched(
        indoc! {"
            impl $enum_name$IsEvent of starknet::Event<$enum_name$> {
                fn append_keys_and_data(
                    self: @$enum_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {
                    match self {$append_variants$
                    }
                }
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$enum_name$> {
                    let selector = *array::SpanTrait::pop_front(ref keys)?;
                    $deserialize_variants$
                    Option::None(())
                }
            }
        "},
        [
            (String::from("enum_name"), enum_name),
            (String::from("append_variants"), append_variants),
            (String::from("deserialize_variants"), deserialize_variants),
        ]
        .into(),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetEventAuxData {
                patches: builder.patches,
                event_data,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

/// Returns true if the type should be derived as an event.
pub fn derive_event_needed<T: QueryAttrs>(with_attrs: &T, db: &dyn SyntaxGroup) -> bool {
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

/// Generates code to emit an event for a field
fn append_field(member_kind: EventFieldKind, field: RewriteNode) -> RewriteNode {
    match member_kind {
        EventFieldKind::Nested => RewriteNode::interpolate_patched(
            "
                starknet::Event::append_keys_and_data(
                    $field$, ref keys, ref data
                );",
            [(String::from("field"), field)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            "
                serde::Serde::serialize($field$, ref keys);",
            [(String::from("field"), field)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
                serde::Serde::serialize($field$, ref data);",
            [(String::from("field"), field)].into(),
        ),
    }
}

fn deserialize_field(member_kind: EventFieldKind, member_name: RewriteNode) -> RewriteNode {
    match member_kind {
        EventFieldKind::Nested => RewriteNode::interpolate_patched(
            "
                let $member_name$ = starknet::Event::deserialize(
                    ref keys, ref data
                )?;",
            [(String::from("member_name"), member_name)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            "
                let $member_name$ = serde::Serde::deserialize(
                    ref keys
                )?;",
            [(String::from("member_name"), member_name)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
                let $member_name$ = serde::Serde::deserialize(
                    ref data
                )?;",
            [(String::from("member_name"), member_name)].into(),
        ),
    }
}
