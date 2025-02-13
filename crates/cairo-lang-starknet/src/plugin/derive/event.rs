use cairo_lang_defs::patcher::{ModifiedNode, RewriteNode};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_starknet_classes::abi::EventFieldKind;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use indoc::{formatdoc, indoc};

use crate::plugin::aux_data::StarknetEventAuxData;
use crate::plugin::consts::{
    EVENT_TRAIT, EVENT_TYPE_NAME, FLAT_ATTR, KEY_ATTR, NESTED_ATTR, SERDE_ATTR,
};
use crate::plugin::events::EventData;

/// Returns the relevant information for the `#[derive(starknet::Event)]` attribute.
pub fn handle_event_derive(
    db: &dyn SyntaxGroup,
    item_ast: &ast::ModuleItem,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<(RewriteNode, StarknetEventAuxData)> {
    match item_ast {
        ast::ModuleItem::Struct(struct_ast) => handle_struct(db, struct_ast, diagnostics),
        ast::ModuleItem::Enum(enum_ast) => handle_enum(db, enum_ast, diagnostics),
        _ => None,
    }
}

// TODO(spapini): Avoid names collisions with `keys` and `data`.
/// Derive the `Event` trait for structs annotated with `derive(starknet::Event)`.
fn handle_struct(
    db: &dyn SyntaxGroup,
    struct_ast: &ast::ItemStruct,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<(RewriteNode, StarknetEventAuxData)> {
    // TODO(spapini): Support generics.
    let generic_params = struct_ast.generic_params(db);
    let ast::OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr().untyped(),
            format!("{EVENT_TYPE_NAME} structs with generic arguments are unsupported"),
        ));
        return None;
    };

    // Generate append_keys_and_data() code.
    let mut append_members = vec![];
    let mut deserialize_members = vec![];
    let mut ctor = vec![];
    let mut members = vec![];
    for member in struct_ast.members(db).elements(db) {
        let member_name = RewriteNode::from_ast_trimmed(&member.name(db));
        let member_kind =
            get_field_kind_for_member(db, diagnostics, &member, EventFieldKind::DataSerde);
        members.push((member.name(db).text(db), member_kind));

        let member_for_append = RewriteNode::interpolate_patched(
            "self.$member_name$",
            &[("member_name".to_string(), member_name.clone())].into(),
        );
        let append_member = append_field(member_kind, member_for_append);
        let deserialize_member = deserialize_field(member_kind, member_name.clone());
        append_members.push(append_member);
        deserialize_members.push(deserialize_member);
        ctor.push(RewriteNode::interpolate_patched(
            "$member_name$, ",
            &[("member_name".to_string(), member_name)].into(),
        ));
    }
    let event_data = EventData::Struct { members };
    let append_members = RewriteNode::Modified(ModifiedNode { children: Some(append_members) });
    let deserialize_members =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_members) });
    let ctor = RewriteNode::Modified(ModifiedNode { children: Some(ctor) });

    // Add an implementation for `Event<StructName>`.
    let struct_name = RewriteNode::from_ast_trimmed(&struct_ast.name(db));
    let event_impl = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
            impl $struct_name$IsEvent of {EVENT_TRAIT}<$struct_name$> {{
                fn append_keys_and_data(
                    self: @$struct_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {{$append_members$
                }}
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$struct_name$> {{$deserialize_members$
                    Option::Some($struct_name$ {{$ctor$}})
                }}
            }}
            "
        ),
        &[
            ("struct_name".to_string(), struct_name),
            ("append_members".to_string(), append_members),
            ("deserialize_members".to_string(), deserialize_members),
            ("ctor".to_string(), ctor),
        ]
        .into(),
    );
    Some((event_impl, StarknetEventAuxData { event_data }))
}

/// Retrieves the field kind for a given struct member,
/// indicating how the field should be serialized.
/// See [EventFieldKind].
fn get_field_kind_for_member(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    member: &ast::Member,
    default: EventFieldKind,
) -> EventFieldKind {
    let is_nested = member.has_attr(db, NESTED_ATTR);
    let is_key = member.has_attr(db, KEY_ATTR);
    let is_serde = member.has_attr(db, SERDE_ATTR);

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic::error(
            member.stable_ptr().untyped(),
            "Nested event fields are currently unsupported".to_string(),
        ));
    }
    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic::error(
            member.stable_ptr().untyped(),
            "Serde event fields are currently unsupported".to_string(),
        ));
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
}

/// Retrieves the field kind for a given enum variant,
/// indicating how the field should be serialized.
/// See [EventFieldKind].
fn get_field_kind_for_variant(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    variant: &ast::Variant,
    default: EventFieldKind,
) -> EventFieldKind {
    let is_nested = variant.has_attr(db, NESTED_ATTR);
    let is_flat = variant.has_attr(db, FLAT_ATTR);
    let is_key = variant.has_attr(db, KEY_ATTR);
    let is_serde = variant.has_attr(db, SERDE_ATTR);

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic::error(
            variant.stable_ptr().untyped(),
            "Nested event fields are currently unsupported".to_string(),
        ));
    }

    if is_flat {
        return EventFieldKind::Flat;
    }

    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic::error(
            variant.stable_ptr().untyped(),
            "Serde event fields are currently unsupported".to_string(),
        ));
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
}

/// Derive the `Event` trait for enums annotated with `derive(starknet::Event)`.
fn handle_enum(
    db: &dyn SyntaxGroup,
    enum_ast: &ast::ItemEnum,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<(RewriteNode, StarknetEventAuxData)> {
    const SELECTOR: &str = "__selector__";
    let enum_name = RewriteNode::from_ast_trimmed(&enum_ast.name(db));

    // TODO(spapini): Support generics.
    let generic_params = enum_ast.generic_params(db);
    let ast::OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr().untyped(),
            format!("{EVENT_TYPE_NAME} enums with generic arguments are unsupported"),
        ));
        return None;
    };

    let mut append_variants = vec![];
    let mut deserialize_flat_variants = vec![];
    let mut deserialize_nested_variants = vec![];
    let mut variants = vec![];
    let mut event_into_impls = vec![];
    for variant in enum_ast.variants(db).elements(db) {
        let ty = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => RewriteNode::text("()"),
            ast::OptionTypeClause::TypeClause(tc) => RewriteNode::from_ast_trimmed(&tc.ty(db)),
        };

        let maybe_add_variant_to_keys = if variant.has_attr(db, FLAT_ATTR) {
            ""
        } else {
            "
                core::array::ArrayTrait::append(ref keys, selector!(\"$variant_name$\"));"
        };
        let variant_name = RewriteNode::from_ast_trimmed(&variant.name(db));
        let name = variant.name(db).text(db);
        let member_kind =
            get_field_kind_for_variant(db, diagnostics, &variant, EventFieldKind::Nested);
        variants.push((name, member_kind));

        let append_member = append_field(member_kind, RewriteNode::text("val"));
        let append_variant = RewriteNode::interpolate_patched(
            &format!(
                "
            $enum_name$::$variant_name$(val) => {{{maybe_add_variant_to_keys}$append_member$
            }},"
            ),
            &[
                ("enum_name".to_string(), enum_name.clone()),
                ("variant_name".to_string(), variant_name.clone()),
                ("append_member".to_string(), append_member),
            ]
            .into(),
        );

        if variant.has_attr(db, FLAT_ATTR) {
            let deserialize_variant = RewriteNode::interpolate_patched(
                "{
            let mut keys = keys;
            let mut data = data;
            match $try_deserialize_member$ {
                Option::Some(val) => {
                    return Option::Some($enum_name$::$variant_name$(val));
                },
                Option::None => {},
            };
        }
        ",
                &[
                    ("enum_name".to_string(), enum_name.clone()),
                    ("variant_name".to_string(), variant_name.clone()),
                    ("try_deserialize_member".to_string(), try_deserialize_field(member_kind)),
                ]
                .into(),
            );
            deserialize_flat_variants.push(deserialize_variant);
        } else {
            let deserialize_member = deserialize_field(member_kind, RewriteNode::text("val"));
            let deserialize_variant = RewriteNode::interpolate_patched(
                &format!(
                    "\
        if {SELECTOR} == selector!(\"$variant_name$\") {{$deserialize_member$
                return Option::Some($enum_name$::$variant_name$(val));
        }}
        "
                ),
                &[
                    ("enum_name".to_string(), enum_name.clone()),
                    ("variant_name".to_string(), variant_name.clone()),
                    ("deserialize_member".to_string(), deserialize_member),
                ]
                .into(),
            );

            deserialize_nested_variants.push(deserialize_variant);
        }
        append_variants.push(append_variant);
        let into_impl = RewriteNode::interpolate_patched(
            indoc! {"
                impl $enum_name$$variant_name$IntoEvent of Into<$ty$, $enum_name$> {
                    fn into(self: $ty$) -> $enum_name$ {
                        $enum_name$::$variant_name$(self)
                    }
                }
                "},
            &[
                ("enum_name".to_string(), enum_name.clone()),
                ("variant_name".to_string(), variant_name),
                ("ty".to_string(), ty),
            ]
            .into(),
        );
        event_into_impls.push(into_impl);
    }
    let event_data = EventData::Enum { variants };
    let append_variants = RewriteNode::Modified(ModifiedNode { children: Some(append_variants) });
    let deserialize_flat_variants =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_flat_variants) });
    let deserialize_nested_variants =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_nested_variants) });

    // Add an implementation for `Event<StructName>`.
    let event_impl = RewriteNode::interpolate_patched(
        &formatdoc!(
            "
            impl $enum_name$IsEvent of {EVENT_TRAIT}<$enum_name$> {{
                fn append_keys_and_data(
                    self: @$enum_name$, ref keys: Array<felt252>, ref data: Array<felt252>
                ) {{
                    match self {{$append_variants$
                    }}
                }}
                fn deserialize(
                    ref keys: Span<felt252>, ref data: Span<felt252>,
                ) -> Option<$enum_name$> {{
                    $deserialize_flat_variants$let {SELECTOR} = \
             *core::array::SpanTrait::pop_front(ref keys)?;
                    $deserialize_nested_variants$Option::None
                }}
            }}
            $event_into_impls$
        "
        ),
        &[
            ("enum_name".to_string(), enum_name),
            ("append_variants".to_string(), append_variants),
            ("deserialize_flat_variants".to_string(), deserialize_flat_variants),
            ("deserialize_nested_variants".to_string(), deserialize_nested_variants),
            (
                "event_into_impls".to_string(),
                RewriteNode::Modified(ModifiedNode { children: Some(event_into_impls) }),
            ),
        ]
        .into(),
    );

    Some((event_impl, StarknetEventAuxData { event_data }))
}

/// Generates code to emit an event for a field
fn append_field(member_kind: EventFieldKind, field: RewriteNode) -> RewriteNode {
    match member_kind {
        EventFieldKind::Nested | EventFieldKind::Flat => RewriteNode::interpolate_patched(
            &format!(
                "
                {EVENT_TRAIT}::append_keys_and_data(
                    $field$, ref keys, ref data
                );"
            ),
            &[("field".to_string(), field)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            "
                core::serde::Serde::serialize($field$, ref keys);",
            &[("field".to_string(), field)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
            core::serde::Serde::serialize($field$, ref data);",
            &[("field".to_string(), field)].into(),
        ),
    }
}

fn deserialize_field(member_kind: EventFieldKind, member_name: RewriteNode) -> RewriteNode {
    RewriteNode::interpolate_patched(
        match member_kind {
            EventFieldKind::Nested | EventFieldKind::Flat => {
                "
                let $member_name$ = starknet::Event::deserialize(
                    ref keys, ref data
                )?;"
            }
            EventFieldKind::KeySerde => {
                "
                let $member_name$ = core::serde::Serde::deserialize(
                    ref keys
                )?;"
            }
            EventFieldKind::DataSerde => {
                "
                let $member_name$ = core::serde::Serde::deserialize(
                    ref data
                )?;"
            }
        },
        &[("member_name".to_string(), member_name)].into(),
    )
}

fn try_deserialize_field(member_kind: EventFieldKind) -> RewriteNode {
    RewriteNode::text(match member_kind {
        EventFieldKind::Nested | EventFieldKind::Flat => {
            "starknet::Event::deserialize(ref keys, ref data)"
        }
        EventFieldKind::KeySerde => "core::serde::Serde::deserialize(ref keys)",
        EventFieldKind::DataSerde => "core::serde::Serde::deserialize(ref data)",
    })
}
