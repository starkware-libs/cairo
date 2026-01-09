use cairo_lang_defs::patcher::{ModifiedNode, RewriteNode};
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_starknet_classes::abi::EventFieldKind;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use indoc::{formatdoc, indoc};
use salsa::Database;

use crate::plugin::aux_data::StarknetEventAuxData;
use crate::plugin::consts::{
    EVENT_TRAIT, EVENT_TYPE_NAME, FLAT_ATTR, KEY_ATTR, NESTED_ATTR, SERDE_ATTR,
};
use crate::plugin::events::EventData;

/// Parameter names used in generated Event trait implementations.
/// These use double underscores to avoid collisions with field names.
const PARAM_KEYS: &str = "__keys__";
const PARAM_DATA: &str = "__data__";

/// Returns the relevant information for the `#[derive(starknet::Event)]` attribute.
pub fn handle_event_derive<'db>(
    db: &'db dyn Database,
    item_ast: &ast::ModuleItem<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Option<(RewriteNode<'db>, StarknetEventAuxData)> {
    match item_ast {
        ast::ModuleItem::Struct(struct_ast) => handle_struct(db, struct_ast, diagnostics),
        ast::ModuleItem::Enum(enum_ast) => handle_enum(db, enum_ast, diagnostics),
        _ => None,
    }
}

/// Derives the `Event` trait for structs annotated with `derive(starknet::Event)`.
fn handle_struct<'db>(
    db: &'db dyn Database,
    struct_ast: &ast::ItemStruct<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Option<(RewriteNode<'db>, StarknetEventAuxData)> {
    // TODO(spapini): Support generics.
    let generic_params = struct_ast.generic_params(db);
    let ast::OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr(db).untyped(),
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
        let member_name_node = member.name(db);
        let member_name_text = member_name_node.text(db);

        let member_name = RewriteNode::from_ast_trimmed(&member_name_node);
        let member_kind =
            get_field_kind_for_member(db, diagnostics, &member, EventFieldKind::DataSerde);
        members.push((member_name_text, member_kind));

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
    let event_data = EventData::Struct {
        members: members.into_iter().map(|(name, kind)| (name.to_string(db), kind)).collect(),
    };
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
                    self: @$struct_name$, ref {PARAM_KEYS}: Array<felt252>, ref {PARAM_DATA}: \
             Array<felt252>
                ) {{$append_members$
                }}
                fn deserialize(
                    ref {PARAM_KEYS}: Span<felt252>, ref {PARAM_DATA}: Span<felt252>,
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
fn get_field_kind_for_member<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    member: &ast::Member<'db>,
    default: EventFieldKind,
) -> EventFieldKind {
    let is_nested = member.has_attr(db, NESTED_ATTR);
    let is_key = member.has_attr(db, KEY_ATTR);
    let is_serde = member.has_attr(db, SERDE_ATTR);

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic::error(
            member.stable_ptr(db).untyped(),
            "Nested event fields are currently unsupported".to_string(),
        ));
    }
    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic::error(
            member.stable_ptr(db).untyped(),
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
fn get_field_kind_for_variant<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    variant: &ast::Variant<'db>,
    default: EventFieldKind,
) -> EventFieldKind {
    let is_nested = variant.has_attr(db, NESTED_ATTR);
    let is_flat = variant.has_attr(db, FLAT_ATTR);
    let is_key = variant.has_attr(db, KEY_ATTR);
    let is_serde = variant.has_attr(db, SERDE_ATTR);

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic::error(
            variant.stable_ptr(db),
            "Nested event fields are currently unsupported".to_string(),
        ));
    }

    if is_flat {
        return EventFieldKind::Flat;
    }

    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic::error(
            variant.stable_ptr(db),
            "Serde event fields are currently unsupported".to_string(),
        ));
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
}

/// Derives the `Event` trait for enums annotated with `derive(starknet::Event)`.
fn handle_enum<'db>(
    db: &'db dyn Database,
    enum_ast: &ast::ItemEnum<'db>,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
) -> Option<(RewriteNode<'db>, StarknetEventAuxData)> {
    const SELECTOR: &str = "__selector__";
    let enum_name = RewriteNode::from_ast_trimmed(&enum_ast.name(db));

    // TODO(spapini): Support generics.
    let generic_params = enum_ast.generic_params(db);
    let ast::OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic::error(
            generic_params.stable_ptr(db),
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
            String::new()
        } else {
            format!(
                "
                core::array::ArrayTrait::append(ref {PARAM_KEYS}, selector!(\"$variant_name$\"));"
            )
        };
        let variant_name_node = variant.name(db);
        let variant_name_text = variant_name_node.text(db);

        let variant_name = RewriteNode::from_ast_trimmed(&variant_name_node);
        let member_kind =
            get_field_kind_for_variant(db, diagnostics, &variant, EventFieldKind::Nested);
        variants.push((variant_name_text, member_kind));

        let append_member = append_field(member_kind, RewriteNode::text("val"));
        let append_variant_template = format!(
            "
            $enum_name$::$variant_name$(val) => {{{}$append_member$
            }},",
            maybe_add_variant_to_keys
        );
        let append_variant = RewriteNode::interpolate_patched(
            &append_variant_template,
            &[
                ("enum_name".to_string(), enum_name.clone()),
                ("variant_name".to_string(), variant_name.clone()),
                ("append_member".to_string(), append_member),
            ]
            .into(),
        );

        if variant.has_attr(db, FLAT_ATTR) {
            let deserialize_variant = RewriteNode::interpolate_patched(
                &format!(
                    "{{
            let mut keys = {PARAM_KEYS};
            let mut data = {PARAM_DATA};
            match $try_deserialize_member$ {{
                Option::Some(val) => {{
                    return Option::Some($enum_name$::$variant_name$(val));
                }},
                Option::None => {{}},
            }};
        }}
        "
                ),
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
    let event_data = EventData::Enum {
        variants: variants.into_iter().map(|(name, kind)| (name.to_string(db), kind)).collect(),
    };
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
                    self: @$enum_name$, ref {PARAM_KEYS}: Array<felt252>, ref {PARAM_DATA}: \
             Array<felt252>
                ) {{
                    match self {{$append_variants$
                    }}
                }}
                fn deserialize(
                    ref {PARAM_KEYS}: Span<felt252>, ref {PARAM_DATA}: Span<felt252>,
                ) -> Option<$enum_name$> {{
                    $deserialize_flat_variants$let {SELECTOR} = \
             *core::array::SpanTrait::pop_front(ref {PARAM_KEYS})?;
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
fn append_field<'db>(member_kind: EventFieldKind, field: RewriteNode<'db>) -> RewriteNode<'db> {
    match member_kind {
        EventFieldKind::Nested | EventFieldKind::Flat => RewriteNode::interpolate_patched(
            &format!(
                "
                {EVENT_TRAIT}::append_keys_and_data(
                    $field$, ref {PARAM_KEYS}, ref {PARAM_DATA}
                );"
            ),
            &[("field".to_string(), field)].into(),
        ),
        EventFieldKind::KeySerde => RewriteNode::interpolate_patched(
            &format!(
                "
                core::serde::Serde::serialize($field$, ref {PARAM_KEYS});"
            ),
            &[("field".to_string(), field)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            &format!(
                "
            core::serde::Serde::serialize($field$, ref {PARAM_DATA});"
            ),
            &[("field".to_string(), field)].into(),
        ),
    }
}

fn deserialize_field<'db>(
    member_kind: EventFieldKind,
    member_name: RewriteNode<'db>,
) -> RewriteNode<'db> {
    let template = match member_kind {
        EventFieldKind::Nested | EventFieldKind::Flat => format!(
            "
                let $member_name$ = starknet::Event::deserialize(
                    ref {PARAM_KEYS}, ref {PARAM_DATA}
                )?;"
        ),
        EventFieldKind::KeySerde => format!(
            "
                let $member_name$ = core::serde::Serde::deserialize(
                    ref {PARAM_KEYS}
                )?;"
        ),
        EventFieldKind::DataSerde => format!(
            "
                let $member_name$ = core::serde::Serde::deserialize(
                    ref {PARAM_DATA}
                )?;"
        ),
    };
    RewriteNode::interpolate_patched(&template, &[("member_name".to_string(), member_name)].into())
}

fn try_deserialize_field<'db>(member_kind: EventFieldKind) -> RewriteNode<'db> {
    let text = match member_kind {
        EventFieldKind::Nested | EventFieldKind::Flat => {
            format!("starknet::Event::deserialize(ref {PARAM_KEYS}, ref {PARAM_DATA})")
        }
        EventFieldKind::KeySerde => format!("core::serde::Serde::deserialize(ref {PARAM_KEYS})"),
        EventFieldKind::DataSerde => format!("core::serde::Serde::deserialize(ref {PARAM_DATA})"),
    };
    RewriteNode::text(&text)
}
