use cairo_lang_defs::db::get_all_path_leafs;
use cairo_lang_defs::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{self, OptionWrappedGenericParamList};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::{formatdoc, indoc};
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use super::aux_data::StarkNetEventAuxData;
use super::consts::{EVENT_ATTR, EVENT_TRAIT, EVENT_TYPE_NAME, NESTED_ATTR};
use super::starknet_module::StarknetModuleKind;

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
            message: format!("{EVENT_TYPE_NAME} structs with generic arguments are unsupported"),
            stable_ptr: generic_params.stable_ptr().untyped(),
        });
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    };

    // Generate append_keys_and_data() code.
    let mut append_members = vec![];
    let mut deserialize_members = vec![];
    let mut ctor = vec![];
    let mut members = vec![];
    for member in struct_ast.members(db).elements(db) {
        let member_name = RewriteNode::new_trimmed(member.name(db).as_syntax_node());
        let member_kind =
            get_field_kind_for_member(db, &mut diagnostics, &member, EventFieldKind::DataSerde);
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
    let struct_name = RewriteNode::new_trimmed(struct_ast.name(db).as_syntax_node());
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
            }}"
        ),
        &[
            ("struct_name".to_string(), struct_name),
            ("append_members".to_string(), append_members),
            ("deserialize_members".to_string(), deserialize_members),
            ("ctor".to_string(), ctor),
        ]
        .into(),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: Some(DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })),
        }),
        diagnostics,
        remove_original_item: false,
    }
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
    let is_key = variant.has_attr(db, "key");
    let is_serde = variant.has_attr(db, "serde");

    // Currently, nested fields are unsupported.
    if is_nested {
        diagnostics.push(PluginDiagnostic {
            message: "Nested event fields are currently unsupported".to_string(),
            stable_ptr: variant.stable_ptr().untyped(),
        });
    }
    // Currently, serde fields are unsupported.
    if is_serde {
        diagnostics.push(PluginDiagnostic {
            message: "Serde event fields are currently unsupported".to_string(),
            stable_ptr: variant.stable_ptr().untyped(),
        });
    }

    if is_key {
        return EventFieldKind::KeySerde;
    }
    default
}

/// Derive the `Event` trait for enums annotated with `derive(starknet::Event)`.
pub fn handle_enum(db: &dyn SyntaxGroup, enum_ast: ast::ItemEnum) -> PluginResult {
    let mut builder = PatchBuilder::new(db);
    let mut diagnostics = vec![];
    let enum_name = RewriteNode::new_trimmed(enum_ast.name(db).as_syntax_node());

    // TODO(spapini): Support generics.
    let generic_params = enum_ast.generic_params(db);
    let OptionWrappedGenericParamList::Empty(_) = generic_params else {
        diagnostics.push(PluginDiagnostic {
            message: format!("{EVENT_TYPE_NAME} enums with generic arguments are unsupported"),
            stable_ptr: generic_params.stable_ptr().untyped(),
        });
        return PluginResult { code: None, diagnostics, remove_original_item: false };
    };

    let mut append_variants = vec![];
    let mut deserialize_variants = vec![];
    let mut variants = vec![];
    let mut event_into_impls = vec![];
    for variant in enum_ast.variants(db).elements(db) {
        let ty = match variant.type_clause(db) {
            ast::OptionTypeClause::Empty(_) => RewriteNode::Text("()".to_string()),
            ast::OptionTypeClause::TypeClause(tc) => {
                RewriteNode::new_trimmed(tc.ty(db).as_syntax_node())
            }
        };
        let variant_name = RewriteNode::new_trimmed(variant.name(db).as_syntax_node());
        let name = variant.name(db).text(db);
        let member_kind =
            get_field_kind_for_variant(db, &mut diagnostics, &variant, EventFieldKind::Nested);
        variants.push((name, member_kind));
        let append_member = append_field(member_kind, RewriteNode::Text("val".into()));
        let append_variant = RewriteNode::interpolate_patched(
            "
            $enum_name$::$variant_name$(val) => {
                array::ArrayTrait::append(ref keys, selector!(\"$variant_name$\"));$append_member$
            },",
            &[
                ("enum_name".to_string(), enum_name.clone()),
                ("variant_name".to_string(), variant_name.clone()),
                ("append_member".to_string(), append_member),
            ]
            .into(),
        );
        let deserialize_member = deserialize_field(member_kind, RewriteNode::Text("val".into()));
        let deserialize_variant = RewriteNode::interpolate_patched(
            "
            if selector == selector!(\"$variant_name$\") {$deserialize_member$
                return Option::Some($enum_name$::$variant_name$(val));
            }",
            &[
                ("enum_name".to_string(), enum_name.clone()),
                ("variant_name".to_string(), variant_name.clone()),
                ("deserialize_member".to_string(), deserialize_member),
            ]
            .into(),
        );
        append_variants.push(append_variant);
        deserialize_variants.push(deserialize_variant);
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
    let deserialize_variants =
        RewriteNode::Modified(ModifiedNode { children: Some(deserialize_variants) });

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
                    let selector = *array::SpanTrait::pop_front(ref keys)?;
                    $deserialize_variants$
                    Option::None
                }}
            }}
            $event_into_impls$
        "
        ),
        &[
            ("enum_name".to_string(), enum_name),
            ("append_variants".to_string(), append_variants),
            ("deserialize_variants".to_string(), deserialize_variants),
            (
                "event_into_impls".to_string(),
                RewriteNode::Modified(ModifiedNode { children: Some(event_into_impls) }),
            ),
        ]
        .into(),
    );

    builder.add_modified(event_impl);

    PluginResult {
        code: Some(PluginGeneratedFile {
            name: "event_impl".into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: Some(DynGeneratedFileAuxData::new(StarkNetEventAuxData { event_data })),
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
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed { value: ast::Expr::Path(path), .. },
                ..
            } = arg
            else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == EVENT_TRAIT {
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
                serde::Serde::serialize($field$, ref keys);",
            &[("field".to_string(), field)].into(),
        ),
        EventFieldKind::DataSerde => RewriteNode::interpolate_patched(
            "
                serde::Serde::serialize($field$, ref data);",
            &[("field".to_string(), field)].into(),
        ),
    }
}

fn deserialize_field(member_kind: EventFieldKind, member_name: RewriteNode) -> RewriteNode {
    RewriteNode::interpolate_patched(
        match member_kind {
            EventFieldKind::Nested => {
                "
                let $member_name$ = starknet::Event::deserialize(
                    ref keys, ref data
                )?;"
            }
            EventFieldKind::KeySerde => {
                "
                let $member_name$ = serde::Serde::deserialize(
                    ref keys
                )?;"
            }
            EventFieldKind::DataSerde => {
                "
                let $member_name$ = serde::Serde::deserialize(
                    ref data
                )?;"
            }
        },
        &[("member_name".to_string(), member_name)].into(),
    )
}

/// Checks whether the given item is a starknet event, and if so - makes sure it's valid and returns
/// its variants. Returns None if it's not a starknet event.
pub fn get_starknet_event_variants(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    item: &ast::Item,
    module_kind: StarknetModuleKind,
) -> Option<Vec<SmolStr>> {
    let (has_event_name, stable_ptr, variants) = match item {
        ast::Item::Struct(strct) => (
            strct.name(db).text(db) == EVENT_TYPE_NAME,
            strct.name(db).stable_ptr().untyped(),
            vec![],
        ),
        ast::Item::Enum(enm) => {
            let has_event_name = enm.name(db).text(db) == EVENT_TYPE_NAME;
            let variants = if has_event_name {
                enm.variants(db).elements(db).into_iter().map(|v| v.name(db).text(db)).collect()
            } else {
                vec![]
            };
            (has_event_name, enm.name(db).stable_ptr().untyped(), variants)
        }
        ast::Item::Use(item) => {
            for leaf in get_all_path_leafs(db, item.use_path(db)) {
                let stable_ptr = &leaf.stable_ptr();
                if stable_ptr.identifier(db) == EVENT_TYPE_NAME {
                    if !item.has_attr(db, EVENT_ATTR) {
                        diagnostics.push(PluginDiagnostic {
                            message: format!(
                                "{} type that is named `{EVENT_TYPE_NAME}` must be marked with \
                                 #[{EVENT_ATTR}].",
                                module_kind.to_str_capital()
                            ),
                            stable_ptr: stable_ptr.untyped(),
                        });
                    }
                    return Some(vec![]);
                }
            }
            return None;
        }
        _ => return None,
    };
    let has_event_attr = item.has_attr(db, EVENT_ATTR);

    match (has_event_attr, has_event_name) {
        (true, false) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "{} type that is marked with #[{EVENT_ATTR}] must be named \
                     `{EVENT_TYPE_NAME}`.",
                    module_kind.to_str_capital()
                ),
                stable_ptr,
            });
            None
        }
        (false, true) => {
            diagnostics.push(PluginDiagnostic {
                message: format!(
                    "{} type that is named `{EVENT_TYPE_NAME}` must be marked with \
                     #[{EVENT_ATTR}].",
                    module_kind.to_str_capital()
                ),
                stable_ptr,
            });
            // The attribute is missing, but this counts as a event - we can't create another
            // (empty) event.
            Some(variants)
        }
        (true, true) => Some(variants),
        (false, false) => None,
    }
}
