use cairo_lang_defs::db::get_all_path_leafs;
use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use const_format::formatcp;
use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use super::consts::{EVENT_ATTR, EVENT_TRAIT, EVENT_TYPE_NAME};
use super::starknet_module::StarknetModuleKind;

/// Generated auxiliary data for the `#[derive(starknet::Event)]` attribute.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum EventData {
    Struct { members: Vec<(SmolStr, EventFieldKind)> },
    Enum { variants: Vec<(SmolStr, EventFieldKind)> },
}

/// Describes how to serialize the event's field.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize, Hash)]
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
    // Serialize as a flat event.
    #[serde(rename = "flat")]
    Flat,
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

/// The code for an empty event.
pub const EMPTY_EVENT_CODE: &str = formatcp! {"\
#[{EVENT_ATTR}]
#[derive(Drop, {EVENT_TRAIT})]
pub enum {EVENT_TYPE_NAME} {{}}
"};

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
