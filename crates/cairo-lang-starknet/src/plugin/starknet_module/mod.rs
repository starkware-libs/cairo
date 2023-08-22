use std::vec;

use cairo_lang_defs::db::get_all_path_leafs;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};
use cairo_lang_utils::extract_matches;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;

use self::component::generate_component_specific_code;
use self::contract::generate_contract_specific_code;
use super::events::is_starknet_event;
use crate::plugin::aux_data::StarkNetContractAuxData;
use crate::plugin::consts::{
    COMPONENT_ATTR, CONTRACT_ATTR, DEPRECATED_CONTRACT_ATTR, STORAGE_ATTR, STORAGE_STRUCT_NAME,
};
use crate::plugin::events::generate_event_code;
use crate::plugin::starknet_module::generation_data::StarknetModuleCommonGenerationData;

pub mod component;
pub mod contract;
pub mod generation_data;

/// The kind of the starknet module (contract/component).
#[derive(PartialEq, Eq, Copy, Clone)]
pub enum StarknetModuleKind {
    Contract,
    Component,
}
impl StarknetModuleKind {
    /// Returns the starknet module kind according to the module's attributes, if any.
    fn from_module(db: &dyn SyntaxGroup, module_ast: &ast::ItemModule) -> Option<Self> {
        if module_ast.has_attr(db, CONTRACT_ATTR) {
            Some(StarknetModuleKind::Contract)
        } else if module_ast.has_attr(db, COMPONENT_ATTR) {
            Some(StarknetModuleKind::Component)
        } else {
            None
        }
    }
    /// Returns the name of the kind, with a leading capital letter.
    pub fn to_str_capital(self) -> &'static str {
        match self {
            Self::Contract => "Contract",
            Self::Component => "Component",
        }
    }
    /// Returns the name of the kind, lower case.
    pub fn to_str_lower(self) -> &'static str {
        match self {
            Self::Contract => "contract",
            Self::Component => "component",
        }
    }

    /// Gets the State struct name, according to the module kind.
    pub fn get_state_struct_name(self) -> String {
        format!("{}State", self.to_str_capital())
    }
    /// Gets the generic argument text, according to the module kind.
    pub fn get_generic_arg_str(self) -> &'static str {
        if matches!(self, StarknetModuleKind::Component) { "<TCS>" } else { "" }
    }
    /// Gets the generic argument text, with preceding `::`, according to the module kind.
    pub fn get_full_generic_arg_str(self) -> &'static str {
        if matches!(self, StarknetModuleKind::Component) { "::<TCS>" } else { "" }
    }
    /// Gets the full State struct name (with the generic argument), according to the module kind.
    pub fn get_full_state_struct_name(self) -> String {
        format!("{}{}", self.get_state_struct_name(), self.get_generic_arg_str())
    }
    /// Gets the member State struct name, according to the module kind.
    pub fn get_member_state_name(self) -> String {
        format!("{}MemberState", self.to_str_capital())
    }
}

/// Handles a contract/component module item.
pub(super) fn handle_module(db: &dyn SyntaxGroup, module_ast: ast::ItemModule) -> PluginResult {
    if module_ast.has_attr(db, DEPRECATED_CONTRACT_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!(
                    "The '{DEPRECATED_CONTRACT_ATTR}' attribute was deprecated, please use \
                     `{CONTRACT_ATTR}` instead.",
                ),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }
    if let Some(kind) = StarknetModuleKind::from_module(db, &module_ast) {
        return validate_module(db, module_ast, kind.to_str_capital());
    }

    PluginResult::default()
}

/// Validates the contract/component module (has body with storage named 'Storage').
fn validate_module(
    db: &dyn SyntaxGroup,
    module_ast: ast::ItemModule,
    module_kind_str: &str,
) -> PluginResult {
    let MaybeModuleBody::Some(body) = module_ast.body(db) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!("{module_kind_str}s without body are not supported."),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };
    let Some(storage_struct_ast) = body.items(db).elements(db).into_iter().find(|item| {
        matches!(item, ast::Item::Struct(struct_ast) if struct_ast.name(db).text(db) == STORAGE_STRUCT_NAME)
    }) else {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: format!("{module_kind_str}s must define a 'Storage' struct."),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    };

    if !storage_struct_ast.has_attr(db, STORAGE_ATTR) {
        return PluginResult {
            code: None,
            diagnostics: vec![PluginDiagnostic {
                message: "'Storage' struct must be annotated with #[storage].".to_string(),
                stable_ptr: module_ast.stable_ptr().untyped(),
            }],
            remove_original_item: false,
        };
    }

    PluginResult::default()
}

/// If the module is annotated with CONTRACT_ATTR or COMPONENT_ATTR, generate the relevant
/// contract/component logic.
pub(super) fn handle_module_by_storage(
    db: &dyn SyntaxGroup,
    struct_ast: ast::ItemStruct,
) -> Option<PluginResult> {
    let module_node = struct_ast.as_syntax_node().parent()?.parent()?.parent()?;
    if module_node.kind(db) != SyntaxKind::ItemModule {
        return None;
    }
    let module_ast = ast::ItemModule::from_syntax_node(db, module_node);
    let module_kind = StarknetModuleKind::from_module(db, &module_ast)?;

    let body = extract_matches!(module_ast.body(db), MaybeModuleBody::Some);
    let mut diagnostics = vec![];
    let mut common_data = StarknetModuleCommonGenerationData::default();

    // Whether an event exists in the given module. If it doesn't, we need to generate an empty one.
    let mut has_event = false;
    // Use declarations to add to the internal submodules. Mapping from 'use' items to their path.
    let mut extra_uses = OrderedHashMap::default();
    for item in body.items(db).elements(db) {
        // Skip elements that only generate other code, but their code itself is ignored.
        if matches!(&item, ast::Item::Struct(item) if item.name(db).text(db) == STORAGE_STRUCT_NAME)
        {
            continue;
        }

        if is_starknet_event(db, &mut diagnostics, &item, module_kind) {
            has_event = true;
        }

        maybe_add_extra_use(db, item, &mut extra_uses);
    }

    generate_event_code(&mut common_data, module_kind, has_event);

    common_data.extra_uses_node = RewriteNode::new_modified(
        extra_uses
            .values()
            .map(|use_path| RewriteNode::Text(format!("\n        use {use_path};")))
            .collect(),
    );

    // Generate the specific code for contract/component according to the module kind.
    let module_kind_specific_code = match module_kind {
        StarknetModuleKind::Contract => {
            generate_contract_specific_code(db, &mut diagnostics, common_data, &body, &module_ast)
        }
        StarknetModuleKind::Component => {
            generate_component_specific_code(db, &mut diagnostics, common_data, &body)
        }
    };

    let module_name = module_ast.name(db).text(db);

    let mut builder = PatchBuilder::new(db);
    builder.add_modified(module_kind_specific_code);
    Some(PluginResult {
        code: Some(PluginGeneratedFile {
            name: module_kind.to_str_lower().into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: match module_kind {
                StarknetModuleKind::Contract => {
                    Some(DynGeneratedFileAuxData::new(StarkNetContractAuxData {
                        contracts: vec![module_name],
                    }))
                }
                StarknetModuleKind::Component => None,
            },
        }),
        diagnostics,
        remove_original_item: true,
    })
}

/// Adds extra uses, to be used in the generated submodules.
fn maybe_add_extra_use(
    db: &dyn SyntaxGroup,
    item: ast::Item,
    extra_uses: &mut OrderedHashMap<smol_str::SmolStr, String>,
) {
    if let Some(ident) = match item {
        ast::Item::Use(item) => {
            let leaves = get_all_path_leafs(db, item.use_path(db));
            for leaf in leaves {
                extra_uses
                    .entry(leaf.stable_ptr().identifier(db))
                    .or_insert_with_key(|ident| format!("super::{}", ident));
            }
            None
        }
        ast::Item::Constant(item) => Some(item.name(db)),
        ast::Item::Module(item) => Some(item.name(db)),
        ast::Item::Impl(item) => Some(item.name(db)),
        ast::Item::Struct(item) => Some(item.name(db)),
        ast::Item::Enum(item) => Some(item.name(db)),
        ast::Item::TypeAlias(item) => Some(item.name(db)),
        // These items are not directly required in generated inner modules.
        ast::Item::ExternFunction(_)
        | ast::Item::ExternType(_)
        | ast::Item::Trait(_)
        | ast::Item::FreeFunction(_)
        | ast::Item::ImplAlias(_)
        | ast::Item::Missing(_) => None,
    } {
        extra_uses.entry(ident.text(db)).or_insert_with_key(|ident| format!("super::{}", ident));
    }
}
