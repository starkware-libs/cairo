//! Parser utilities for the with_components macro.

use crate::{
    constants::{
        CONSTRUCTOR_ATTRIBUTE, CONTRACT_ATTRIBUTE, EVENT_ENUM_NAME, FLAT_ATTRIBUTE,
        STORAGE_STRUCT_NAME, SUBSTORAGE_ATTRIBUTE,
    },
    utils::tabs,
};
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_macro::{Diagnostic, Diagnostics};
use cairo_lang_syntax::node::helpers::{BodyItems, QueryAttrs};
use cairo_lang_syntax::node::{
    ast::{self, MaybeModuleBody},
    db::SyntaxGroup,
    SyntaxNode, Terminal, TypedSyntaxNode,
};
use indoc::indoc;
use regex::Regex;

use super::{
    components::{AllowedComponents, ComponentInfo},
    diagnostics::{errors, warnings},
};

/// The parser for the with_components macro.
pub struct WithComponentsParser<'a> {
    /// The base node.
    base_node: SyntaxNode,
    /// The components info.
    components_info: &'a [ComponentInfo<'a>],
}

impl<'a> WithComponentsParser<'a> {
    /// Creates a new parser for the with_components macro.
    pub fn new(base_node: SyntaxNode, components_info: &'a [ComponentInfo]) -> Self {
        Self {
            base_node,
            components_info,
        }
    }

    /// Parses the module and returns the patched code.
    pub fn parse(&mut self, db: &dyn SyntaxGroup) -> (String, Diagnostics) {
        let base_node = self.base_node.clone();
        let mut builder = PatchBuilder::new_ex(db, &base_node);

        let typed = ast::SyntaxFile::from_syntax_node(db, base_node);
        let mut base_rnode = RewriteNode::from_ast(&typed);
        let module_rnode = base_rnode
            .modify_child(db, ast::SyntaxFile::INDEX_ITEMS)
            .modify_child(db, 0);

        // Validate the contract module
        let (errors, mut warnings) =
            validate_contract_module(db, module_rnode, self.components_info);
        if !errors.is_empty() {
            return (String::new(), errors.into());
        }

        // Get the body node
        let body_rnode = module_rnode.modify_child(db, ast::ItemModule::INDEX_BODY);

        process_module_items(body_rnode, db, self.components_info);
        add_use_clauses_and_macros(body_rnode, db, self.components_info);

        builder.add_modified(base_rnode);
        let (content, _) = builder.build();

        // Add warnings for each component
        for component_info in self.components_info.iter() {
            let component_warnings = add_per_component_warnings(&content, component_info);
            warnings.extend(component_warnings);
        }

        (content, warnings.into())
    }
}

/// Validates that the contract module:
///
/// - Has the `#[starknet::contract]` attribute.
/// - Has a constructor calling the corresponding initializers.
/// - Has the corresponding immutable configs.
///
/// NOTE: Missing initializers and configs are added as Warnings.
/// NOTE: When an error is found, the functions doesn't return any warnings to avoid noise.
///
/// # Returns
///
/// * `errors` - The errors that arose during the validation.
/// * `warnings` - The warnings that arose during the validation.
fn validate_contract_module(
    db: &dyn SyntaxGroup,
    node: &mut RewriteNode,
    components_info: &[ComponentInfo],
) -> (Vec<Diagnostic>, Vec<Diagnostic>) {
    let mut warnings = vec![];

    if let RewriteNode::Copied(copied) = node {
        let item = ast::ItemModule::from_syntax_node(db, copied.clone());

        // 1. Check that the module has a body (error)
        let MaybeModuleBody::Some(body) = item.body(db) else {
            let error = Diagnostic::error(errors::NO_BODY);
            return (vec![error], vec![]);
        };

        // 2. Check that the module has the `#[starknet::contract]` attribute (error)
        if !item.has_attr(db, CONTRACT_ATTRIBUTE) {
            let error = Diagnostic::error(errors::NO_CONTRACT_ATTRIBUTE(CONTRACT_ATTRIBUTE));
            return (vec![error], vec![]);
        }

        // 3. Check that the module has the corresponding initializers (warning)
        let components_with_initializer = components_info
            .iter()
            .filter(|c| c.has_initializer)
            .collect::<Vec<&ComponentInfo>>();

        if !components_with_initializer.is_empty() {
            let constructor = body.items_vec(db).into_iter().find(|item| {
            matches!(item, ast::ModuleItem::FreeFunction(function_ast) if function_ast.has_attr(db, CONSTRUCTOR_ATTRIBUTE))
        });
            let constructor_code = if let Some(constructor) = constructor {
                // Get the constructor code (maybe we can do this without the builder)
                let constructor_ast = constructor.as_syntax_node();
                let typed = ast::ModuleItem::from_syntax_node(db, constructor_ast.clone());
                let constructor_rnode = RewriteNode::from_ast(&typed);
                let mut builder = PatchBuilder::new_ex(db, &constructor_ast);
                builder.add_modified(constructor_rnode);
                let (code, _) = builder.build();
                code
            } else {
                String::new()
            };
            let mut components_with_initializer_missing = vec![];
            for component in components_with_initializer.iter() {
                if !constructor_code.contains(&format!("self.{}.initializer(", component.storage)) {
                    components_with_initializer_missing.push(component.short_name());
                }
            }

            if !components_with_initializer_missing.is_empty() {
                let components_with_initializer_missing_str =
                    components_with_initializer_missing.join(", ");
                let warning = Diagnostic::warn(warnings::INITIALIZERS_MISSING(
                    &components_with_initializer_missing_str,
                ));
                warnings.push(warning);
            }
        }

        // 4. Check that the contract has the corresponding immutable configs
        for component in components_info.iter().filter(|c| c.has_immutable_config) {
            // Get the body code (maybe we can do this without the builder)
            let body_ast = body.as_syntax_node();
            let typed = ast::ModuleBody::from_syntax_node(db, body_ast.clone());
            let body_rnode = RewriteNode::from_ast(&typed);

            let mut builder = PatchBuilder::new_ex(db, &body_ast);
            builder.add_modified(body_rnode);
            let (code, _) = builder.build();

            // Check if the DefaultConfig is used
            let component_parent_path = component
                .path
                .strip_suffix(&component.name)
                .expect("Component path must end with the component name");
            let re = Regex::new(&format!(
                r"use {}[{{\w, \n]*DefaultConfig[{{\w}}, \n]*;",
                component_parent_path
            ))
            .unwrap();

            let default_config_used = re.is_match(&code);
            if !default_config_used {
                let immutable_config_implemented =
                    code.contains(&format!("of {}::ImmutableConfig", component.name));
                if !immutable_config_implemented {
                    let warning = Diagnostic::warn(warnings::IMMUTABLE_CONFIG_MISSING(
                        component.short_name(),
                        &format!("{}DefaultConfig", component_parent_path),
                    ));
                    warnings.push(warning);
                }
            }
        }
    }

    (vec![], warnings)
}

/// Adds warnings that may be helpful for users.
fn add_per_component_warnings(code: &str, component_info: &ComponentInfo) -> Vec<Diagnostic> {
    let mut warnings = vec![];

    match component_info.kind() {
        AllowedComponents::Vesting => {
            // Check that the VestingScheduleTrait is implemented
            let linear_impl_used = code.contains("LinearVestingSchedule");
            let vesting_trait_used = code.contains("VestingScheduleTrait");
            if !linear_impl_used && !vesting_trait_used {
                let warning = Diagnostic::warn(warnings::VESTING_SCHEDULE_IMPL_MISSING);
                warnings.push(warning);
            }
        }
        AllowedComponents::Initializable => {
            // Check that the initialize internal function is called
            let initialize_internal_function_called =
                code.contains("self.initializable.initialize()");
            if !initialize_internal_function_called {
                let warning = Diagnostic::warn(warnings::INITIALIZABLE_NOT_USED);
                warnings.push(warning);
            }
        }
        AllowedComponents::Pausable => {
            // Check that the pause and unpause functions are called
            let pause_function_called = code.contains("self.pausable.pause()");
            let unpause_function_called = code.contains("self.pausable.unpause()");
            if !pause_function_called || !unpause_function_called {
                let warning = Diagnostic::warn(warnings::PAUSABLE_NOT_USED);
                warnings.push(warning);
            }
        }
        AllowedComponents::ERC20 => {
            // Check that the ERC20HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC20HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC20HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(warnings::ERC20_HOOKS_IMPL_MISSING);
                warnings.push(warning);
            }
        }
        AllowedComponents::ERC721 => {
            // Check that the ERC721HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC721HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC721HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(warnings::ERC721_HOOKS_IMPL_MISSING);
                warnings.push(warning);
            }
        }
        AllowedComponents::ERC1155 => {
            // Check that the ERC1155HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC1155HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC1155HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(warnings::ERC1155_HOOKS_IMPL_MISSING);
                warnings.push(warning);
            }
        }
        AllowedComponents::Upgradeable => {
            // Check that the upgrade function is called
            let upgrade_function_called = code.contains("self.upgradeable.upgrade");
            if !upgrade_function_called {
                let warning = Diagnostic::warn(warnings::UPGRADEABLE_NOT_USED);
                warnings.push(warning);
            }
        }
        AllowedComponents::Votes => {
            // Check that the SNIP12Metadata is implemented
            let snip12_metadata_implemented = code.contains("of SNIP12Metadata");
            if !snip12_metadata_implemented {
                let warning = Diagnostic::warn(warnings::SNIP12_METADATA_IMPL_MISSING);
                warnings.push(warning);
            }
        }
        _ => {}
    }
    warnings
}

/// Iterates over the items in the body node and processes them.
fn process_module_items(
    body_rnode: &mut RewriteNode,
    db: &dyn SyntaxGroup,
    components_info: &[ComponentInfo],
) {
    let items_rnode = body_rnode.modify_child(db, ast::ModuleBody::INDEX_ITEMS);
    let items_mnode = items_rnode.modify(db);
    let mut event_enum_found = false;

    for item_rnode in items_mnode.children.as_mut().unwrap() {
        if let RewriteNode::Copied(copied) = item_rnode {
            let item = ast::ModuleItem::from_syntax_node(db, copied.clone());

            match item {
                ast::ModuleItem::Struct(item_struct)
                    if item_struct.name(db).text(db) == STORAGE_STRUCT_NAME =>
                {
                    process_storage_struct(item_rnode, db, components_info);
                }
                ast::ModuleItem::Enum(item_enum)
                    if item_enum.name(db).text(db) == EVENT_ENUM_NAME =>
                {
                    process_event_enum(item_rnode, db, components_info);
                    event_enum_found = true;
                }
                _ => {}
            }
        }
    }

    // If the event enum is not found, add it.
    if !event_enum_found {
        add_event_enum(body_rnode, db, components_info);
    }
}

/// Modifies the storage struct to add the component entries.
fn process_storage_struct(
    item_struct: &mut RewriteNode,
    db: &dyn SyntaxGroup,
    components_info: &[ComponentInfo],
) {
    let item_struct_mnode = item_struct.modify(db);
    let item_struct_children = item_struct_mnode.children.as_mut().unwrap();
    let components_rnode =
        ComponentsGenerationData(components_info).generate_for_storage_struct(db);

    // Insert the components at the beginning of the struct body.
    item_struct_children.insert(ast::ItemStruct::INDEX_LBRACE + 1, components_rnode);
}

/// Modifies the event enum to add the component events.
fn process_event_enum(
    item_enum: &mut RewriteNode,
    db: &dyn SyntaxGroup,
    components_info: &[ComponentInfo],
) {
    let item_enum_mnode = item_enum.modify(db);
    let item_enum_children = item_enum_mnode.children.as_mut().unwrap();
    let components_rnode = ComponentsGenerationData(components_info).generate_for_event_enum(db);

    // Insert the components at the beginning of the enum body.
    item_enum_children.insert(ast::ItemEnum::INDEX_LBRACE + 1, components_rnode);
}

fn add_event_enum(
    body_rnode: &mut RewriteNode,
    db: &dyn SyntaxGroup,
    components_info: &[ComponentInfo],
) {
    let body_mnode = body_rnode.modify(db);
    let event_enum_rnode = ComponentsGenerationData(components_info).generate_event_enum(db);

    // It is safe to unwrap here because we know that the node has at least the storage struct children
    body_mnode
        .children
        .as_mut()
        .unwrap()
        .insert(ast::ModuleBody::INDEX_RBRACE, event_enum_rnode);
}

/// Modifies the body node to add the use clauses and the `component!` macros to the module.
fn add_use_clauses_and_macros(
    body_rnode: &mut RewriteNode,
    db: &dyn SyntaxGroup,
    components_info: &[ComponentInfo],
) {
    let body_mnode = body_rnode.modify(db);
    let components_rnode = ComponentsGenerationData(components_info).generate_for_module(db);

    // It is safe to unwrap here because we know that the node has at least the storage struct children
    body_mnode
        .children
        .as_mut()
        .unwrap()
        .insert(ast::ModuleBody::INDEX_RBRACE, components_rnode);
}

/// Set of component information to be used for code generation.
struct ComponentsGenerationData<'a>(&'a [ComponentInfo<'a>]);

impl ComponentsGenerationData<'_> {
    fn generate_for_module(self, _db: &dyn SyntaxGroup) -> RewriteNode {
        RewriteNode::interpolate_patched(
            indoc! {"

        $component_use_clause_entries$

        $component_macro_entries$

        $component_internal_impls_entries$
        "},
            &[
                (
                    "component_use_clause_entries".to_string(),
                    self.component_use_clause_entries(),
                ),
                (
                    "component_macro_entries".to_string(),
                    self.component_macro_entries(),
                ),
                (
                    "component_internal_impls_entries".to_string(),
                    self.component_internal_impls_entries(),
                ),
            ]
            .into(),
        )
    }

    fn generate_for_storage_struct(self, _db: &dyn SyntaxGroup) -> RewriteNode {
        let mut entries = vec![];
        for component in self.0.iter() {
            entries.push(format!("{}#[{}]", tabs(2), SUBSTORAGE_ATTRIBUTE));
            entries.push(format!(
                "{}pub {}: {}::Storage,",
                tabs(2),
                component.storage,
                component.name
            ));
        }
        RewriteNode::Text(entries.join("\n") + "\n")
    }

    fn generate_for_event_enum(self, _db: &dyn SyntaxGroup) -> RewriteNode {
        let mut entries = vec![];
        for component in self.0.iter() {
            entries.push(format!("{}#[{}]", tabs(2), FLAT_ATTRIBUTE));
            entries.push(format!(
                "{}{}: {}::Event,",
                tabs(2),
                component.event,
                component.name
            ));
        }
        RewriteNode::Text(entries.join("\n") + "\n")
    }

    fn generate_event_enum(self, _db: &dyn SyntaxGroup) -> RewriteNode {
        let mut entries = vec![];

        entries.push(format!("\n{}#[event]", tabs(1)));
        entries.push(format!("{}#[derive(Drop, starknet::Event)]", tabs(1)));
        entries.push(format!("{}enum {} {{", tabs(1), EVENT_ENUM_NAME));
        for component in self.0.iter() {
            entries.push(format!("{}#[{}]", tabs(2), FLAT_ATTRIBUTE));
            entries.push(format!(
                "{}{}: {}::Event,",
                tabs(2),
                component.event,
                component.name
            ));
        }
        entries.push(format!("{}}}", tabs(1)));
        RewriteNode::Text(entries.join("\n"))
    }

    fn component_use_clause_entries(&self) -> RewriteNode {
        let mut entries = vec![];
        for component in self.0.iter() {
            entries.push(format!("{}use {};", tabs(1), component.path));
        }
        RewriteNode::Text(entries.join("\n"))
    }

    fn component_macro_entries(&self) -> RewriteNode {
        let mut entries = vec![];
        for component in self.0.iter() {
            entries.push(format!(
                "{}component!(path: {}, storage: {}, event: {});",
                tabs(1),
                component.name,
                component.storage,
                component.event
            ));
        }
        RewriteNode::Text(entries.join("\n"))
    }

    fn component_internal_impls_entries(&self) -> RewriteNode {
        let mut entries = vec![];
        for component in self.0.iter() {
            for implementation in component.internal_impls.iter() {
                entries.push(format!(
                    "{}impl {}{} = {}::{}<ContractState>;",
                    tabs(1),
                    component.short_name(),
                    implementation,
                    component.name,
                    implementation
                ));
            }
        }
        RewriteNode::Text(entries.join("\n"))
    }
}
