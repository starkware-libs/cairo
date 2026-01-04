use crate::constants::{
    CONSTRUCTOR_ATTRIBUTE, CONTRACT_ATTRIBUTE, EVENT_ENUM_NAME, FLAT_ATTRIBUTE,
    STORAGE_STRUCT_NAME, SUBSTORAGE_ATTRIBUTE,
};
use crate::utils::tabs;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_formatter::format_string;
use cairo_lang_macro::{attribute_macro, Diagnostic, Diagnostics, ProcMacroResult, TokenStream};
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_syntax::node::ast::MaybeModuleBody;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{BodyItems, QueryAttrs};
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use indoc::{formatdoc, indoc};
use regex::Regex;

const ALLOWED_COMPONENTS: [&str; 22] = [
    "Account",
    "EthAccount",
    "SRC9",
    "AccessControl",
    "Ownable",
    "Vesting",
    "SRC5",
    "Initializable",
    "Pausable",
    "ReentrancyGuard",
    "ERC20",
    "ERC721",
    "ERC721Enumerable",
    "ERC721Receiver",
    "ERC1155",
    "ERC1155Receiver",
    "ERC2981",
    "Upgradeable",
    "Nonces",
    "Multisig",
    "TimelockController",
    "Votes",
    // "TODO:Governor",
    // "TODO:GovernorCoreExecution",
    // "TODO:GovernorCountingSimple",
    // "TODO:GovernorSettings",
    // "TODO:GovernorTimelockExecution",
    // "TODO:GovernorVotesQuorumFraction",
    // "TODO:GovernorVotes",
];

/// Inserts multiple component dependencies into a modules codebase.
#[attribute_macro]
pub fn with_components(attribute_stream: TokenStream, item_stream: TokenStream) -> ProcMacroResult {
    let args = parse_args(&attribute_stream.to_string());

    // 1. Get the components info (if valid)
    let mut components_info = vec![];
    for arg in args {
        let component_info_result = get_component_info(&arg);
        if let Ok(info) = component_info_result {
            components_info.push(info);
        } else {
            return ProcMacroResult::new(TokenStream::empty())
                .with_diagnostics(component_info_result.unwrap_err());
        }
    }

    // 2. Parse the item stream
    let db = SimpleParserDatabase::default();
    let (content, mut diagnostics) = match db.parse_virtual(item_stream.to_string()) {
        Ok(node) => build_patch(&db, node, components_info.clone()),
        Err(err) => {
            let error = Diagnostic::error(err.format(&db));
            return ProcMacroResult::new(TokenStream::empty()).with_diagnostics(error.into());
        }
    };

    // 4. Add warnings for each component
    for component_info in components_info.iter() {
        let component_warnings = add_per_component_warnings(&content, component_info);
        diagnostics.extend(component_warnings);
    }

    let formatted_content = if !content.is_empty() {
        format_string(&db, content)
    } else {
        content
    };

    ProcMacroResult::new(TokenStream::new(formatted_content)).with_diagnostics(diagnostics)
}

/// Parses the arguments from the attribute stream.
fn parse_args(text: &str) -> Vec<String> {
    let re = Regex::new(r"(\w+)").unwrap();
    let matches = re.find_iter(text);
    matches.map(|m| m.as_str().to_string()).collect()
}

/// Builds the patch for a given node and component info.
fn build_patch(
    db: &dyn SyntaxGroup,
    node: SyntaxNode,
    components_info: Vec<ComponentInfo>,
) -> (String, Diagnostics) {
    let mut builder = PatchBuilder::new_ex(db, &node);

    let typed = ast::SyntaxFile::from_syntax_node(db, node);
    let mut base_rnode = RewriteNode::from_ast(&typed);
    let module_rnode = base_rnode
        .modify_child(db, ast::SyntaxFile::INDEX_ITEMS)
        .modify_child(db, 0);

    // Validate the contract module
    let (errors, warnings) = validate_contract_module(db, module_rnode, &components_info);
    if !errors.is_empty() {
        return (String::new(), errors.into());
    }

    let body_rnode = module_rnode.modify_child(db, ast::ItemModule::INDEX_BODY);

    process_module_items(body_rnode, db, &components_info);
    add_use_clauses_and_macros(body_rnode, db, &components_info);

    builder.add_modified(base_rnode);
    let (content, _) = builder.build();

    (content, warnings.into())
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
            let error = Diagnostic::error(indoc! {"
                Contract module must have a body.
            "});
            return (vec![error], vec![]);
        };

        // 2. Check that the module has the `#[starknet::contract]` attribute (error)
        if !item.has_attr(db, CONTRACT_ATTRIBUTE) {
            let error = Diagnostic::error(formatdoc! {"
                Contract module must have the `#[{CONTRACT_ATTRIBUTE}]` attribute.
            "});
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
                let warning = Diagnostic::warn(formatdoc! {"
                    It looks like the initializers for the following components are missing:

                    {components_with_initializer_missing_str}

                    This may lead to unexpected behavior. We recommend adding the corresponding initializer calls to the constructor.
                "});
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
            let default_config_path = format!("{}DefaultConfig", component_parent_path);
            let default_config_used = code.contains(&default_config_path);
            if !default_config_used {
                // Check if the ImmutableConfig is implemented
                let immutable_config_implemented =
                    code.contains(&format!("of {}::ImmutableConfig", component.name));
                if !immutable_config_implemented {
                    let warning = Diagnostic::warn(formatdoc! {"
                    The {} component requires an ImmutableConfig implementation in scope. It looks like this implementation is missing.

                    You can use the default implementation by importing:

                    `use {};`
                ", component.short_name(), default_config_path});
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

    match component_info.short_name() {
        "Vesting" => {
            // 1. Check that the VestingScheduleTrait is implemented
            let linear_impl_used = code.contains("LinearVestingSchedule");
            let vesting_trait_used = code.contains("VestingScheduleTrait");
            if !linear_impl_used && !vesting_trait_used {
                let warning = Diagnostic::warn(indoc! {"
                    The Vesting component requires an implementation of the VestingScheduleTrait in scope. It looks like this implementation is missing.

                    You can use the LinearVestingSchedule implementation by importing it:

                    `use openzeppelin_finance::vesting::LinearVestingSchedule;`
                "});
                warnings.push(warning);
            }
        }
        "Initializable" => {
            // 1. Check that the initialize internal function is called
            let initialize_internal_function_called =
                code.contains("self.initializable.initialize()");
            if !initialize_internal_function_called {
                let warning = Diagnostic::warn(indoc! {"
                    It looks like the `self.initializable.initialize()` function is not used in the contract. If
                    this is intentional, you may consider removing the Initializable component.
                "});
                warnings.push(warning);
            }
        }
        "Pausable" => {
            // 1. Check that the pause and unpause functions are called
            let pause_function_called = code.contains("self.pausable.pause()");
            let unpause_function_called = code.contains("self.pausable.unpause()");
            if !pause_function_called || !unpause_function_called {
                let warning = Diagnostic::warn(indoc! {"
                    It looks like either the `self.pausable.pause()` or `self.pausable.unpause()` mechanisms are not implemented in the contract. If
                    this is intentional, you may consider removing the Pausable component.
                "});
                warnings.push(warning);
            }
        }
        "ERC20" => {
            // 1. Check that the ERC20HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC20HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC20HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(indoc! {"
                    The ERC20 component requires an implementation of the ERC20HooksTrait in scope. It looks like this implementation is missing.

                    You can use the ERC20HooksEmptyImpl implementation by importing it:

                    `use openzeppelin_token::erc20::ERC20HooksEmptyImpl;`
                "});
                warnings.push(warning);
            }
        }
        "ERC721" => {
            // 1. Check that the ERC721HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC721HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC721HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(indoc! {"
                    The ERC721 component requires an implementation of the ERC721HooksTrait in scope. It looks like this implementation is missing.

                    You can use the ERC721HooksEmptyImpl implementation by importing it:

                    `use openzeppelin_token::erc721::ERC721HooksEmptyImpl;`
                "});
                warnings.push(warning);
            }
        }
        "ERC1155" => {
            // 1. Check that the ERC1155HooksTrait is implemented
            let hooks_trait_used = code.contains("ERC1155HooksTrait");
            let hooks_empty_impl_used = code.contains("ERC1155HooksEmptyImpl");
            if !hooks_trait_used && !hooks_empty_impl_used {
                let warning = Diagnostic::warn(indoc! {"
                    The ERC1155 component requires an implementation of the ERC1155HooksTrait in scope. It looks like this implementation is missing.

                    You can use the ERC1155HooksEmptyImpl implementation by importing it:

                    `use openzeppelin_token::erc1155::ERC1155HooksEmptyImpl;`
                "});
                warnings.push(warning);
            }
        }
        "Upgradeable" => {
            // 1. Check that the upgrade function is called
            let upgrade_function_called = code.contains("self.upgradeable.upgrade");
            if !upgrade_function_called {
                let warning = Diagnostic::warn(indoc! {"
                    It looks like the `self.upgradeable.upgrade(new_class_hash)` function is not used in the contract. If
                    this is intentional, you may consider removing the Upgradeable component.
                "});
                warnings.push(warning);
            }
        }
        "Votes" => {
            // 1. Check that the SNIP12Metadata is implemented
            let snip12_metadata_implemented = code.contains("of SNIP12Metadata");
            if !snip12_metadata_implemented {
                let warning = Diagnostic::warn(indoc! {"
                    The Votes component requires an implementation of the SNIP12Metadata trait. It looks like this implementation is missing.
                "});
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
    components_info: &Vec<ComponentInfo>,
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
    components_info: &Vec<ComponentInfo>,
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
    components_info: &Vec<ComponentInfo>,
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
    components_info: &Vec<ComponentInfo>,
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
    components_info: &Vec<ComponentInfo>,
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

/// Information about a component.
///
/// # Members
///
/// * `name` - The name of the component (e.g. `ERC20Component`)
/// * `path` - The path from where the component is imported (e.g. `openzeppelin_token::erc20::ERC20Component`)
/// * `storage` - The path to reference the component in storage (e.g. `erc20`)
/// * `event` - The path to reference the component events (e.g. `ERC20Event`)
/// * `has_initializer` - Whether the component requires an initializer (e.g. `true`)
/// * `internal_impls` - The internal implementations of the component to be added to
///   the module by default (e.g. `["InternalImpl1", "InternalImpl2"]`)
#[derive(Debug, Clone)]
pub struct ComponentInfo<'a> {
    pub name: &'a str,
    pub path: &'a str,
    pub storage: &'a str,
    pub event: &'a str,
    pub has_initializer: bool,
    pub has_immutable_config: bool,
    pub internal_impls: Vec<&'a str>,
}

impl<'a> ComponentInfo<'a> {
    fn short_name(&self) -> &'a str {
        self.name
            .split("Component")
            .next()
            .expect("Component name must end with 'Component'")
    }
}

/// Returns the component info for a given component name.
///
/// # Arguments
///
/// * `name` - The name of the component (e.g. `ERC20`).
fn get_component_info<'a>(name: &str) -> Result<ComponentInfo<'a>, Diagnostics> {
    match name {
        "Account" => Ok(ComponentInfo {
            name: "AccountComponent",
            path: "openzeppelin_account::AccountComponent",
            storage: "account",
            event: "AccountEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "EthAccount" => Ok(ComponentInfo {
            name: "EthAccountComponent",
            path: "openzeppelin_account::EthAccountComponent",
            storage: "eth_account",
            event: "EthAccountEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "SRC9" => Ok(ComponentInfo {
            name: "SRC9Component",
            path: "openzeppelin_account::extensions::SRC9Component",
            storage: "src9",
            event: "SRC9Event",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Ownable" => Ok(ComponentInfo {
            name: "OwnableComponent",
            path: "openzeppelin_access::ownable::OwnableComponent",
            storage: "ownable",
            event: "OwnableEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "AccessControl" => Ok(ComponentInfo {
            name: "AccessControlComponent",
            path: "openzeppelin_access::accesscontrol::AccessControlComponent",
            storage: "access_control",
            event: "AccessControlEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Vesting" => Ok(ComponentInfo {
            name: "VestingComponent",
            path: "openzeppelin_finance::vesting::VestingComponent",
            storage: "vesting",
            event: "VestingEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "SRC5" => Ok(ComponentInfo {
            name: "SRC5Component",
            path: "openzeppelin_introspection::src5::SRC5Component",
            storage: "src5",
            event: "SRC5Event",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Initializable" => Ok(ComponentInfo {
            name: "InitializableComponent",
            path: "openzeppelin_security::InitializableComponent",
            storage: "initializable",
            event: "InitializableEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Pausable" => Ok(ComponentInfo {
            name: "PausableComponent",
            path: "openzeppelin_security::PausableComponent",
            storage: "pausable",
            event: "PausableEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ReentrancyGuard" => Ok(ComponentInfo {
            name: "ReentrancyGuardComponent",
            path: "openzeppelin_security::ReentrancyGuardComponent",
            storage: "reentrancy_guard",
            event: "ReentrancyGuardEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC20" => Ok(ComponentInfo {
            name: "ERC20Component",
            path: "openzeppelin_token::erc20::ERC20Component",
            storage: "erc20",
            event: "ERC20Event",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC721" => Ok(ComponentInfo {
            name: "ERC721Component",
            path: "openzeppelin_token::erc721::ERC721Component",
            storage: "erc721",
            event: "ERC721Event",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC721Enumerable" => Ok(ComponentInfo {
            name: "ERC721EnumerableComponent",
            path: "openzeppelin_token::erc721::extensions::ERC721EnumerableComponent",
            storage: "erc721_enumerable",
            event: "ERC721EnumerableEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC721Receiver" => Ok(ComponentInfo {
            name: "ERC721ReceiverComponent",
            path: "openzeppelin_token::erc721::ERC721ReceiverComponent",
            storage: "erc721_receiver",
            event: "ERC721ReceiverEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC1155" => Ok(ComponentInfo {
            name: "ERC1155Component",
            path: "openzeppelin_token::erc1155::ERC1155Component",
            storage: "erc1155",
            event: "ERC1155Event",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC1155Receiver" => Ok(ComponentInfo {
            name: "ERC1155ReceiverComponent",
            path: "openzeppelin_token::erc1155::ERC1155ReceiverComponent",
            storage: "erc1155_receiver",
            event: "ERC1155ReceiverEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "ERC2981" => Ok(ComponentInfo {
            name: "ERC2981Component",
            path: "openzeppelin_token::common::erc2981::ERC2981Component",
            storage: "erc2981",
            event: "ERC2981Event",
            has_initializer: true,
            has_immutable_config: true,
            internal_impls: vec!["InternalImpl"],
        }),
        "Upgradeable" => Ok(ComponentInfo {
            name: "UpgradeableComponent",
            path: "openzeppelin_upgrades::UpgradeableComponent",
            storage: "upgradeable",
            event: "UpgradeableEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Nonces" => Ok(ComponentInfo {
            name: "NoncesComponent",
            path: "openzeppelin_utils::nonces::NoncesComponent",
            storage: "nonces",
            event: "NoncesEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Multisig" => Ok(ComponentInfo {
            name: "MultisigComponent",
            path: "openzeppelin_governance::multisig::MultisigComponent",
            storage: "multisig",
            event: "MultisigEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "TimelockController" => Ok(ComponentInfo {
            name: "TimelockControllerComponent",
            path: "openzeppelin_governance::timelock::TimelockControllerComponent",
            storage: "timelock_controller",
            event: "TimelockControllerEvent",
            has_initializer: true,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        "Votes" => Ok(ComponentInfo {
            name: "VotesComponent",
            path: "openzeppelin_governance::votes::VotesComponent",
            storage: "votes",
            event: "VotesEvent",
            has_initializer: false,
            has_immutable_config: false,
            internal_impls: vec!["InternalImpl"],
        }),
        _ => {
            let allowed_components = ALLOWED_COMPONENTS.join(", ");
            let error_message = formatdoc! {"
                Invalid component: {name}

                Allowed components are:
                {allowed_components}
            "};
            let error = Diagnostic::error(error_message);
            Err(error.into())
        }
    }
}

/// Set of component information to be used for code generation.
struct ComponentsGenerationData<'a>(&'a Vec<ComponentInfo<'a>>);

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

#[cfg(test)]
mod tests {
    use super::parse_args;

    #[test]
    fn test_parse_args() {
        let attribute = "(ERC20, Ownable)";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["ERC20", "Ownable"]);

        let attribute = "ERC20";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["ERC20"]);

        let attribute = "(Ownable, ERC20, Other, Another)";
        let result = parse_args(attribute);
        assert_eq!(result, vec!["Ownable", "ERC20", "Other", "Another"]);
    }
}
