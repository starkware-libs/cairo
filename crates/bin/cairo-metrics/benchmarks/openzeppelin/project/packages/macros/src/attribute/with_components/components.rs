use super::diagnostics::errors;
use cairo_lang_macro::Diagnostic;

/// The list of allowed components for the `with_components` attribute.
pub enum AllowedComponents {
    Account,
    EthAccount,
    SRC9,
    AccessControl,
    Ownable,
    Vesting,
    SRC5,
    Initializable,
    Pausable,
    ReentrancyGuard,
    ERC20,
    ERC721,
    ERC721Enumerable,
    ERC721Receiver,
    ERC1155,
    ERC1155Receiver,
    ERC2981,
    ERC4626,
    Upgradeable,
    Nonces,
    Multisig,
    TimelockController,
    Votes,
    Governor,
    GovernorCoreExecution,
    GovernorCountingSimple,
    GovernorSettings,
    GovernorTimelockExecution,
    GovernorVotesQuorumFraction,
    GovernorVotes,
}

impl AllowedComponents {
    pub fn from_str(s: &str) -> Result<Self, Diagnostic> {
        match s {
            "Account" => Ok(AllowedComponents::Account),
            "EthAccount" => Ok(AllowedComponents::EthAccount),
            "SRC9" => Ok(AllowedComponents::SRC9),
            "AccessControl" => Ok(AllowedComponents::AccessControl),
            "Ownable" => Ok(AllowedComponents::Ownable),
            "Vesting" => Ok(AllowedComponents::Vesting),
            "SRC5" => Ok(AllowedComponents::SRC5),
            "Initializable" => Ok(AllowedComponents::Initializable),
            "Pausable" => Ok(AllowedComponents::Pausable),
            "ReentrancyGuard" => Ok(AllowedComponents::ReentrancyGuard),
            "ERC20" => Ok(AllowedComponents::ERC20),
            "ERC721" => Ok(AllowedComponents::ERC721),
            "ERC721Enumerable" => Ok(AllowedComponents::ERC721Enumerable),
            "ERC721Receiver" => Ok(AllowedComponents::ERC721Receiver),
            "ERC1155" => Ok(AllowedComponents::ERC1155),
            "ERC1155Receiver" => Ok(AllowedComponents::ERC1155Receiver),
            "ERC2981" => Ok(AllowedComponents::ERC2981),
            "ERC4626" => Ok(AllowedComponents::ERC4626),
            "Upgradeable" => Ok(AllowedComponents::Upgradeable),
            "Nonces" => Ok(AllowedComponents::Nonces),
            "Multisig" => Ok(AllowedComponents::Multisig),
            "TimelockController" => Ok(AllowedComponents::TimelockController),
            "Votes" => Ok(AllowedComponents::Votes),
            "Governor" => Ok(AllowedComponents::Governor),
            "GovernorCoreExecution" => Ok(AllowedComponents::GovernorCoreExecution),
            "GovernorCountingSimple" => Ok(AllowedComponents::GovernorCountingSimple),
            "GovernorSettings" => Ok(AllowedComponents::GovernorSettings),
            "GovernorTimelockExecution" => Ok(AllowedComponents::GovernorTimelockExecution),
            "GovernorVotesQuorumFraction" => Ok(AllowedComponents::GovernorVotesQuorumFraction),
            "GovernorVotes" => Ok(AllowedComponents::GovernorVotes),
            _ => Err(Diagnostic::error(errors::INVALID_COMPONENT(s))),
        }
    }

    pub fn get_info(&self) -> ComponentInfo<'static> {
        match self {
            AllowedComponents::Account => ComponentInfo {
                name: "AccountComponent",
                path: "openzeppelin_account::AccountComponent",
                storage: "account",
                event: "AccountEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::EthAccount => ComponentInfo {
                name: "EthAccountComponent",
                path: "openzeppelin_account::EthAccountComponent",
                storage: "eth_account",
                event: "EthAccountEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::SRC9 => ComponentInfo {
                name: "SRC9Component",
                path: "openzeppelin_account::extensions::SRC9Component",
                storage: "src9",
                event: "SRC9Event",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Ownable => ComponentInfo {
                name: "OwnableComponent",
                path: "openzeppelin_access::ownable::OwnableComponent",
                storage: "ownable",
                event: "OwnableEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::AccessControl => ComponentInfo {
                name: "AccessControlComponent",
                path: "openzeppelin_access::accesscontrol::AccessControlComponent",
                storage: "access_control",
                event: "AccessControlEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Vesting => ComponentInfo {
                name: "VestingComponent",
                path: "openzeppelin_finance::vesting::VestingComponent",
                storage: "vesting",
                event: "VestingEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::SRC5 => ComponentInfo {
                name: "SRC5Component",
                path: "openzeppelin_introspection::src5::SRC5Component",
                storage: "src5",
                event: "SRC5Event",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Initializable => ComponentInfo {
                name: "InitializableComponent",
                path: "openzeppelin_security::InitializableComponent",
                storage: "initializable",
                event: "InitializableEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Pausable => ComponentInfo {
                name: "PausableComponent",
                path: "openzeppelin_security::PausableComponent",
                storage: "pausable",
                event: "PausableEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ReentrancyGuard => ComponentInfo {
                name: "ReentrancyGuardComponent",
                path: "openzeppelin_security::ReentrancyGuardComponent",
                storage: "reentrancy_guard",
                event: "ReentrancyGuardEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC20 => ComponentInfo {
                name: "ERC20Component",
                path: "openzeppelin_token::erc20::ERC20Component",
                storage: "erc20",
                event: "ERC20Event",
                has_initializer: true,
                has_immutable_config: true,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC721 => ComponentInfo {
                name: "ERC721Component",
                path: "openzeppelin_token::erc721::ERC721Component",
                storage: "erc721",
                event: "ERC721Event",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC721Enumerable => ComponentInfo {
                name: "ERC721EnumerableComponent",
                path: "openzeppelin_token::erc721::extensions::ERC721EnumerableComponent",
                storage: "erc721_enumerable",
                event: "ERC721EnumerableEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC721Receiver => ComponentInfo {
                name: "ERC721ReceiverComponent",
                path: "openzeppelin_token::erc721::ERC721ReceiverComponent",
                storage: "erc721_receiver",
                event: "ERC721ReceiverEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC1155 => ComponentInfo {
                name: "ERC1155Component",
                path: "openzeppelin_token::erc1155::ERC1155Component",
                storage: "erc1155",
                event: "ERC1155Event",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC1155Receiver => ComponentInfo {
                name: "ERC1155ReceiverComponent",
                path: "openzeppelin_token::erc1155::ERC1155ReceiverComponent",
                storage: "erc1155_receiver",
                event: "ERC1155ReceiverEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC2981 => ComponentInfo {
                name: "ERC2981Component",
                path: "openzeppelin_token::common::erc2981::ERC2981Component",
                storage: "erc2981",
                event: "ERC2981Event",
                has_initializer: true,
                has_immutable_config: true,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::ERC4626 => ComponentInfo {
                name: "ERC4626Component",
                path: "openzeppelin_token::erc20::extensions::erc4626::ERC4626Component",
                storage: "erc4626",
                event: "ERC4626Event",
                has_initializer: true,
                has_immutable_config: true,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Upgradeable => ComponentInfo {
                name: "UpgradeableComponent",
                path: "openzeppelin_upgrades::UpgradeableComponent",
                storage: "upgradeable",
                event: "UpgradeableEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Nonces => ComponentInfo {
                name: "NoncesComponent",
                path: "openzeppelin_utils::nonces::NoncesComponent",
                storage: "nonces",
                event: "NoncesEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Multisig => ComponentInfo {
                name: "MultisigComponent",
                path: "openzeppelin_governance::multisig::MultisigComponent",
                storage: "multisig",
                event: "MultisigEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::TimelockController => ComponentInfo {
                name: "TimelockControllerComponent",
                path: "openzeppelin_governance::timelock::TimelockControllerComponent",
                storage: "timelock_controller",
                event: "TimelockControllerEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Votes => ComponentInfo {
                name: "VotesComponent",
                path: "openzeppelin_governance::votes::VotesComponent",
                storage: "votes",
                event: "VotesEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::Governor => ComponentInfo {
                name: "GovernorComponent",
                path: "openzeppelin_governance::governor::GovernorComponent",
                storage: "governor",
                event: "GovernorEvent",
                has_initializer: true,
                has_immutable_config: true,
                internal_impls: vec!["InternalImpl"],
            },
            AllowedComponents::GovernorCoreExecution => ComponentInfo {
                name: "GovernorCoreExecutionComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorCoreExecutionComponent",
                storage: "governor_core_execution",
                event: "GovernorCoreExecutionEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["GovernorExecution"],
            },
            AllowedComponents::GovernorCountingSimple => ComponentInfo {
                name: "GovernorCountingSimpleComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorCountingSimpleComponent",
                storage: "governor_counting_simple",
                event: "GovernorCountingSimpleEvent",
                has_initializer: false,
                has_immutable_config: false,
                internal_impls: vec!["GovernorCounting"],
            },
            AllowedComponents::GovernorSettings => ComponentInfo {
                name: "GovernorSettingsComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorSettingsComponent",
                storage: "governor_settings",
                event: "GovernorSettingsEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl", "GovernorSettings"],
            },
            AllowedComponents::GovernorTimelockExecution => ComponentInfo {
                name: "GovernorTimelockExecutionComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorTimelockExecutionComponent",
                storage: "governor_timelock_execution",
                event: "GovernorTimelockExecutionEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl", "GovernorExecution"],
            },
            AllowedComponents::GovernorVotesQuorumFraction => ComponentInfo {
                name: "GovernorVotesQuorumFractionComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorVotesQuorumFractionComponent",
                storage: "governor_votes_quorum_fraction",
                event: "GovernorVotesQuorumFractionEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl", "GovernorQuorum", "GovernorVotes"],
            },
            AllowedComponents::GovernorVotes => ComponentInfo {
                name: "GovernorVotesComponent",
                path: "openzeppelin_governance::governor::extensions::GovernorVotesComponent",
                storage: "governor_votes",
                event: "GovernorVotesEvent",
                has_initializer: true,
                has_immutable_config: false,
                internal_impls: vec!["InternalImpl", "GovernorVotes"],
            },
        }
    }
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
    pub fn short_name(&self) -> &'a str {
        self.name
            .split("Component")
            .next()
            .expect("Component name must end with 'Component'")
    }

    pub fn kind(&self) -> AllowedComponents {
        // It should be safe to unwrap here since the ComponentInfo must have
        // been created from a valid AllowedComponent
        AllowedComponents::from_str(self.short_name()).unwrap()
    }
}
