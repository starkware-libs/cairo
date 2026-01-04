## Governance

> **NOTE:** This document is better viewed at [https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance)

This crate includes primitives for on-chain governance.

### Interfaces

- [`IGovernor`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#IGovernor)
- [`IMultisig`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#IMultisig)
- [`ITimelock`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#ITimelock)
- [`IVotes`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#IVotes)

### Components

- [`GovernorComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorComponent)

- [`TimelockControllerComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#TimelockControllerComponent)
- [`VotesComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#VotesComponent)

#### Governor Extensions

- [`GovernorCoreExecutionComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorCoreExecutionComponent)
- [`GovernorCountingSimpleComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorCountingSimpleComponent)
- [`GovernorSettingsComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorSettingsComponent)
- [`GovernorVotesComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorVotesComponent)
- [`GovernorVotesQuorumFractionComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorVotesQuorumFractionComponent)
- [`GovernorTimelockExecutionComponent`](https://docs.openzeppelin.com/contracts-cairo/2.0.0/api/governance#GovernorTimelockExecutionComponent)
