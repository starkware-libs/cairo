from typing import Optional
import asyncio
import os
from starknet_py.net.account.account import Account
from starknet_py.net.models.address import Address
from starknet_py.net.client_models import Call
from starknet_py.contract import Contract
from starknet_py.hash.selector import get_selector_from_name
from pathlib import Path
from dataclasses import dataclass


ROLE_ADMIN = {
    "governance_admin": "governance_admin",
    "upgrade_governor": "governance_admin",
    "app_role_admin": "governance_admin",
    "app_governor": "app_role_admin",
    "operator": "app_role_admin",
    "token_admin": "app_role_admin",
    "security_admin": "security_admin",
    "security_agent": "security_admin",
}


def get_role_admin(role: str) -> str:
    return ROLE_ADMIN[role]


def get_contract_path(contract_name: str) -> Path:
    target_dev_path = Path(
        os.path.join(os.path.dirname(__file__), "target", "dev", f"{contract_name}.json")
    )
    if target_dev_path.exists():
        return target_dev_path

    pre_compiled_contract_path = get_compiled_contract_path(contract_name=contract_name)
    if pre_compiled_contract_path is not None:
        return pre_compiled_contract_path

    raise FileNotFoundError(
        f"Contract {contract_name}.json not found in:\n"
        f"  - target/dev/\n"
        f"  - compiled_contracts/"
    )


def get_compiled_contract_path(contract_name: str) -> Optional[Path]:
    compiled_contracts_path = Path(
        os.path.join(
            os.path.dirname(__file__),
            "compiled_contracts",
            f"{contract_name}.json",
        )
    )
    if compiled_contracts_path.exists():
        return compiled_contracts_path


def load_contract(contract_name: str) -> str:
    contract_path = get_contract_path(contract_name)
    return contract_path.read_text("utf-8")


async def grant_roles(contracts_by_role: dict[str, Contract], roles: dict[str, Account]):
    await asyncio.gather(
        *[
            (
                await contracts_by_role["governance_admin"]
                .functions[f"register_{role_name}"]
                .invoke_v3(roles[role_name].address, auto_estimate=True)
            ).wait_for_acceptance()
            for role_name in ["app_role_admin", "upgrade_governor", "security_admin"]
        ]
    )

    await asyncio.gather(
        *[
            (
                await contracts_by_role["app_role_admin"]
                .functions[f"register_{role_name}"]
                .invoke_v3(roles[role_name].address, auto_estimate=True)
            ).wait_for_acceptance()
            for role_name in ["app_governor", "operator", "token_admin"]
        ]
    )

    await (
        await contracts_by_role["security_admin"]
        .functions["register_security_agent"]
        .invoke_v3(roles["security_agent"].address, auto_estimate=True)
    ).wait_for_acceptance()


async def grant_roles_by_contract_address(contract_address: Address, roles: dict[str, Account]):
    # Verify all required roles are present
    admin_roles = ["app_role_admin", "upgrade_governor"]
    app_roles = ["app_governor", "operator", "token_admin"]
    security_roles = ["security_agent"]
    all_required_roles = (
        admin_roles + app_roles + security_roles + ["governance_admin", "security_admin"]
    )

    assert all(
        role in roles for role in all_required_roles
    ), f"Missing required roles: {set(all_required_roles) - set(roles.keys())}"

    # Verify that the admin roles are set by the governance admin.
    assert all(get_role_admin(role) == "governance_admin" for role in admin_roles)

    # Register governance roles. The client is the governance admin since it is the only one that
    # can register the admin roles.
    await asyncio.gather(
        *[
            roles[get_role_admin(role_name)].client.wait_for_tx(
                (
                    await roles[get_role_admin(role_name)].execute_v3(
                        calls=Call(
                            to_addr=contract_address,
                            selector=get_selector_from_name(f"register_{role_name}"),
                            calldata=[roles[role_name].address],
                        ),
                        auto_estimate=True,
                    )
                ).transaction_hash
            )
            for role_name in admin_roles
        ]
    )

    # Verify that the app roles are set by the app role admin.
    assert all(get_role_admin(role) == "app_role_admin" for role in app_roles)
    # Register app roles. The client is the app role admin since it is the only one that
    # can register the app roles.
    await asyncio.gather(
        *[
            roles[get_role_admin(role_name)].client.wait_for_tx(
                (
                    await roles[get_role_admin(role_name)].execute_v3(
                        calls=Call(
                            to_addr=contract_address,
                            selector=get_selector_from_name(f"register_{role_name}"),
                            calldata=[roles[role_name].address],
                        ),
                        auto_estimate=True,
                    )
                ).transaction_hash
            )
            for role_name in app_roles
        ]
    )

    # In the setup of the test, the security admin is the gov admin account. Therefore, need to
    # register the security admin account as the security admin.
    await roles["governance_admin"].client.wait_for_tx(
        (
            await roles["governance_admin"].execute_v3(
                calls=Call(
                    to_addr=contract_address,
                    selector=get_selector_from_name("register_security_admin"),
                    calldata=[roles["security_admin"].address],
                ),
                auto_estimate=True,
            )
        ).transaction_hash
    )

    # Verify that the security agent is set by the security admin.
    assert get_role_admin("security_agent") == "security_admin"
    # Register security agent. The client is the security admin since it is the only one that
    # can register the security agent.
    await roles[get_role_admin("security_agent")].client.wait_for_tx(
        (
            await roles[get_role_admin("security_agent")].execute_v3(
                calls=Call(
                    to_addr=contract_address,
                    selector=get_selector_from_name("register_security_agent"),
                    calldata=[roles["security_agent"].address],
                ),
                auto_estimate=True,
            )
        ).transaction_hash
    )


@dataclass
class AccountNonceManager:
    account_number: int
    nonce: int = 0

    def get_nonce(self) -> int:
        """
        Return the current nonce value.
        """
        return self.nonce

    def bump_nonce(self) -> int:
        """
        Increment the nonce by 1 and return it.
        """
        self.nonce += 1
        return self.nonce - 1
