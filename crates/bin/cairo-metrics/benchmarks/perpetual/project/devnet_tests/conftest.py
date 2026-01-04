import pytest
from typing import Iterator, Callable, Union
import pytest_asyncio
import contextlib
import time
import requests
from test_utils.starknet_test_utils import StarknetTestUtils
from starknet_py.net.models.chains import StarknetChainId
from test_utils.starknet_test_utils import KeyPair
from starknet_py.net.account.account import Account
from starknet_py.net.models.address import Address
from starknet_py.proxy.contract_abi_resolver import ContractAbiResolver, ProxyConfig
from starknet_py.net.client_models import ResourceBoundsMapping, ResourceBounds
from starknet_py.contract import Contract

resource_bounds = ResourceBoundsMapping(
    l1_gas=ResourceBounds(max_amount=10**15, max_price_per_unit=10**12),
    l1_data_gas=ResourceBounds(max_amount=10**15, max_price_per_unit=10**12),
    l2_gas=ResourceBounds(max_amount=10**15, max_price_per_unit=10**12),
)

# Random block number for the forked network
# When changing the forked number, need to update NOW_TIMESTAMP
# and EXTENDED_ACTIVE_SYNTHETIC_ASSET_IDS
FORK_BLOCK = 4415803
NOW_TIMESTAMP = 1765971224

# Required for funding tick when forking from mainnet.
# This list must be sorted in ascending order.
EXTENDED_ACTIVE_SYNTHETIC_ASSET_IDS = [
    255399919616426605400900257294843904,
    255399919633304379671818952480129024,
    255399919636945194886730150859243520,
    270915948027776046540867925536931840,
    338824487460085561411166284290195456,
    338883663474438343746899177019277312,
    338905303280690764515996647076921344,
    339127382605583335846376969292742656,
    339128557725978860634015450544472064,
    339167696437051981397172032081231872,
    339189412421329087967425821086318592,
    339248760150559911178053148269871104,
    339249788878727834962578719796887552,
    344097595806435454645696181259206656,
    344278863660798979802850626929426432,
    344400637349001255728961162330505216,
    349208209667358115027548008674754560,
    349553874717371921940984895618678784,
    354684143347689286618180522700963840,
    359653178029624005735478030701166592,
    359754745788483493062461101120159744,
    359855675003405245145646005674311680,
    359977924063000458297011360688504832,
    359998998794517018713123529349398528,
    364785659509114736355216580319117312,
    370260563196593831237922481296637952,
    370321410161845694364216165796937728,
    375656867931341909315028931361898496,
    380625508329364671305743144700608512,
    380663843873413173657742089127985152,
    385960324586951584765944650413375488,
    390746430762672347138718348219514880,
    396000038112596647361460946256003072,
    396101378379648832210803477251620864,
    396101380212403101135280116273774592,
    396323605930722754555422238098587648,
    401211989740530901651818111897174016,
    401212385921352564110845035577081856,
    401334549526471356934815741500194816,
    401395555207980563015918882817835008,
    401415362247400203280702639630712832,
    401415448619475043208479622807158784,
    406403816491335810657189215283970048,
    411778891792336016648512811332272128,
    411817625024622139428925067103305728,
    416789435879299995223824283975286784,
    416789436818522072078212394507042816,
    416992418108949182116875998607704064,
    417113878881044044170394349040828416,
    427174429141597609995520190256775168,
    431877150642355703025313311742754816,
    432365923161760081025958177373421568,
    432549653271839585844452234968956928,
    432568984945910917982054318042251264,
    432590218091046889185300129471528960,
    432590218096110157059814614722674688,
    432670881640427675234041645227835392,
    432690441716627433572355962568704000,
    437477565754482165017662333485318144,
    437557744680566327621078168874516480,
    437638715834618327041098343530889216,
    437761440258352922500030743109959680,
    437822852025511902434950189083000832,
    437823079767580094335063891128614912,
    442933058567680452956063953642848256,
    448024668544195796360802891422760960,
    453216002551035381248377800238301184,
    453276691323521307730974454904258560,
    453276858440817581570030792557461504,
    458247228599093253039736795210711040,
    458267273324209361917147961830146048,
    458550751644561930336286859415519232,
    458591633377628216554099757268598784,
    468711525804946640478875348763672576,
    468915544507303112068184696028135424,
    468976147866535357546823156383088640,
]

PERPETUALS_CONTRACT_ADDRESS = 0x062DA0780FAE50D68CECAA5A051606DC21217BA290969B302DB4DD99D2E9B470
OPERATOR_ADDRESS = 0x048DDC53F41523D2A6B40C3DFF7F69F4BBAC799CD8B2E3FC50D3DE1D4119441F
UPGRADE_GOVERNOR_ADDRESS = 0x0562BBB386BB3EF6FFB94878EC77C0779487F277DEA89568F1CD7CDF958EDDE7
APP_GOVERNOR_ADDRESS = 0x003CCFFE0137EA21294C1CC28F6C29DD495F5B9F1101EC86AE53EF51178AEFA2
# Extended governance admin is also app role admin
GOVERNANCE_ADMIN_ADDRESS = 0x0522E5BA327BFBD85138B29BDE060A5340A460706B00AE2E10E6D2A16FBF8C57
RICH_USDC_HOLDER_ADDRESS = 0x054A6DF48915BE451CD6650C3697C5789B934EB2A89D90CBB71E3234F24F0311


@pytest.fixture
def accounts_to_impersonate() -> dict[str, tuple[int, int]]:
    # dict[account_name] = (address, dummy_key)
    return {
        "operator": (OPERATOR_ADDRESS, 1),
        "upgrade_governor": (UPGRADE_GOVERNOR_ADDRESS, 2),
        "app_governor": (APP_GOVERNOR_ADDRESS, 3),
        "governance_admin": (GOVERNANCE_ADMIN_ADDRESS, 4),
        "rich_usdc_holder": (RICH_USDC_HOLDER_ADDRESS, 5),
    }


def wait_for_devnet(port: int, timeout: int = 60) -> bool:
    """
    Poll the devnet endpoint until it's ready or timeout.
    Returns True if devnet is ready, False if timeout.
    """
    start_time = time.time()
    while time.time() - start_time < timeout:
        try:
            response = requests.post(
                f"http://localhost:{port}",
                json={"jsonrpc": "2.0", "method": "starknet_chainId", "params": [], "id": 1},
                timeout=2,
            )
            if response.status_code == 200:
                return True
        except (requests.ConnectionError, requests.Timeout):
            pass
        time.sleep(1)
    return False


@pytest.fixture
def starknet_test_utils_factory():
    """
    Factory for creating StarknetTestUtils instances.
    Overrides the fixture from test_utils.fixtures.
    """

    @contextlib.contextmanager
    def _factory(**kwargs):
        with StarknetTestUtils.context_manager(**kwargs) as val:
            # Wait for devnet to be ready (especially important for forked devnet)
            port = val.starknet.port
            if not wait_for_devnet(port, timeout=120):
                raise RuntimeError(f"Devnet at port {port} failed to start within timeout")
            # Extra stabilization time after devnet is responsive
            time.sleep(2)
            yield val

    return _factory


@pytest.fixture
def starknet_forked(
    starknet_test_utils_factory: Callable[..., Iterator[StarknetTestUtils]]
) -> Iterator[StarknetTestUtils]:
    with starknet_test_utils_factory(
        fork_network="https://rpc.pathfinder.equilibrium.co/mainnet/rpc/v0_10",
        fork_block=FORK_BLOCK,
        starknet_chain_id=StarknetChainId.MAINNET,
        start_time=NOW_TIMESTAMP,
    ) as val:
        yield val


@pytest.fixture
def starknet_forked_with_impersonated_accounts(
    starknet_forked: StarknetTestUtils,
    accounts_to_impersonate: dict[str, tuple[int, int]],
) -> StarknetTestUtils:
    """
    Impersonate the operator account in the forked Starknet instance.
    """
    client = starknet_forked.starknet.get_client()
    for address, _ in accounts_to_impersonate.values():
        client.impersonate_account_sync(address)

    return starknet_forked


@pytest.fixture
def accounts(
    starknet_forked_with_impersonated_accounts: StarknetTestUtils,
    accounts_to_impersonate: dict[str, tuple[int, int]],
) -> dict[str, Account]:
    """
    Return a dictionary of Account instances for the impersonated accounts.
    """
    return {
        account: Account(
            client=starknet_forked_with_impersonated_accounts.starknet.get_client(),
            address=Address(address),
            # Use a dummy private key since the account is impersonated.
            key_pair=KeyPair.from_private_key(dummy_key),
            chain=StarknetChainId.MAINNET,
        )
        for account, (address, dummy_key) in accounts_to_impersonate.items()
    }


@pytest_asyncio.fixture
async def contracts(
    accounts: dict[str, Account],
) -> dict[str, Contract]:
    """
    Return a dictionary of Contract instances for the contracts.
    """
    return await contracts_inner_fixture(PERPETUALS_CONTRACT_ADDRESS, accounts)


async def contracts_inner_fixture(
    contract_address: int,
    accounts: Union[dict[str, Account], list[Account]],
) -> Union[dict[str, Contract], dict[Account, Contract]]:
    result = {}
    if isinstance(accounts, dict):
        for account_name, account in accounts.items():
            abi, cairo_version = await ContractAbiResolver(
                address=contract_address,
                client=account.client,
                proxy_config=ProxyConfig(),
            ).resolve()
            result[account_name] = Contract(
                address=contract_address,
                abi=abi,
                provider=account,
                cairo_version=cairo_version,
            )
    elif isinstance(accounts, list):
        for account in accounts:
            abi, cairo_version = await ContractAbiResolver(
                address=contract_address,
                client=account.client,
                proxy_config=ProxyConfig(),
            ).resolve()
            result[account] = Contract(
                address=contract_address,
                abi=abi,
                provider=account,
                cairo_version=cairo_version,
            )
    return result


@pytest.fixture
def test_utils(
    starknet_forked_with_impersonated_accounts: StarknetTestUtils,
    accounts: dict[str, Account],
    contracts: dict[str, Contract],
):
    """
    Function-scoped PerpetualsTestUtils instance.
    Each test gets its own instance with its own fork.
    """
    from devnet_tests.perpetuals_test_utils import PerpetualsTestUtils

    return PerpetualsTestUtils(
        starknet_forked_with_impersonated_accounts,
        PERPETUALS_CONTRACT_ADDRESS,
        NOW_TIMESTAMP,
        EXTENDED_ACTIVE_SYNTHETIC_ASSET_IDS,
        accounts,
        contracts,
    )
