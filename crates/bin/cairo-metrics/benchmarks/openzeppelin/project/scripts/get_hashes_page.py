import sys
import json

KNOWN_ORDER = [
    "ERC20Upgradeable",
    "ERC721Upgradeable",
    "ERC1155Upgradeable",
    "AccountUpgradeable",
    "EthAccountUpgradeable",
    "UniversalDeployer"
]

def main():
    # Required compiler version argument
    cmp_version = sys.argv[1]

    # Read class hashes from stdin
    contracts = json.load(sys.stdin)

    print(generate_doc_file(cmp_version, contracts))


def generate_doc_file(cmp_version, contracts):
    header = f"""// Version
:class-hash-cairo-version: \
https://crates.io/crates/cairo-lang-compiler/{cmp_version}[cairo {cmp_version}]
"""
    hashes = "// Class Hashes\n"
    contracts['contracts'] = remove_prefix_from_names(contracts['contracts'])
    contracts['contracts'].sort(key=lambda x: x['name'])

    hashes += get_known_order_hashes(contracts['contracts'])
    for contract in contracts['contracts']:
        # Avoid the already added contracts in the known order
        if contract['name'] in KNOWN_ORDER:
            continue
        # Avoid adding mocks
        # TODO: remove this after mocks are removed from the artifacts built outside tests
        if "Mock" in contract['name']:
            continue
        hashes += f":{contract['name']}-class-hash: {normalize_len(contract['sierra'])}\n"

    footer = """// Presets page
:presets-page: xref:presets.adoc[Sierra class hash]"""

    return f"{header}\n{hashes}\n{footer}\n"


def remove_prefix_from_names(contracts):
    for contract in contracts:
        contract.update([("name", remove_prefix(contract['name'], 'openzeppelin_presets_'))])
    return contracts


def remove_prefix(text, prefix):
    if text.startswith(prefix):
        return text[len(prefix):]
    return text


def get_known_order_hashes(contracts):
    known_order_hashes = [""] * len(KNOWN_ORDER)
    for contract in contracts:
        if contract['name'] in KNOWN_ORDER:
            index = KNOWN_ORDER.index(contract['name'])
            known_order_hashes[index] = f":{contract['name']}-class-hash: {normalize_len(contract['sierra'])}\n"
    return ''.join(known_order_hashes)


def normalize_len(sierra_hash):
    return "0x" + "0" * (66 - len(sierra_hash)) + sierra_hash[2:]


if __name__ == '__main__':
    main()
