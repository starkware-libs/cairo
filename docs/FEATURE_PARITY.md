# Cairo 0 Features Parity
 
 In this page we track the missing features to reach feature parity with the old compiler version. We divided them by Cairo, Starknet and specific system calls in Starknet OS.
 
 If we missed a feature, please let us know.

## Cairo features
| name                   | status |
|------------------------|--------|
| `if (x == y)`          | ✅     |
| `if (x == y & z == w)` | ✅     |
| Short strings          | ✅     |
| Structs                | ✅     |
| Builtin Range check    | ✅     |
| Builtin Pedersen       | ✅     |
| Builtin Bitwise        | ✅     |
| Uint256                | ✅     |
| Builtin ec-op          | ✅     |
| Append-only arrays     | ✅     |
| Named arguments        | ✅     |
| Serde (serialization)  | ✅     |
| Dict                   | ⏳     |
| `if (cond1 && cond2)`  |        |
| Find element           |        |


---

## Starknet features

| name                                      | status |
|-------------------------------------------|--------|
| Contract interface                        | ✅     |
| External functions and view functions     | ✅     |
| Storage variables - felts                 | ✅     |
| Storage variables - mapping               | ✅     |
| Storage variables - other types as values | ✅     |
| Events                                    | ✅     |


---

## Starknet system calls

| name                  | status |
|-----------------------|--------|
| storage_read          | ✅     |
| storage_write         | ✅     |
| get_caller_address    | ✅     |
| call_contract         | ⏳     |
| library_call          |        |
| deploy                |        |
| get_block_number      |        |
| get_block_timestamp   |        |
| get_contract_address  |        |
| get_sequencer_address |        |
| get_transaction_info  |        |
| send_message_to_l1    |        |
| deploy                |        |

