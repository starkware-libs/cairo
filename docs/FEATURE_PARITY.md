# Cairo 0 Features Parity

On this page we track the missing features to reach feature parity with the old compiler version. We divided them into Cairo, Starknet and specific system calls in Starknet OS.

If we missed a feature, please let us know.

## Cairo features
| name                   | status | description |
|------------------------|--------|-------------|
| `if (x == y)`          | ✅     | Conditional expression comparing two values |
| `if (x == y & z == w)` | ✅     | Conditional expression with multiple comparisons |
| Short strings          | ✅     | Support for short string literals |
| Structs                | ✅     | Support for struct data types |
| Builtin Range check    | ✅     | Range check builtin functionality |
| Builtin Pedersen       | ✅     | Pedersen hash builtin functionality |
| Builtin Bitwise        | ✅     | Bitwise operations builtin functionality |
| Uint256                | ✅     | Support for 256-bit unsigned integers |
| Builtin ec-op          | ✅     | Elliptic curve operations builtin functionality |
| Append-only arrays     | ✅     | Support for arrays that can only be appended to |
| Named arguments        | ✅     | Support for named function arguments |
| Serde (serialization)  | ✅     | Serialization and deserialization functionality |
| Dict                   | ✅     | Dictionary data structure |
| `if (cond1 && cond2)`  | ✅     | Logical AND in conditional expressions |
| Find element           | ❌     | Array search functionality to find elements by predicate |


---

## Starknet features

| name                                      | status | description |
|-------------------------------------------|--------|-------------|
| Contract interface                        | ✅     | Interface definition for contracts |
| External functions and view functions     | ✅     | Functions that can be called from outside the contract |
| Storage variables - felts                 | ✅     | Support for field element storage variables |
| Storage variables - mapping               | ✅     | Support for mapping storage variables |
| Storage variables - other types as values | ✅     | Support for complex types in storage variables |
| Events                                    | ✅     | Support for contract events |


---

## Starknet system calls

| name                  | status | description |
|-----------------------|--------|-------------|
| storage_read          | ✅     | Read from contract storage |
| storage_write         | ✅     | Write to contract storage |
| get_caller_address    | ✅     | Get the address of the calling contract |
| call_contract         | ✅     | Call another contract |
| library_call          | ✅     | Call a library contract |
| deploy                | ✅     | Deploy a new contract |
| get_block_number      | ✅     | Get the current block number |
| get_block_timestamp   | ✅     | Get the current block timestamp |
| get_contract_address  | ✅     | Get the address of the current contract |
| get_sequencer_address | ✅     | Get the address of the sequencer |
| get_transaction_info  | ✅     | Get information about the current transaction |
| send_message_to_l1    | ✅     | Send a message to Ethereum L1 |

---

## Developer Tools and Documentation

This section tracks the progress of developer tools and documentation that enhance the Cairo development experience.

| name                       | status | description |
|----------------------------|--------|-------------|
| VSCode Extension           | ✅     | Syntax highlighting and language support for Cairo in VSCode |
| Language Server Protocol   | ✅     | LSP implementation for Cairo |
| Debugger                   | ❌     | Interactive debugger for Cairo programs |
| Comprehensive API Docs     | 🔄     | Complete API documentation for all Cairo libraries |
| Interactive Tutorial       | ❌     | Step-by-step interactive tutorial for learning Cairo |
| Performance Profiler       | ❌     | Tool for profiling Cairo program performance |
| Code Coverage Tool         | ❌     | Tool for measuring test coverage in Cairo code |
| Package Manager            | ✅     | Tool for managing Cairo package dependencies |

Status Legend:
- ✅ Implemented
- 🔄 In Progress
- ❌ Not Implemented

