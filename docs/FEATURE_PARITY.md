# Cairo 0 Features Parity
 
 In this page we track the missing features to reach feature parity with the old compiler version. We dived them by Cairo, StarkNet and specific system calls in StarkNet OS.
 
 If we missed a feature, please let us know.

## Cairo features
| name                   | status |
|------------------------|--------|
| if (x == y)            | ✅      |
| Short strings          | ✅      |
| Builtin Range check    | ✅      |
| Dict                   | ⏳      |
| Uint256                | ⏳      |
| Structs                | ⏳      |
| Builtin Pedersen       | ⏳      |
| Builtin Bitwise        | ⏳      |
| Builtin ec-op          | ⏳      |
| Append-only arrays     |        |
| `if (x == y & z == w)` |        |
| Named arguments        |        |
| Serde (serialization)  |        |
| Find element           |        |


---

## StarkNet features

| name                                      | status |
|-------------------------------------------|--------|
| Contract interface                        | ✅      |
| External functions and view functions     | ✅      |
| Storage variables - felts                 | ✅      |
| Storage variables - mapping               | ⏳      |
| Storage variables - other types as values | ⏳      |
| Events                                    |        |


---

## StarkNet system calls

| name          | status |
|---------------|--------|
| read          | ✅      |
| write         | ✅      |
| call_contract | ⏳      |
| library_call  |        |
|               |        |
