# Item declaration statement

```bnf
ITEM_STMT : ITEM /* except IMPL_ITEM */
```

An _item declaration statement_ has a syntactic form identical to an item declaration within
a module, except implementation items are not permitted.
Declaring an item within a statement block restricts its scope to the block containing the
statement.
The item nor any of its sub-items are not accessible outside the enclosing block's scope.
It is otherwise identical in meaning to declaring the item inside a module.

There is no implicit capture of the containing function's generic parameters, parameters, and local
variables.

## Example

```crust
fn outer() {
    let outer_var = true;

    fn inner() {
        // outer_var is not in scope here
    }

    inner();
}
```
