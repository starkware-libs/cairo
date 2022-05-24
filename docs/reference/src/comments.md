# Comments

```bnf
LINE_COMMENT : "//"  ( ~[/!] ~EOL* )?
ITEM_DOC     : "///" ( ~"/"  ~EOL* )?
MODULE_DOC   : "//!" ~EOL*
```

Comments follow general C++ style of line (`//`) comments.
Non-documentation comments are interpreted as whitespace.

## Documentation comments

**Item** documentation comments begin with _exactly three_ slashes (`///`).
**Module** documentation comments begin with `//!` sequence.
Both comments are treated as documentation attributes attached respectively to next item and
parent module.

Consecutive documentation comments are merged into single documentation attribute.

It is illegal to put module documentation comment within item body.
It is also illegal to put item documentation comment as last element of module body.

## Example

```crust
// Comment
//// Comment

//! Module documentation, line 1.
//! Module documentation, line 2.

/// Item (func) documentation, line 1.
/// Item (func) documentation, line 2.
fn func(i: felt) {
    /// Item (PI) documentation, line 1.
    const PI_2: felt = 3_14;

    /// Illegal item documentation.

    //! Illegal module documentation.
}

//! Module documentation, line 3.

/// Item (bar) documentation, line 1.
fn bar() {}

/// Illegal item documentation.
```
