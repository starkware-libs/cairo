# Bug Hunter #12 — Formatter (line breaking / wrapping) findings

Area: `crates/cairo-lang-formatter/src/{formatter_impl.rs, node_properties.rs}`,
focused on line-break decisions, indentation, spacing, comment/trivia placement.
(Module-sort / `#[cairofmt::skip]` / take_doc / RewriteNode issues explicitly avoided.)

---

## Bug 1: Line width is measured in **bytes**, not characters — multi-byte (non-ASCII) comments cause spurious line breaks and wrong comment wrapping

### Severity
Correctness + documentation/behavior mismatch. Produces objectively wrong output
(unnecessary/incorrect line breaks) for any code containing non-ASCII comments
(e.g. accented letters, Greek, CJK in `//`, `///`, or `//!` comments). Output is
still *idempotent*, so it is silently wrong rather than oscillating.

Cairo identifiers and string literals must be ASCII (the parser rejects non-ASCII
strings with `StringMustBeAscii`), so the only place multi-byte UTF-8 legitimately
appears in a source file is **comments** — which is exactly where this bug bites.

### Location(s) — same root cause, two manifestations

1. **Trailing-comment width** — `crates/cairo-lang-formatter/src/formatter_impl.rs:278-294`
   (`LineComponent::width`):

   ```rust
   pub fn width(&self) -> usize {
       match self {
           Self::Token(s) => s.len(),                 // <-- bytes, not chars
           ...
           Self::Comment { content, is_trailing } => {
               if *is_trailing {
                   content.len()                      // <-- bytes, not chars
               } else {
                   0
               }
           }
       }
   }
   ```

   The doc-comments on the width helpers explicitly promise characters, e.g.
   `formatter_impl.rs:432` (`/// The width, in number of chars, of the whole LineTree.`)
   and `formatter_impl.rs:436-437`. The implementation uses `str::len()` (UTF-8 byte
   length). For a trailing comment made of N 2-byte code points, the measured width is
   ~2× the real column width, so a line that visibly fits in 100 columns is treated as
   over-long and gets broken.

2. **Leading-comment word wrapping** — `crates/cairo-lang-formatter/src/formatter_impl.rs:792-822`
   (`format_leading_comment`):

   ```rust
   let max_comment_width = max_line_width
       .saturating_sub(cur_indent)
       .saturating_sub(orig_comment_line.n_slashes)
       ...
   for word in orig_comment_line.content.split(' ') {
       if current_line.content.is_empty()
           || current_line.content.len() + word.len() <= max_comment_width  // <-- bytes
       {
           current_line.content.push_str(word);
           ...
   ```

   `current_line.content.len()` and `word.len()` are byte lengths compared against a
   column budget, so a comment whose visible width is well under the limit gets wrapped
   onto extra lines when it contains multi-byte characters.

### Root cause
`str::len()` returns the number of UTF-8 bytes, not the number of characters/columns.
Width/column budgeting must use `content.chars().count()` (or a proper display-width
measure). Because `Token` content is always ASCII in valid Cairo, only the two
comment paths above are observably affected.

### Demonstration (measured, real public-API formatting)

All runs use the default `FormatterConfig` (max_line_length = 100), via
`get_formatted_file` on a `SimpleParserDatabase` — the exact path the crate's own
`test.rs` uses.

**Manifestation 1 — trailing comment forces a spurious break.**

Input (`α` = U+03B1, 2 bytes each; 60 of them):
```cairo
fn f() { let x = 1; // αααααααααααααααααααααααααααααααααααααααααααααααααααααααααααα
}
```
Actual output (WRONG — `let x = 1;` split even though the whole line is only **74**
visible columns):
```cairo
fn f() {
    let x =
        1; // αααααααααααααααααααααααααααααααααααααααααααααααααααααααααααα
}
```
ASCII control with an *even longer* visible line (78 columns) is left untouched,
proving the break is caused by byte-count, not real width:
```cairo
// input:  fn f() { let x = 1; // aaaaaaaa...(60 a's) }
// output (correct, one line):
fn f() {
    let x = 1; // aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
}
```

**Manifestation 2 — leading comment wraps far too early.**

Input: a `//` comment of 30 words `αα` (visible width incl. `// ` ≈ 93 columns):
```cairo
fn f() {
    // αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα
    let x = 1;
}
```
Actual output (WRONG — wrapped into two comment lines, first line only **60** visible
columns):
```cairo
fn f() {
    // αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα αα
    // αα αα αα αα αα αα αα αα αα αα αα αα
    let x = 1;
}
```
The ASCII control (`aa` words, 96 visible columns) stays on a single line, confirming
the premature wrap is byte-driven.

### Full self-contained regression test

Add to `crates/cairo-lang-formatter/src/test.rs` (uses only the existing test deps:
`SimpleParserDatabase`, `get_formatted_file`, `FormatterConfig`, `pretty_assertions`):

```rust
#[test]
fn multibyte_comment_width_is_measured_in_chars() {
    use cairo_lang_parser::utils::SimpleParserDatabase;
    use crate::{FormatterConfig, get_formatted_file};

    let db = SimpleParserDatabase::default();
    let config = FormatterConfig::default(); // max_line_length = 100

    let fmt = |src: &str| {
        let root = db.parse_virtual(src).expect("parse failed");
        get_formatted_file(&db, &root, config.clone())
    };

    // --- Manifestation 1: trailing comment (74 visible columns) must NOT break. ---
    let greek60: String = "α".repeat(60);
    let input1 = format!("fn f() {{ let x = 1; // {greek60}\n}}");
    let expected1 = format!("fn f() {{\n    let x = 1; // {greek60}\n}}\n");
    // ASCII control of the SAME/greater visible width is left on one line, so the
    // multi-byte version must be too.
    assert_eq!(fmt(&input1), expected1,
        "trailing multi-byte comment triggered a spurious line break (width counted in bytes)");

    // --- Manifestation 2: leading comment (~93 visible columns) must NOT wrap. ---
    let greek_words: String =
        std::iter::repeat("αα").take(30).collect::<Vec<_>>().join(" ");
    let input2 = format!("fn f() {{\n    // {greek_words}\n    let x = 1;\n}}");
    let expected2 = format!("fn f() {{\n    // {greek_words}\n    let x = 1;\n}}\n");
    assert_eq!(fmt(&input2), expected2,
        "leading multi-byte comment was wrapped early (width counted in bytes)");
}
```

With today's code both assertions FAIL, producing the WRONG outputs shown above.
(The corresponding ASCII inputs — 60 `a`'s / 30 `aa` words — pass, i.e. are left
un-broken, which is the correct behavior the multi-byte inputs should also get.)

### How to verify
- Add the test to `test.rs` and run `cargo test -p cairo-lang-formatter multibyte`.
- Or format the two `.cairo` inputs above with the CLI (`scarb fmt` / the crate's
  formatter) at default settings and observe the extra breaks/wraps.

### Suggested fix
In `LineComponent::width`, use `s.chars().count()` for `Token` and
`content.chars().count()` for the trailing-`Comment` branch; in
`format_leading_comment`, compare `current_line.content.chars().count() +
word.chars().count()` against `max_comment_width` (and derive `max_comment_width`
from char counts as well). Prefer a shared "display width" helper so all width
accounting is consistent.

---

## Notes on areas checked that were clean

The formatter is otherwise robust: I ran idempotency (`format(format(x)) == format(x)`)
and correctness spot-checks over a broad set of real Cairo inputs and found **no**
non-idempotency and no token/comment loss or duplication in:

- long method chains, binary-operator chains, nested closures, closure call-chains;
- long attributes (item- and field-level), generic param/arg lists, tuples, arrays,
  struct constructors, nested generic types;
- `match` arms (including block arms with trailing commas, doc-comments on arms),
  `if/else if/else` chains, `let ... else`, deeply nested blocks;
- comments in many positions: leading/trailing, dangling-before-`}`, inner (`//!`),
  doc (`///`), comments between call args, comment after `{`, mixed slash counts,
  trailing whitespace in comments, comment-only files;
- blank-line collapsing (module-level, statement-level, inside `match`),
  empty struct/fn bodies, semicolon handling before post-operators.

Only the byte-vs-char width issue above reproduced as a genuine defect.

## Files checked
- `crates/cairo-lang-formatter/src/formatter_impl.rs` (line-break engine: `LineComponent::width`, `width_between`, `break_line_tree*`, `format_leading_comment`, `push_comment`, comment/trivia handling, `format_node`/`format_internal`/`format_terminal`/`format_trivia`/`format_token`)
- `crates/cairo-lang-formatter/src/node_properties.rs` (break-point weights, spacing rules, protected-zone precedences, `should_skip_terminal`)
- `crates/cairo-lang-formatter/src/lib.rs` (public API, `FormatterConfig` defaults)
- `crates/cairo-lang-formatter/src/test.rs` (test harness / golden-file conventions)
- `crates/cairo-lang-formatter/test_data/**` (existing golden inputs/outputs for comment_overflow, trailing_comment, linebreaking, etc.)
- Verified with a temporary integration test driving `get_formatted_file` on `SimpleParserDatabase` (removed after use).
