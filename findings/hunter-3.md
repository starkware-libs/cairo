# Hunter #3 findings — executable format & plugin

Area: `crates/cairo-lang-executable/src/{executable.rs,plugin.rs,lib.rs}` (there is no
`plugin.rs` in this crate — the crate only has `compile.rs`, `debug_info.rs`, `executable.rs`,
`lib.rs`; the actual macro-plugin logic lives in `crates/cairo-lang-executable-plugin/src/lib.rs`,
which was reviewed instead) and `crates/cairo-lang-executable-plugin/src/`.

## Bug 1: unbounded parameter index baked into a felt252 short-string literal overflows for functions with >= 101 parameters

- **File + location**: `crates/cairo-lang-executable-plugin/src/lib.rs:176-182`, inside
  `ExecutablePlugin::generate_code`:

```rust
builder.add_modified(
    RewriteNode::Text(format!(
        "    let __param{EXECUTABLE_PREFIX}{param_idx} = Serde::deserialize(ref \
         input).expect('Failed to deserialize param #{param_idx}');\n"
    ))
    .mapped(db, param),
);
```

- **Description**: For every parameter of a `#[executable]`-annotated function, the plugin
  generates a call to `.expect(...)` with a short-string literal
  `'Failed to deserialize param #{param_idx}'`. `param_idx` is the parameter's positional index,
  written with no bound and no truncation. Cairo short-string (`felt252`) literals are byte-packed
  into a single field element, and the literal is only valid if the packed value is within the
  felt252 range (roughly 31 ASCII bytes maximum, fewer if the leading byte is large, as it is
  here since the string starts with the letter `F`, 0x46).
  `"Failed to deserialize param #"` alone is 29 ASCII characters. Once `param_idx` reaches 3
  digits (i.e. the 101st parameter, index `100`), the literal becomes 32 characters, whose packed
  numeric value exceeds the felt252 modulus. This causes the **generated** code to fail semantic
  validation with `E2008: The value does not fit within the range of type core::felt252.`, and the
  diagnostic is anchored (via `RewriteNode` code-mapping) onto the user's original function
  signature — even though the user wrote nothing incorrect. A `#[executable]` function with 101 or
  more parameters can therefore never compile, with a confusing, seemingly-unrelated error message
  pointing at the tail of the function's parameter list.
- **Root cause**: `param_idx` is interpolated into a Cairo short-string literal without any bound
  check or fallback (e.g. dropping the index from the message, or emitting it as a separate
  `felt252`/numeric literal instead of embedding it in a short string). The generated diagnostic
  message silently grows past the language's own short-string capacity as parameter count grows.
- **How reproduced**: Verified live against the real plugin/semantic pipeline (not by poking
  internals — this is exactly what the compiler does when expanding a `#[executable]` function
  with many parameters). I temporarily added a `#[test]` to
  `crates/cairo-lang-executable-plugin/src/test.rs` that builds a 101-parameter
  `#[executable]` function through `cairo_lang_semantic::test_utils::setup_test_module` +
  `cairo_lang_plugins::test_utils::expand_module_text` (the exact same harness the crate's own
  golden tests use), and observed:

  ```
  error[E2008]: The value does not fit within the range of type core::felt252.
   --> lib.cairo:2:1399
  fn main(a0: felt252, a1: felt252, ..., a100: felt252) {}
                                                                        ^^^^^^^^^^^^^
  ```

  The test was reverted afterward (repo is clean — only used for verification, per task
  instructions not to leave stray changes).

- **Full test code** (idiomatic usage of the real public API — drop into a `#[test]` in
  `crates/cairo-lang-executable-plugin/src/test.rs`, which already has `SHARED_DB`,
  `setup_test_module`, and `expand_module_text` imported):

```rust
#[test]
fn executable_with_101_params_fails_to_compile() {
    // A perfectly normal (if unusually large) #[executable] function: 101 felt252 params.
    let params = (0..101).map(|i| format!("a{i}: felt252")).collect::<Vec<_>>().join(", ");
    let cairo_code = format!("#[executable]\nfn main({params}) {{}}");

    let db = SHARED_DB.lock().unwrap().snapshot();
    let (test_module, semantic_diagnostics) = setup_test_module(&db, &cairo_code).split();
    let _ = expand_module_text(&db, test_module.module_id, &mut vec![]);

    // BUG: valid user code with 101 params fails to compile because the plugin's generated
    // `.expect('Failed to deserialize param #100')` literal is 32 ASCII bytes and thus does not
    // fit in a felt252 short string. The diagnostic is misleadingly anchored on the user's
    // function signature, not on anything the user actually got wrong.
    assert!(
        semantic_diagnostics.contains("does not fit within the range of type core::felt252"),
        "expected a felt252-out-of-range diagnostic caused by the generated deserialize-error \
         short string, got:\n{semantic_diagnostics}"
    );
}
```

- **How to verify**: Paste the test above into
  `crates/cairo-lang-executable-plugin/src/test.rs` (it already `use`s everything needed) and run:

  ```
  cargo test -p cairo-lang-executable-plugin --lib executable_with_101_params_fails_to_compile -- --nocapture
  ```

  It passes today (i.e. the bug reproduces), demonstrating that any `#[executable]` function with
  101+ parameters is currently uncompilable due to this plugin-generated literal, independent of
  anything the user's code does. (100 parameters, indices 0-99, is fine: the longest literal is 31
  bytes, at the felt252 short-string limit — with 101 parameters, index `100` makes the literal 32
  bytes and it breaks.)
- **Severity note**: This is an extreme parameter count for a real function, so practical impact
  is low, but it is a genuine, deterministic, unconditional compile failure for legitimate Cairo
  source with no workaround available to the user (they cannot change how the plugin formats its
  own generated error message). Filing as a real, demonstrated (not merely suspected) bug given
  the reproduction above.

## Other areas reviewed, no bugs found

I carefully traced the following and found the logic to match documented behavior and existing
golden tests (`plugin_test_data/{diagnostics,expansion}`, `compile_test_data/basic`):

- `Executable::new` (`crates/cairo-lang-executable/src/executable.rs:24-62`): the
  `non_returning_header` (`ap += builtins.len(); call rel 4; jmp rel 0;`) is exactly
  `NOT_RETURNING_HEADER_SIZE` (6) words; the `call rel 4` correctly lands on the first instruction
  of `compiled.wrapper.header` (offset 6); the `Standalone` entrypoint (offset 0) and `Bootloader`
  entrypoint (offset `NOT_RETURNING_HEADER_SIZE`) match their doc comments; `debug_info.annotations`
  `program_offset` correctly accounts for the non-returning header plus the wrapper header's total
  op-size, landing exactly at the start of the Sierra-compiled body. Cross-checked against
  `CairoProgram::assemble_ex` in `cairo-lang-sierra-to-casm` (`header ++ self.instructions ++ consts
  ++ footer`), and against multiple golden outputs in `compile_test_data/basic` — all consistent.
- `RawExecutableAnalyzer` (`crates/cairo-lang-executable-plugin/src/lib.rs:47-121`): return-type,
  param-count, param-types and param-mutability checks for `#[executable_raw]` all match their
  error messages and the `plugin_test_data/diagnostics` golden cases exactly (including the
  input-must-not-be-`ref` / output-must-be-`ref` mutability checks, which are easy to get backwards
  but are not here).
  actually checked the `Mutability` enum (`Immutable`/`Mutable`/`Reference`) in
  `cairo-lang-semantic/src/semantic.rs` to confirm `mut` (`Mutable`) is correctly distinct from
  `ref` (`Reference`) so a `mut input: Span<felt252>` param is correctly accepted while a
  `ref input: Span<felt252>` param is correctly rejected.
- `ExecutablePlugin::generate_code` (`crates/cairo-lang-executable-plugin/src/lib.rs:127-231`):
  generic-params rejection, `ref`-param rejection, per-param `Serde::deserialize` generation with
  correct positional indices, the `assert(...is_empty(input)..., 'Input too long for params.')`
  trailing check, and the final `Serde::serialize(__result, ref output)` — all match the golden
  expansions verbatim, and diagnostic code-mapping (`.mapped(db, param)` / `.mapped(db, &clause)`)
  correctly reproduces the original-source positions shown in
  `plugin_test_data/diagnostics` (e.g. "no Serde impl" pointing at the original param/return-type
  location, not the generated code).
- `declared_attributes` / `executable_attributes` (`lib.rs:224-230`): correctly declare
  `executable` + `executable_raw` as known attributes, and correctly mark only `executable_raw` as
  the "executable attribute" consumed by `find_executable_function_ids` in
  `cairo-lang-sierra-generator/src/executables.rs` — since `#[executable]` always expands into a
  `#[executable_raw]`-tagged wrapper, this is the correct function to key off of.
- `IMPLICIT_PRECEDENCE` list (`lib.rs:32-41`): compared against the analogous list in
  `cairo-lang-starknet/src/plugin/consts.rs`; the executable variant is a strict subset (it
  correctly omits `SegmentArena`/`GasBuiltin`/`System`, none of which apply to `#[executable]`
  compilation — gas is disabled and syscalls are rejected) in the same relative order as the
  starknet list. Confirmed via `compile_test_data/basic` that functions using `SegmentArena`
  (dict) implicitly still compile correctly without it being declared in the precedence list.
- `compile.rs` (`prepare_db`, `compile_executable`, `compile_executable_in_prepared_db`,
  `find_executable_functions`, `originating_function_path`,
  `compile_executable_function_in_prepared_db`): traced the "ambiguous executable" error path,
  the `--executable <path>` filtering via `originating_function_path`'s prefix/suffix stripping of
  `EXECUTABLE_PREFIX`, and the syscall-libfunc rejection loop (`_syscall` suffix check) against the
  `Test use System implicit with keccak/sha256` golden cases — all consistent. No off-by-one or
  inverted-condition bugs found.
- `debug_info.rs`: `Annotations`/`ProgramInformation`/`DebugInfo` serde plumbing is a straightforward
  wrapper; `#[serde(skip_serializing_if = "Annotations::is_empty")]` and `OrderedHashMap` usage look
  correct and match the `github.com/software-mansion/cairo-profiler` namespace convention described
  in the doc comment.
- `lib.rs` (both crates): plain module wiring, nothing notable.

## Files checked

- `/home/user/cairo/crates/cairo-lang-executable/src/executable.rs`
- `/home/user/cairo/crates/cairo-lang-executable/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-executable/src/compile.rs`
- `/home/user/cairo/crates/cairo-lang-executable/src/debug_info.rs`
- `/home/user/cairo/crates/cairo-lang-executable/src/test.rs`
- `/home/user/cairo/crates/cairo-lang-executable/src/compile_test_data/basic`
- `/home/user/cairo/crates/cairo-lang-executable/Cargo.toml`
- `/home/user/cairo/crates/cairo-lang-executable-plugin/src/lib.rs`
- `/home/user/cairo/crates/cairo-lang-executable-plugin/src/test.rs`
- `/home/user/cairo/crates/cairo-lang-executable-plugin/src/plugin_test_data/diagnostics`
- `/home/user/cairo/crates/cairo-lang-executable-plugin/src/plugin_test_data/expansion`
- `/home/user/cairo/crates/cairo-lang-executable-plugin/Cargo.toml`
- `/home/user/cairo/crates/cairo-lang-sierra-generator/src/executables.rs` (read for context on
  how `executable_attributes` is consumed; not in primary focus area, no bugs found)
- `/home/user/cairo/crates/cairo-lang-semantic/src/semantic.rs` (read `Mutability` enum definition
  for context; not in primary focus area)
- `/home/user/cairo/crates/cairo-lang-semantic/src/corelib.rs` (read `validate_literal`/
  `LiteralError` for context on Bug 1's root cause; not in primary focus area)
- `/home/user/cairo/crates/cairo-lang-semantic/src/expr/compute.rs` (read literal
  finalization/range-check logic for context on Bug 1's root cause; not in primary focus area)
- `/home/user/cairo/crates/cairo-lang-sierra-to-casm/src/compiler.rs` (read `assemble_ex` for
  context on `Executable::new`'s bytecode layout; not in primary focus area)
- `/home/user/cairo/crates/cairo-lang-starknet/src/plugin/consts.rs` (read `IMPLICIT_PRECEDENCE`
  for comparison; not in primary focus area)

Per project convention I did not read or grep `crates/cairo-lang-syntax/src/node/ast.rs`.
