# Contributing to CairoLS

This document is a supplement to the [general contributing guidelines](../../docs/CONTRIBUTING.md)
for the Cairo repository.
Please read that document first carefully before proceeding with the instructions below.

This document you are using Visual Studio Code with the [Cairo extension](../../vscode-cairo) as the
editor of choice for running CairoLS against.
You might also want to check out
the [Cairo extension's contributing guidelines](../../vscode-cairo/CONTRIBUTING.md).

## Commits

If your commit/pull request is solely related to CairoLS, please prefix your commit message/PR title
with `LS: `.

## Testing

We are building an extensive end-to-end tests suite for CairoLS [here](./tests/e2e).
These tests implement a simple language client that you can control (like put a cursor at certain
position, send a request to the server, etc.).
Check out existing tests for examples of what you can do.
If you need, do not hesitate to extend the language client with new capabilities!
Its source code is located [here](./tests/e2e/support/mod.rs).

> [!IMPORTANT]
> The test suite is not complete, but we **require** adding tests for any new developments.

Mind that these tests tend to be slow, so try to stuff as much as possible into a single test,
to reduce the overhead of constant sections, like booting LS or analysing the `core` crate.

## Debugging

This section shows some more advanced tricks that you can employ to debug the CairoLS.

### Advanced logging

You can enable more [granular][env-filter-directives] logging by configuring environment variables
for the language server.
To do so, paste the following into your `.vscode/settings.json`:

```json
{
    "cairo1.languageServerExtraEnv": {
        "CAIRO_LS_LOG": "cairo_lang_language_server=debug",
        "RUST_BACKTRACE": "1"
    }
}
```

### Profiling

CairoLS has built-in support for generating profile files based on its tracing/logging system.
This mechanism allows investigating various cases of slow query execution, deadlocks, and other
performance issues.

To generate a profile file, paste the following into your `.vscode/settings.json`:

```json
{
    "cairo1.languageServerExtraEnv": {
        "CAIRO_LS_PROFILE": "1"
    }
}
```

This will generate a trace file that you'll be able to further analyze.
CairoLS will print the path to this trace file and instructions on how to analyze it on its standard
error.
In Visual Studio Code you will find this output in the `Output` → `Cairo Language Server` panel.
We're not copying these here because nobody will bother keeping this document in sync.

### Use tests

If you find a short reproduction of your problem, we strongly suggest writing an E2E test and
including it in your PR.
Not only will this make your development cycle faster (because checking your changes will be now
automated),
but you will also enable future developers not to fall into the pitfall that caused the bug you
found and debugged 🤓.

[env-filter-directives]: https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html#directives
