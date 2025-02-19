# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue,
email, or any other method with the owners of this repository before making a change.
Please note we have a [code of conduct](CODE_OF_CONDUCT.md), please follow it in all your
interactions with the project.

## Development environment setup

To set up a development environment, please follow these steps:

1. Clone the repo

   ```sh
   git clone https://github.com/starkware-libs/cairo
   ```

2. Download and install [Visual Studio Code](https://code.visualstudio.com/).

3. Install the [Cairo extension for Visual Studio Code][vscode-cairo] from the extension
   marketplace.

The `rustfmt` configuration used by cairo requires a nightly version of Rust.
You can install the nightly version by running.

```sh
rustup install nightly-2025-02-16
```

## Running Tests

There are various types of tests within the repository, including both Cairo-based and Rust-based
tests.

### Running Cairo Tests

To run Cairo tests, we use the `cairo-test` CLI tool, designed for executing tests written in Cairo.
This tool is not exclusive to the Compiler repository and can be used in any Cairo project.
To run the tests, execute the following command:

```sh
cargo run --bin cairo-test -- <path-to-cairo-project>
```

If you want to run a single test file, you can use the `--single-file` flag. For example, to run
the `test.cairo` file within the `tests` directory, use the following command:

```sh
cargo run --bin cairo-test -- --single-file tests/test.cairo
```

Two scripts are available to run tests within the Compiler repository:

1. `scripts/cairo_test.sh`: Executes tests within the `corelib` Cairo project and
   the `tests/bug_samples` Cairo project.
2. `scripts/starknet_test.sh`: Runs tests within the `cairo-lang-starknet/cairo_level_tests` Cairo
   project.

### Running Rust Tests

Rust-based tests can be executed using `cargo test`, similar to any Rust project.
However, some tests are written within an internal testing framework, which supports additional
options.
Test inputs and expected outputs are stored in files without extensions,
typically within a dedicated directory.

Additional options can be controlled via environment variables:

- `CAIRO_FIX_TESTS`: When set to 1, the tests will automatically adjust the expected outputs,
  and format all the code sections in the tests.
  This is particularly useful when making code changes that affect a lot of tests.
- `CAIRO_TEST_FILTER`: This variable filters tests based on a substring contained within their
  names.
- `CAIRO_SKIP_FORMAT_TESTS`: By default, test sections containing Cairo code are formatted before
  execution,
  and the test will fail if the formatting is incorrect or be fixed if
  `CAIRO_FIX_TESTS` is set to 1.
  Setting this variable to 1 skips this formatting step.

For example, to run tests containing the substring foo, automatically fix expected outputs, and
skip the formatting of the tests, use the following command:

```sh
CAIRO_FIX_TESTS=1 CAIRO_TEST_FILTER=foo CAIRO_SKIP_FORMAT_TESTS=1 cargo test
```

## Issues and feature requests

You've found a bug in the source code, a mistake in the documentation, or maybe you'd like a new
feature? Take a look at [GitHub Discussions](https://github.com/starkware-libs/cairo/discussions)
to see if it's already being discussed. You can help us by
[submitting an issue on GitHub](https://github.com/starkware-libs/cairo/issues).
Before you create an issue, make sure to search the issue archive—your issue may have already
been addressed!

Please try to create bug reports that are:

- _Reproducible._ Include steps to reproduce the problem.
- _Specific._ Include as much detail as possible: which version, what environment, etc.
- _Unique._ Do not duplicate existing opened issues.
- _Scoped to a Single Bug._ One bug per report.

**Even better: Submit a pull request with a fix or new feature!**

### How to submit a Pull Request

1. Search our repository for open or closed
   [Pull Requests](https://github.com/starkware-libs/cairo/pulls)
   that relate to your submission. You don't want to duplicate effort.
2. Fork the project
3. Create your feature branch (`git checkout -b feat/amazing_feature`)
4. Implement your feature
5. Run the code formatter for Rust and Cairo (`scripts/rust_fmt.sh && scripts/cairo_fmt.sh`)
6. Commit your changes (`git commit -m 'feat: Add amazing_feature'`)
7. Push to the branch (`git push origin feat/amazing_feature`)
8. [Open a Pull Request](https://github.com/starkware-libs/cairo/compare) to one of the following
   branches:
    * [main](https://github.com/starkware-libs/cairo/tree/main) -
      If the change does not break any existing Cairo code (**Cairo backward-compatible**), and does
      not affect Sierra to CASM compilation.
    * [cairo-major-update](https://github.com/starkware-libs/cairo/tree/cairo-major-update) -
      If the change breaks the existing high-level code (**Cairo non-backward-compatible**), and
      does not affect Sierra to CASM compilation.
    * [sierra-minor-update](https://github.com/starkware-libs/cairo/tree/sierra-minor-update) -
      If Sierra to CASM compilation changed in a **backward-compatible** manner — meaning existing
      Sierra code will compile to the same CASM.
      For example, addition of a new libfunc.
    * [sierra-major-update](https://github.com/starkware-libs/cairo/tree/sierra-major-update) -
      If Sierra to CASM compilation changed in a **non-backward-compatible** manner — meaning
      existing Sierra code will compile to different CASM.
      For example, editing the CASM implementation of an existing libfunc.
      This may also include no longer accepting some old Sierra code.

[vscode-cairo]: https://marketplace.visualstudio.com/items?itemName=starkware.cairo1
