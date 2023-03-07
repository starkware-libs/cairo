# Contributing

When contributing to this repository, please first discuss the change you wish to make via issue, email, or any other method with the owners of this repository before making a change.
Please note we have a [code of conduct](CODE_OF_CONDUCT.md), please follow it in all your interactions with the project.

## Development environment setup

> **[?]**
> Proceed to describe how to setup local development environment.
> e.g:

To set up a development environment, please follow these steps:

1. Clone the repo

   ```sh
   git clone https://github.com/starkware-libs-cairo
   ```

2. Download and install [VSCode](https://code.visualstudio.com/).

3. Follow instructions in [vscode-cairo](../vscode-cairo/README.md).

If you have a lot of failing tests when running
`cargo test`
and those are due to file diff not being correct, there is a way to automatically fix it.
Run the tests again, with the `CAIRO_FIX_TESTS` environment variable set to `1`:
```sh
CAIRO_FIX_TESTS=1 cargo test
```
Some tests should now be fixed.

## Issues and feature requests

You've found a bug in the source code, a mistake in the documentation or maybe you'd like a new feature?Take a look at [GitHub Discussions](https://github.com/starkware-libs-cairo/discussions) to see if it's already being discussed.  You can help us by [submitting an issue on GitHub](https://github.com/starkware-libs-cairo/issues). Before you create an issue, make sure to search the issue archive -- your issue may have already been addressed!

Please try to create bug reports that are:

- _Reproducible._ Include steps to reproduce the problem.
- _Specific._ Include as much detail as possible: which version, what environment, etc.
- _Unique._ Do not duplicate existing opened issues.
- _Scoped to a Single Bug._ One bug per report.

**Even better: Submit a pull request with a fix or new feature!**

### How to submit a Pull Request

1. Search our repository for open or closed
   [Pull Requests](https://github.com/starkware-libs-cairo/pulls)
   that relate to your submission. You don't want to duplicate effort.
2. Fork the project
3. Create your feature branch (`git checkout -b feat/amazing_feature`)
4. Implement your feature
5. Run the code formatter for Rust and Cairo (`scripts/rust_fmt.sh && scripts/cairo_fmt.sh`)
6. Commit your changes (`git commit -m 'feat: Add amazing_feature'`)
7. Push to the branch (`git push origin feat/amazing_feature`)
8. [Open a Pull Request](https://github.com/starkware-libs/cairo/compare)
