<div align="center">
  <h1>Cairo 🐺 </h1>
  <h2> ⚡ Blazing ⚡ fast ⚡ compiler for Cairo, written in 🦀 Rust 🦀 </h2>
  <img src="./resources/img/cairo-logo-square.png" height="200" width="200">
  <br />
  <a href="https://github.com/starkware-libs/cairo/issues/new?assignees=&labels=bug&template=01_BUG_REPORT.md&title=bug%3A+">Report a Bug</a>
  -
  <a href="https://github.com/starkware-libs/cairo/issues/new?assignees=&labels=enhancement&template=02_FEATURE_REQUEST.md&title=feat%3A+">Request a Feature</a>
  -
  <a href="https://github.com/starkware-libs/cairo/discussions">Ask a Question</a>
</div>

<div align="center">
<br />

[![GitHub Workflow Status](https://github.com/starkware-libs/cairo/actions/workflows/ci.yml/badge.svg)](https://github.com/starkware-libs/cairo/actions/workflows/ci.yml)
[![Project license](https://img.shields.io/github/license/starkware-libs/cairo.svg?style=flat-square)](LICENSE)
[![Pull Requests welcome](https://img.shields.io/badge/PRs-welcome-ff69b4.svg?style=flat-square)](https://github.com/starkware-libs/cairo/issues?q=is%3Aissue+is%3Aopen+label%3A%22help+wanted%22)

</div>

<details open="open">
<summary>Table of Contents</summary>

- [Report a Bug](#report-a-bug)
- [Request a Feature](#request-a-feature)
- [About](#about)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Compiling and running Cairo files](#compiling-and-running-cairo-files)
  - [Development](#development)
    - [Install the language server](#install-the-language-server)
- [Roadmap](#roadmap)
- [Support](#support)
- [Project assistance](#project-assistance)
- [Contributing](#contributing)
- [Authors \& contributors](#authors--contributors)
- [Security](#security)
- [License](#license)

</details>

---

## About

Cairo is the first Turing-complete language for creating provable programs for general computation.

## Getting Started

### Prerequisites

- Install [Rust](https://www.rust-lang.org/tools/install)
- Setup Rust:
```bash
rustup override set stable && rustup update && cargo test
```

### Compiling and running Cairo files

Compile Cairo to Sierra:
```bash
cargo run --bin cairo-compile -- /path/to/input.cairo /path/to/output.sierra --replace-ids
```

Compile Sierra to casm (Cairo assembly):
```bash
cargo run --bin sierra-compile -- /path/to/input.sierra /path/to/output.casm
```

Run Cairo code directly:
```bash
cargo run --bin cairo-run -- -p /path/to/file.cairo
```

See more information [here](./crates/cairo-runner/README.md). You can also find Cairo examples in the [examples](./examples) directory.

For running tests specifically, see here: [cairo-test](./crates/cairo-test-runner/README.md)

### Development

#### Install the language server

Follow the instructions in [vscode-cairo](./vscode-cairo/README.md).

## Roadmap

See the [open issues](https://github.com/starkware-libs/cairo/issues) for a list of proposed features (and known issues).

- [Top Feature Requests](https://github.com/starkware-libs/cairo/issues?q=label%3Aenhancement+is%3Aopen+sort%3Areactions-%2B1-desc) (Add your votes using the 👍 reaction)
- [Top Bugs](https://github.com/starkware-libs/cairo/issues?q=is%3Aissue+is%3Aopen+label%3Abug+sort%3Areactions-%2B1-desc) (Add your votes using the 👍 reaction)
- [Newest Bugs](https://github.com/starkware-libs/cairo/issues?q=is%3Aopen+is%3Aissue+label%3Abug)

## Support

- We encourage developers to ask and answer questions on [stackoverflow](https://stackoverflow.com/questions/tagged/cairo-lang).
- Contact options listed on [this GitHub profile](https://github.com/starkware-libs)

## Project assistance

If you want to say **thank you** or/and support active development of Cairo:

- Add a [GitHub Star](https://github.com/starkware-libs/cairo) to the project.
- Tweet about your Cairo work.
- Write interesting articles about the project on [Dev.to](https://dev.to/), [Medium](https://medium.com/) or your personal blog.

Together, we can make Cairo **better**!

## Contributing

First off, thanks for taking the time to contribute! Contributions are what make the open-source community such an amazing place to learn, inspire, and create. Any contributions you make will benefit everybody else and are **greatly appreciated**.

Please read [our contribution guidelines](docs/CONTRIBUTING.md), and thank you for being involved!

## Authors & contributors

For a full list of all authors and contributors, see [the contributors page](https://github.com/starkware-libs/cairo/contributors).

## Security

Cairo follows good practices of security, but 100% security cannot be assured.
Cairo is provided **"as is"** without any **warranty**. Use at your own risk.

_For more information and to report security issues, please refer to our [security documentation](docs/SECURITY.md)._

## License

This project is licensed under the **Apache 2.0**.

See [LICENSE](LICENSE) for more information.
