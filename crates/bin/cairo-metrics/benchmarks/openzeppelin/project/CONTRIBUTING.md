# Contributing to OpenZeppelin Contracts for Cairo

We really appreciate and value contributions to OpenZeppelin Contracts for Cairo. Please take 5' to review the items listed below to make sure that your contributions are merged as soon as possible.

## Contribution guidelines

Before starting development, please [create an issue](https://github.com/OpenZeppelin/cairo-contracts/issues/new/choose) to open the discussion, validate that the PR is wanted, and coordinate overall implementation details.

### Coding style

After a few radical changes in the Cairo language (mainly the transition to Cairo 1), our coding style guidelines became automatically deprecated.
That's why [we're working on setting new ones](https://github.com/OpenZeppelin/cairo-contracts/issues/696).
Feel free to read, contribute, discuss, and ask questions in the issue.

## Creating Pull Requests (PRs)

As a contributor, you are expected to fork this repository, work on your own fork and then submit pull requests. The pull requests will be reviewed and eventually merged into the main repo. See ["Fork-a-Repo"](https://help.github.com/articles/fork-a-repo/) for how this works.

## A typical workflow

1. Make sure your fork is up to date with the main repository:

    ```sh
    cd cairo-contracts
    git remote add upstream https://github.com/OpenZeppelin/cairo-contracts.git
    git fetch upstream
    git pull --rebase upstream main
    ```

    > NOTE: The directory `cairo-contracts` represents your fork's local copy.

2. Branch out from `main` into `fix/some-bug-short-description-#123` (ex: `fix/typos-in-docs-#123`):

    (Postfixing #123 will associate your PR with the issue #123 and make everyone's life easier =D)

    ```sh
    git checkout -b fix/some-bug-short-description-#123
    ```

3. Make your changes, add your files, and [update the documentation](#documentation). Make sure to update the [CHANGELOG](CHANGELOG.md) (*[learn how](https://keepachangelog.com/en/1.1.0/)*).

4. Commit and push to your fork.

    ```sh
    git add src/file.cairo
    git commit -m "Fix some bug short description #123"
    git push origin fix/some-bug-short-description-#123
    ```

5. Run tests and linter. This can be done by running local continuous integration and make sure it passes.

    ```bash
    # run tests
    snforge test -w

    # run linter
    scarb fmt -w --check
    ```

6. Go to [OpenZeppelin/cairo-contracts](https://github.com/OpenZeppelin/cairo-contracts) in your web browser and issue a new pull request.
    Begin the body of the PR with "Fixes #123" or "Resolves #123" to link the PR to the issue that it is resolving.
    *IMPORTANT* Read the PR template very carefully and make sure to follow all the instructions. These instructions
    refer to some very important conditions that your PR must meet in order to be accepted, such as making sure that all PR checks pass.

7. Maintainers will review your code and possibly ask for changes before your code is pulled in to the main repository. We'll check that all tests pass, review the coding style, and check for general code correctness. If everything is OK, we'll merge your pull request and your code will be part of OpenZeppelin Contracts for Cairo.

    *IMPORTANT* Please pay attention to the maintainer's feedback, since it's a necessary step to keep up with the standards OpenZeppelin Contracts attains to.

## Documentation

Before submitting the PR, you must update the corresponding documentation entries in the docs folder. In the future we may use something similar to solidity-docgen to automatically generate docs, but for now we are updating .adoc entries manually.

NOTE: When the scarb version is bumped, the *Overview* page *Installation* section must be updated accordingly.

If you want to run the documentation UI locally:

1. Change directory into docs inside the project and run npm install.

    ```bash
    cd docs && npm i
    ```

2. Build the docs and run the local server (default to localhost:8080). This will watch for changes in the docs/module folder, and update the UI accordingly.

    ```bash
    npm run docs:watch
    ```

## Class hashes

Every time there's a language bump or a change in a preset or component used by one, new class hashes should be checked and updated in the presets doc page.

## Integration tests

Currently, Starknet's test suite has important differences with public networks. We strongly suggest testing new features against a testnet before submitting the PR, to make sure that everything works as expected in a real environment.

We are looking into defining a better process for these integration tests, but in the meantime the PR author/contributor must test the feature in a live network and then share a link to a reviewable transaction.
Ideally, the transaction should come from a verified contract whose execution can be traced, for the reviewer to evaluate and possibly reproduce.

## All set

If you have any questions, feel free to post them as an [issue](https://github.com/OpenZeppelin/cairo-contracts/issues).

Finally, if you're looking to collaborate and want to find easy tasks to start, look at the issues we marked as ["Good first issue"](https://github.com/OpenZeppelin/cairo-contracts/labels/good%20first%20issue).

Thanks for your time and code!
