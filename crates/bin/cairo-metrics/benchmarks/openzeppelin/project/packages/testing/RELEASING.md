# Releasing

(1) Checkout the branch to be released. This will usually be `main` except in the event of a hotfix. For hotfixes, checkout the release branch you want to fix.

(2) Create a new release branch (`openzeppelin_testing-v*.*.*`).

```sh
git checkout -b openzeppelin_testing-v1.0.0
```

(3) Create the release entry in [the changelog](./CHANGELOG.md) with the contents of the _Unreleased_ section, which should be left empty.

(4) Push and open a PR targeting `main` to carefully review the release changes. This will trigger a Github workflow that automatically bumps the version number of the package and update the docs.

```sh
git push openzeppelin_testing-v1.0.0
```

(5) After getting the PR merged, the release is completed. Make sure not to delete the branch, since there's no extra
tag pegged to the release.
