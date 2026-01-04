# Releasing

(1) Checkout the branch to be released. This will usually be `main` except in the event of a hotfix. For hotfixes, checkout the release branch you want to fix.

(2) Create a new release branch.

```sh
git checkout -b release-v0.8.0
```

(3) Create the release entry in [the changelog](CHANGELOG.md) with the contents of the _Unreleased_ section, which should be left empty.

(4) Push and open a PR targeting `main` to carefully review the release changes. This will trigger a Github workflow that automatically bumps the version number throughout the project.

```sh
git push release-v0.8.0
```

(5) Once merged, pull the changes from the release branch.
Then, create a tag on the release branch and push it to the main repository.

```sh
git pull
git tag v0.8.0
git push origin v0.8.0
```

(6) After that, go to the repo's [releases page](https://github.com/OpenZeppelin/cairo-contracts/releases/) and [create a new one](https://github.com/OpenZeppelin/cairo-contracts/releases/new) with the new tag and the base branch as target (`main` except in the event of a hotfix).
Make sure to write a detailed release description and a short changelog.

(7) Finally, from the released tag, create and push a doc branch to deploy the corresponding version
to the doc-site.

```sh
git checkout -b docs-v0.8.0
git push docs-v0.8.0
```
