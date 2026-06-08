//! StarkWare staking contract benchmark scenario.
//!
//! The staking contract (https://github.com/starkware-libs/starknet-staking) is a large real-world
//! Starknet project, which makes it a useful memory stress test for the front end. It depends on
//! openzeppelin and starkware_utils, so it isn't a self-contained crate.
//!
//! The compiler can't read Scarb manifests, so it needs a `cairo_project.toml`. Rather than invoke
//! Scarb at benchmark time (it isn't available on CI, and we can't rely on a pre-populated Scarb
//! cache either), we generated one *once* locally from `scarb metadata` and checked it in as
//! `staking/cairo_project.toml.template`, with the per-repository source roots replaced by
//! `{STAKING_SRC}` / `{OZ_SRC}` / `{UTILS_SRC}` placeholders.
//!
//! At benchmark time [`prepare_staking`] needs only `git`: it shallow-clones all three pinned
//! sources (the contract plus its two dependency repos) and substitutes the placeholders with their
//! `src` directories to produce a usable project file. The dependency revisions match the staking
//! `Scarb.lock`, and the in-repo `src` trees were verified byte-identical to the published registry
//! sources, so this compiles exactly what Scarb would resolve. The whole checkout (clones + project
//! file) is removed when the returned [`StakingCheckout`] is dropped.
//!
//! The scenario is best-effort: if `git` is missing or the network is unavailable (e.g. on CI),
//! preparation logs why and yields `None` so the rest of the benchmark still runs.

use std::path::{Path, PathBuf};
use std::process::Command;

use super::BenchConfig;

const STAKING_REPO_URL: &str = "https://github.com/starkware-libs/starknet-staking";
/// Tag `@staking/contracts-v1.0.1-dev.978` on `main`. Pinned (rather than tracking a branch) so the
/// measured heap numbers stay comparable across runs.
const STAKING_REV: &str = "6467407bb81862b30ad44d6028351032a5c369b8";
const OPENZEPPELIN_REPO_URL: &str = "https://github.com/OpenZeppelin/cairo-contracts";
/// Tag `v2.0.0`.
const OPENZEPPELIN_REV: &str = "f1c15a30a44311166e0368081c5f79ab5a02c1d3";
const STARKWARE_UTILS_REPO_URL: &str = "https://github.com/starkware-libs/starkware-starknet-utils";
/// Matches the `starkware_utils` revision pinned in the staking `Scarb.lock`.
const STARKWARE_UTILS_REV: &str = "c19a9487eb7377a147a63189bc8dd4c126b61522";
/// The checked-in `cairo_project.toml`, generated once from `scarb metadata` (see module docs). Its
/// source roots are the `{STAKING_SRC}` / `{OZ_SRC}` / `{UTILS_SRC}` placeholders.
const STAKING_PROJECT_TEMPLATE: &str = include_str!("../staking/cairo_project.toml.template");

/// A throwaway checkout of the staking contract, prepared for benchmarking. The underlying
/// directory (source clones + generated `cairo_project.toml`) is deleted on drop, so all paths
/// created by [`prepare_staking`] are cleaned up even if the benchmark panics mid-run.
pub struct StakingCheckout {
    dir: PathBuf,
}

impl StakingCheckout {
    /// Builds the [`BenchConfig`] pointing at this checkout. Only the `staking` crate is compiled
    /// as the entry point; openzeppelin/starkware_utils are reached as dependencies.
    pub fn config(&self) -> BenchConfig {
        BenchConfig {
            name: "staking",
            path: self.dir.clone(),
            starknet: true,
            sierra_phases: false,
            main_crate: Some("staking"),
        }
    }
}

impl Drop for StakingCheckout {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.dir);
    }
}

/// Clones the pinned staking contract and its dependency repos under `dir`, then writes a
/// `cairo_project.toml` describing the staking-contract-only compilation. Returns `None` (after
/// cleaning up any partial checkout) if any step fails, so the scenario is simply skipped.
pub fn prepare_staking(dir: &Path) -> Option<StakingCheckout> {
    // Start from a clean slate: a leftover dir from an aborted run would break `git init`.
    let _ = std::fs::remove_dir_all(dir);

    let result = clone_sources(dir).and_then(|()| write_cairo_project(dir));
    match result {
        Ok(()) => Some(StakingCheckout { dir: dir.to_path_buf() }),
        Err(e) => {
            eprintln!("dhat: skipping staking scenario: {e}");
            let _ = std::fs::remove_dir_all(dir);
            None
        }
    }
}

/// Shallow-clones the contract and both dependency repos into fixed subdirectories of `dir`
/// (`staking`, `openzeppelin`, `starkware_utils`), matching the placeholders in the project
/// template.
fn clone_sources(dir: &Path) -> Result<(), String> {
    clone_repo(STAKING_REPO_URL, STAKING_REV, &dir.join("staking"))?;
    clone_repo(OPENZEPPELIN_REPO_URL, OPENZEPPELIN_REV, &dir.join("openzeppelin"))?;
    clone_repo(STARKWARE_UTILS_REPO_URL, STARKWARE_UTILS_REV, &dir.join("starkware_utils"))?;
    Ok(())
}

/// Shallow-fetches exactly `git_ref` (a commit SHA or tag) from `url` into `dest`. Fetching the
/// single revision directly (rather than `git clone`) avoids downloading unrelated history.
fn clone_repo(url: &str, git_ref: &str, dest: &Path) -> Result<(), String> {
    std::fs::create_dir_all(dest)
        .map_err(|e| format!("failed to create {}: {e}", dest.display()))?;
    // Initialize an empty repo in `dest`, fetch just the pinned revision, then check it out.
    let steps: [&[&str]; 3] = [
        &["init", "-q"],
        &["fetch", "-q", "--depth", "1", url, git_ref],
        &["-c", "advice.detachedHead=false", "checkout", "-q", "FETCH_HEAD"],
    ];
    for args in steps {
        let status = Command::new("git")
            .current_dir(dest)
            .args(args)
            .status()
            .map_err(|e| format!("failed to run git in {}: {e}", dest.display()))?;
        if !status.success() {
            return Err(format!("`git {}` failed in {}", args.join(" "), dest.display()));
        }
    }
    Ok(())
}

/// Substitutes the template's source-root placeholders with the cloned repositories' `src`
/// directories and writes `<dir>/cairo_project.toml`.
fn write_cairo_project(dir: &Path) -> Result<(), String> {
    let content = STAKING_PROJECT_TEMPLATE
        .replace("{STAKING_SRC}", &dir.join("staking").to_string_lossy())
        .replace("{OZ_SRC}", &dir.join("openzeppelin").to_string_lossy())
        .replace("{UTILS_SRC}", &dir.join("starkware_utils").to_string_lossy());
    let path = dir.join("cairo_project.toml");
    std::fs::write(&path, content).map_err(|e| format!("failed to write {}: {e}", path.display()))
}
