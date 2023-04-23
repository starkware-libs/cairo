#!/usr/bin/env node
// @ts-check

const { stdin } = process;
const core = require("@actions/core");

// From https://github.com/sindresorhus/get-stdin/blob/main/index.js
async function getStdin() {
  let result = "";

  if (stdin.isTTY) {
    return result;
  }

  stdin.setEncoding("utf8");

  for await (const chunk of stdin) {
    result += chunk;
  }

  return result;
}

async function main() {
  const stdinData = await getStdin();
  console.log("stdin:", stdinData);

  /**
   * @type string[]
   **/
  const filesChanged = JSON.parse(stdinData);
  console.log("filesChanged:", filesChanged);

  const generalDeps = [];
  const modifiedDeps = [];
  const jobsToRun = [];

  get_general_modified_deps(filesChanged, generalDeps);
  console.log("joined generalDeps:", generalDeps.join());
  core.setOutput("deps", generalDeps.join());

  add_all_modified_deps(filesChanged, modifiedDeps);
  add_all_jobs_to_run(modifiedDeps, jobsToRun);
  console.log("joined jobsToRun:", jobsToRun.join());
  core.setOutput("jobs", jobsToRun.join());
}

// Adds all modified dependencies to the modifiedDeps array, according to the changed files.
async function add_all_modified_deps(filesChanged, modifiedDeps) {
  maybe_add_modified_dep(filesChanged, "casm", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "compiler", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "debug", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "defs", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "diagnostics", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "eq-solver", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "filesystem", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "formatter", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "language-server", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "lowering", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "parser", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "plugins", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "proc_macros", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "project", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "runner", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "semantic", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "sierra", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "sierra-ap-change", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "sierra-gas", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "sierra-generator", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "sierra-to-casm", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "starknet", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "syntax", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "syntax-codegen", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "test-runner", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "test-utils", modifiedDeps);
  maybe_add_modified_dep(filesChanged, "utils", modifiedDeps);

  console.log("modifiedDeps:", modifiedDeps);
}

// Adds general modified dependencies, according to the changed files. General dependencies are
// dependencies that are not specific to a crate. These dependencies will be exported using the
// "deps" output.
async function get_general_modified_deps(filesChanged, generalDeps) {
  if (
    filesChanged.findIndex((fileChanged) => fileChanged.endsWith(".rs")) != -1
  ) {
    generalDeps.push("--rust--");
  }

  if (
    filesChanged.findIndex((fileChanged) => fileChanged.endsWith(".cairo")) !=
    -1
  ) {
    generalDeps.push("--cairo--");
  }

  if (
    filesChanged.findIndex((fileChanged) =>
      fileChanged.endsWith("Cargo.toml")
    ) != -1
  ) {
    generalDeps.push("--cargo-toml--");
  }

  if (
    filesChanged.findIndex((fileChanged) =>
      fileChanged.startsWith("corelib/")
    ) != -1
  ) {
    generalDeps.push("--corelib--");
  }

  if (
    filesChanged.findIndex((fileChanged) =>
      fileChanged.startsWith("tests/bug_samples/")
    ) != -1
  ) {
    generalDeps.push("--bug-samples--");
  }

  console.log("generalDeps:", generalDeps);
}

// Adds all jobs to run, according to the modified dependencies. This is done by defining which
// dependencies should make each job run. These jobs will be exported using the "jobs" output.
async function add_all_jobs_to_run(modifiedDeps, jobsToRun) {
  add_job_to_run("casm", modifiedDeps, ["utils"], jobsToRun);
  add_job_to_run(
    "compiler",
    modifiedDeps,
    [
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "parser",
      "plugins",
      "project",
      "semantic",
      "sierra",
      "sierra-generator",
      "syntax",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run("debug", modifiedDeps, ["proc-macros", "utils"], jobsToRun);
  add_job_to_run(
    "defs",
    modifiedDeps,
    [
      "debug",
      "diagnostics",
      "filesystem",
      "parser",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "diagnostics",
    modifiedDeps,
    ["filesystem", "proc-macros", "utils"],
    jobsToRun
  );
  add_job_to_run("eq-solver", modifiedDeps, ["utils"], jobsToRun);
  add_job_to_run("filesystem", modifiedDeps, ["debug", "utils"], jobsToRun);
  add_job_to_run(
    "formatter",
    modifiedDeps,
    ["diagnostics", "filesystem", "parser", "syntax", "utils"],
    jobsToRun
  );
  add_job_to_run(
    "language-server",
    modifiedDeps,
    [
      "compiler",
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "formatter",
      "lowering",
      "parser",
      "plugins",
      "project",
      "semantic",
      "starknet",
      "syntax",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "lowering",
    modifiedDeps,
    [
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "parser",
      "plugins",
      "proc-macros",
      "semantic",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "parser",
    modifiedDeps,
    [
      "diagnostics",
      "filesystem",
      "syntax",
      "syntax-codegen",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "plugins",
    modifiedDeps,
    [
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "parser",
      "semantic",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run("proc-macros", modifiedDeps, ["debug"], jobsToRun);
  add_job_to_run("project", modifiedDeps, ["filesystem"], jobsToRun);
  add_job_to_run(
    "runner",
    modifiedDeps,
    [
      "casm",
      "compiler",
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "semantic",
      "sierra",
      "sierra-ap-change",
      "sierra-gas",
      "sierra-generator",
      "sierra-to-casm",
      "starknet",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "semantic",
    modifiedDeps,
    [
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "parser",
      "plugins",
      "proc-macros",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run("sierra", modifiedDeps, ["utils"], jobsToRun);
  add_job_to_run(
    "sierra-ap-change",
    modifiedDeps,
    ["eq-solver", "sierra", "utils"],
    jobsToRun
  );
  add_job_to_run(
    "sierra-gas",
    modifiedDeps,
    ["eq-solver", "sierra", "test-utils", "utils"],
    jobsToRun
  );
  add_job_to_run(
    "sierra-generator",
    modifiedDeps,
    [
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "parser",
      "plugins",
      "proc-macros",
      "semantic",
      "sierra",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "sierra-to-casm",
    modifiedDeps,
    ["casm", "sierra", "sierra-ap-change", "sierra-gas", "utils"],
    jobsToRun
  );
  add_job_to_run(
    "starknet",
    modifiedDeps,
    [
      "casm",
      "compiler",
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "parser",
      "plugins",
      "semantic",
      "sierra",
      "sierra-ap-change",
      "sierra-gas",
      "sierra-generator",
      "sierra-to-casm",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run(
    "syntax",
    modifiedDeps,
    ["debug", "filesystem", "utils"],
    jobsToRun
  );
  add_job_to_run("syntax-codegen", modifiedDeps, ["utils"], jobsToRun);
  add_job_to_run(
    "test-runner",
    modifiedDeps,
    [
      "casm",
      "compiler",
      "debug",
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "plugins",
      "project",
      "runner",
      "semantic",
      "sierra",
      "sierra-generator",
      "sierra-to-casm",
      "starknet",
      "syntax",
      "utils",
    ],
    jobsToRun
  );
  add_job_to_run("utils", modifiedDeps, [], jobsToRun);
  add_job_to_run(
    "tests",
    modifiedDeps,
    [
      "casm",
      "compiler",
      "defs",
      "diagnostics",
      "filesystem",
      "lowering",
      "parser",
      "plugins",
      "runner",
      "semantic",
      "sierra",
      "sierra-gas",
      "sierra-generator",
      "sierra-to-casm",
      "syntax",
      "test-utils",
      "utils",
    ],
    jobsToRun
  );

  console.log("jobsToRun:", jobsToRun);
}

// Adds a dependency to the list of modified dependencies if any of the crate files was modified.
async function maybe_add_modified_dep(filesChanged, crate_name, modifiedDeps) {
  if (
    filesChanged.findIndex(
      (fileChanged) =>
        fileChanged.startsWith("crates/cairo-lang-" + crate_name + "/") &&
        !fileChanged.includes("test")
    ) != -1
  ) {
    modifiedDeps.push("--" + crate_name + "--");
  } else if (
    filesChanged.findIndex((fileChanged) =>
      fileChanged.startsWith("crates/cairo-lang-" + crate_name + "/")
    ) != -1
  ) {
    modifiedDeps.push("--" + crate_name + "-test-only--");
  }
}

// Adds a job to the list of jobs to run if any of its dependencies were modified.
async function add_job_to_run(job_name, modifiedDeps, deps, jobsToRun) {
  if (
    deps.findIndex((dep) => modifiedDeps.includes("--" + dep + "--")) != -1 ||
    modifiedDeps.includes("--" + job_name + "--") ||
    modifiedDeps.includes("--" + job_name + "-test-only--")
  ) {
    jobsToRun.push("--test-" + job_name + "--");
  }
}

main().then(function () {
  console.log("Done");
});
