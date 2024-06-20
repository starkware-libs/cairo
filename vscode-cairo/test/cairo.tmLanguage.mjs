import assert from "assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import test from "node:test";
import { fileURLToPath } from "node:url";
import { highlightCairoCode } from "./support/grammar.mjs";
import { snap } from "./support/snapshots.mjs";
import { parseTestFile } from "./support/testFile.mjs";

const cairoLangParserRoot = path.join(
  fileURLToPath(import.meta.url),
  "..",
  "..",
  "..",
  "crates",
  "cairo-lang-parser",
);

const runners = {
  get_diagnostics: cairoCodeTestRunner,
  test_colored_parsed_code: cairoCodeAsFilePathTestRunner,
  test_full_parser_tree: cairoCodeAsFilePathTestRunner,
  test_partial_parser_tree: cairoCodeTestRunner,
  test_partial_parser_tree_with_trivia: cairoCodeTestRunner,
};

test("highlighting cairo-lang-parser tests", async (t) => {
  const parserTestDataRoot = path.join(cairoLangParserRoot, "src", "parser_test_data");
  assert.ok((await fs.stat(parserTestDataRoot)).isDirectory());

  const testedCode = new Set();

  for await (const testFile of fs.glob(path.join(parserTestDataRoot, "**", "*"))) {
    if (!(await fs.stat(testFile)).isFile()) {
      continue;
    }

    if (path.extname(testFile) === ".cairo") {
      /// Skip cairo files as they will be read by `test_full_parser_tree` tests.
      continue;
    }

    const testFileName = path.relative(parserTestDataRoot, testFile);
    for (const test of await parseTestFile(testFile)) {
      const runner = getRunner(test);
      const code = await runner(test);

      /// Do not test the same code snippet twice.
      if (testedCode.has(code)) {
        continue;
      }
      testedCode.add(code);

      await t.test(path.join(testFileName, test.test_name), async () => {
        await testCairoCode(code);
      });
    }
  }
});

/**
 * Finds a matching test runner for the given test, or fail the test if no runner is found.
 */
function getRunner({ test_runner_name }) {
  assert.ok(typeof test_runner_name === "string", "test is missing test_runner_name section");
  const name = /^(?<name>[a-zA-Z0-9_]+)/.exec(test_runner_name)?.groups?.name;
  assert.ok(name, `invalid test_runner_name: ${test_runner_name}`);

  if (name in runners) {
    return runners[name];
  } else {
    throw new Error(
      `Unknown test runner name: ${name}. ` +
        `Please add it to the '${nameOf(() => runners)}' array in ${fileURLToPath(import.meta.url)}.`,
    );
  }
}

/**
 * Extracts cairo code for highlighting test,
 * when test has a `cairo_code` section of the following form:
 *
 * ```
 * //! > cairo_code
 * some cairo code
 * ```
 */
async function cairoCodeTestRunner({ cairo_code }) {
  return cairo_code;
}

/**
 * Extracts cairo code for highlighting test,
 * when test has a `cairo_code` section of the following form:
 *
 * ```
 * //! > cairo_code
 * >>> file: src/parser_test_data/cairo_test_files/test1.cairo
 * ```
 */
async function cairoCodeAsFilePathTestRunner({ cairo_code }) {
  const relPath = /^>>> file: (?<path>.+)/.exec(cairo_code)?.groups?.path;
  assert.ok(relPath, "invalid cairo_code section");
  const fullPath = path.join(cairoLangParserRoot, relPath);
  return await fs.readFile(fullPath, "utf8");
}

/**
 * Actual test logic.
 */
async function testCairoCode(code) {
  const syntax = await highlightCairoCode(code);
  const actual = `\
//! > cairo_code
${code}

//! > syntax
${syntax}
`;
  const snapshot = await snap(code, actual);
  assert.equal(actual, snapshot);
}

/**
 * Stringifies function body.
 *
 * This helper is useful for getting the name of a variable in a way that survives refactorings.
 * Source: https://stackoverflow.com/a/66935761
 */
function nameOf(f) {
  return f.toString().replace(/[ |()=>]/g, "");
}
