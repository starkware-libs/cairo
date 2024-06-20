import assert from "assert/strict";
import * as fs from "node:fs/promises";

/**
 * JavaScript reimplementation of `cairo_lang_test_utils::parse_test_file::parse_test_file`.
 *
 * @param {string} testFilePath
 * @returns {Promise<{ test_name: string, test_runner_name?: string, cairo_code?: string }[]>}
 */
export async function parseTestFile(testFilePath) {
  const testFileText = await fs.readFile(testFilePath, "utf8");
  const tests = [];

  const testTexts = testFileText.split(/\/\/! > =+\n/g);
  assert.ok(testTexts.length > 0, "no tests found in test file");
  for (const testText of testTexts) {
    const sections = Array.from(testText.matchAll(/\/\/! > (?<name>[^\n]+)\n(?<text>.*)/g));
    assert.ok(sections.length > 0, "no sections found in test");

    const obj = {
      test_name: sections.shift().groups.name,
    };

    for (const m of sections) {
      obj[m.groups.name] = m.groups.text;
    }

    tests.push(obj);
  }

  return tests;
}
