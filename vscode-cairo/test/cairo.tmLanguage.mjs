import assert from "assert/strict";
import test from "node:test";
import corpus from "../test-support/cairo-corpus.gen.mjs";
import { highlightCairoCode } from "../test-support/grammar.mjs";
import { snap } from "../test-support/snapshots.mjs";

test("highlighting cairo-lang-parser tests", async (t) => {
  for (const code of corpus) {
    const testName = code.replace(/\s+/g, " ").slice(0, 20);

    await t.test(testName, async () => {
      const syntax = await highlightCairoCode(code);
      const actual = `\
//! > cairo_code
${code}

//! > syntax
${syntax}
`;
      const snapshot = await snap(code, actual);
      await assert.equal(actual, snapshot);
    });
  }
});
