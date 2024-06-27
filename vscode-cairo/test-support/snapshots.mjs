import { createHash } from "node:crypto";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { fileURLToPath } from "node:url";

const isFixMode = process.env.CAIRO_FIX_TESTS === "1";

export const snap = isFixMode ? fix : read;

async function read(key) {
  const snapshotPath = getSnapshotPath(key);
  return await fs.readFile(snapshotPath, "utf8");
}

async function fix(key, text) {
  const snapshotPath = getSnapshotPath(key);
  await fs.mkdir(path.dirname(snapshotPath), { recursive: true });
  await fs.writeFile(snapshotPath, text, "utf8");
  return text;
}

function getSnapshotPath(key) {
  const hash = createHash("sha256").update(key).digest("hex").slice(0, 10);
  return path.join(fileURLToPath(import.meta.url), "..", "..", "test", "snapshots", `${hash}.txt`);
}
