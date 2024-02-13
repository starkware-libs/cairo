import * as fs from "fs";
import * as os from "os";
import * as path from "path";

/**
 * Returns the full path to the tool if it is executable, otherwise `undefined`.
 */
export async function checkTool(fullPath: string): Promise<string | undefined> {
  if (await isExecutable(fullPath)) {
    return fullPath;
  } else {
    return undefined;
  }
}

/**
 * Returns the full path to the tool if it is executable, trying to add known
 * program file extensions; otherwise, returns `undefined`.
 */
export async function findToolAtWithExtension(
  name: string,
): Promise<string | undefined> {
  const envExt = process.env["PATHEXT"] || "";
  const extensions = envExt.split(";");

  for (const ext of extensions) {
    const tool = await checkTool(name + ext);
    if (tool != undefined) {
      return tool;
    }
  }

  return undefined;
}

export async function findToolInPath(
  name: string,
): Promise<string | undefined> {
  const envPath = process.env["PATH"] || "";
  const pathDirs = envPath
    .replace(/"+/g, "")
    .split(path.delimiter)
    .filter(Boolean);

  for (const d of pathDirs) {
    const tool = await findToolAtWithExtension(path.join(d, name));
    if (tool != undefined) {
      return tool;
    }
  }

  return undefined;
}

export async function findToolInAsdf(
  name: string,
): Promise<string | undefined> {
  if (os.platform() === "win32") {
    return undefined;
  }

  const asdfDataDir =
    process.env["ASDF_DATA_DIR"] || path.join(os.homedir(), ".asdf");

  const shim = path.join(asdfDataDir, "shims", name);

  return await findToolAtWithExtension(shim);
}

async function isExecutable(path: string): Promise<boolean> {
  try {
    await fs.promises.access(path, fs.constants.X_OK);
    return true;
  } catch (e) {
    return false;
  }
}
