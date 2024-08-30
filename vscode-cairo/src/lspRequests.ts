import * as lc from "vscode-languageclient/node";

export type ExpandMacroRequest = {
  textDocument: { uri: string };
  position: { line: number; character: number };
};
export type ExpandMacroResponse = string | null;
export const expandMacro = new lc.RequestType<ExpandMacroRequest, ExpandMacroResponse, void>(
  "cairo/expandMacro",
);

export type ProvideVirtualFileRequest = {
  uri: string;
};
export type ProvideVirtualFileResponse = {
  content?: string;
};
export const vfsProvide = new lc.RequestType<
  ProvideVirtualFileRequest,
  ProvideVirtualFileResponse,
  void
>("vfs/provide");

export type ViewAnalyzedCratesResponse = string;
export const viewAnalyzedCrates = new lc.RequestType0<ViewAnalyzedCratesResponse, void>(
  "cairo/viewAnalyzedCrates",
);
