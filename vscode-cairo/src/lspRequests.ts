import * as lc from "vscode-languageclient/node";

export type ExpandMacroRequest = {
  textDocument: { uri: string };
  position: { line: number; character: number };
};
export type ExpandMacroReponse = string | null;
export const expandMacro = new lc.RequestType<ExpandMacroRequest, ExpandMacroReponse, void>(
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
