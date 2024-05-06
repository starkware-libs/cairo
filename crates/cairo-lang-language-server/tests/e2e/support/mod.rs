use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::time::Duration;
use std::{fs, future};

use assert_fs::prelude::*;
use assert_fs::TempDir;
use cairo_lang_language_server::build_service_for_e2e_tests;
use futures::channel::mpsc;
use futures::{join, stream, FutureExt, SinkExt, StreamExt, TryFutureExt};
use lsp_types::{lsp_notification, lsp_request, Url};
use serde_json::Value;
use tokio::time::error::Elapsed;
use tokio::time::timeout;
use tower_lsp::jsonrpc::Id;
use tower_lsp::{lsp_types, ClientSocket, LanguageServer, LspService};
use tower_service::Service;

use self::runtime::GuardedRuntime;
use crate::support::jsonrpc::{Message, RequestIdGenerator};
use crate::support::runtime::AbortOnDrop;

mod jsonrpc;
mod runtime;

macro_rules! sandbox {
    (
        $(files { $($file:expr => $content:expr),* $(,)? })?
    ) => {{
        let mut w = $crate::support::Sandbox::new();

        $($(w.file($file, $content);)*)?

        w.server()
    }};
}

pub(crate) use sandbox;

pub struct Sandbox {
    t: TempDir,
    fixture_files: Vec<PathBuf>,
}

impl Sandbox {
    pub fn new() -> Self {
        let t = TempDir::new().unwrap();
        Self { t, fixture_files: Vec::new() }
    }

    #[must_use]
    pub fn server(self) -> Server {
        Server::start(self)
    }
}

/// Builder methods.
impl Sandbox {
    pub fn file(&mut self, path: impl AsRef<Path>, source: impl AsRef<str>) {
        self.fixture_files.push(path.as_ref().to_owned());
        self.t.child(path).write_str(source.as_ref().trim()).unwrap();
    }
}

/// Introspection methods.
impl Sandbox {
    fn get_absolute_path(&self, path: impl AsRef<Path>) -> PathBuf {
        self.t.child(path).path().to_owned()
    }

    fn get_file_url(&self, path: impl AsRef<Path>) -> Url {
        Url::from_file_path(self.get_absolute_path(path)).unwrap()
    }

    fn get_file_contents(&self, path: impl AsRef<Path>) -> String {
        fs::read_to_string(self.get_absolute_path(path)).unwrap()
    }
}

pub struct Server {
    sandbox: Sandbox,
    rt: GuardedRuntime,
    req_id: RequestIdGenerator,
    input_tx: mpsc::Sender<Message>,
    output: ServerOutput,
    _main_loop: AbortOnDrop,
}

impl Server {
    fn start(sandbox: Sandbox) -> Self {
        let rt = GuardedRuntime::start();
        let (service, loopback) = build_service_for_e2e_tests();

        let (requests_tx, requests_rx) = mpsc::channel(0);
        let (responses_tx, responses_rx) = mpsc::channel(0);

        let main_loop = rt
            .spawn(Self::serve(service, loopback, requests_rx, responses_tx.clone()))
            .abort_handle()
            .into();

        let mut this = Self {
            sandbox,
            rt,
            req_id: RequestIdGenerator::default(),
            input_tx: requests_tx,
            output: ServerOutput::new(responses_rx),
            _main_loop: main_loop,
        };

        this.initialize();

        this
    }

    /// Copy-paste of [`tower_lsp::Server::serve`].
    async fn serve(
        mut service: LspService<impl LanguageServer>,
        loopback: ClientSocket,
        mut requests_rx: mpsc::Receiver<Message>,
        mut responses_tx: mpsc::Sender<Message>,
    ) {
        let (client_requests, mut client_responses) = loopback.split();
        let (client_requests, client_abort) = stream::abortable(client_requests);
        let (mut server_tasks_tx, server_tasks_rx) = mpsc::channel(100);

        let process_server_tasks = server_tasks_rx
            .buffer_unordered(4)
            .filter_map(future::ready)
            .map(Message::Response)
            .map(Ok)
            .forward(responses_tx.clone().sink_map_err(|_| unreachable!()))
            .map(|_| ());

        let print_output = client_requests
            .map(Message::Request)
            .map(Ok)
            .forward(responses_tx.clone().sink_map_err(|_| unreachable!()))
            .map(|_| ());

        let read_input = async {
            while let Some(msg) = requests_rx.next().await {
                match msg {
                    Message::Request(req) => {
                        if let Err(err) = future::poll_fn(|cx| service.poll_ready(cx)).await {
                            eprintln!("{err:?}");
                            break;
                        }

                        let fut = service.call(req).unwrap_or_else(|err| {
                            eprintln!("{err:?}");
                            None
                        });

                        server_tasks_tx.send(fut).await.unwrap()
                    }
                    Message::Response(res) => {
                        if let Err(err) = client_responses.send(res).await {
                            eprintln!("{err:?}");
                            break;
                        }
                    }
                }
            }

            server_tasks_tx.disconnect();
            responses_tx.disconnect();
            client_abort.abort();
        };

        join!(print_output, read_input, process_server_tasks);
    }

    fn initialize(&mut self) {
        // Blindly copied from Rust Analyzer:
        // https://github.com/rust-lang/rust-analyzer/blob/a2ed6837bc45f13ecd72553dd0aaa16dc7d5dd87/crates/rust-analyzer/tests/slow-tests/support.rs#L129-L184
        let capabilities = lsp_types::ClientCapabilities {
            workspace: Some(lsp_types::WorkspaceClientCapabilities {
                did_change_watched_files: Some(
                    lsp_types::DidChangeWatchedFilesClientCapabilities {
                        dynamic_registration: Some(true),
                        relative_pattern_support: None,
                    },
                ),
                workspace_edit: Some(lsp_types::WorkspaceEditClientCapabilities {
                    resource_operations: Some(vec![
                        lsp_types::ResourceOperationKind::Create,
                        lsp_types::ResourceOperationKind::Delete,
                        lsp_types::ResourceOperationKind::Rename,
                    ]),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            text_document: Some(lsp_types::TextDocumentClientCapabilities {
                definition: Some(lsp_types::GotoCapability {
                    link_support: Some(true),
                    ..Default::default()
                }),
                code_action: Some(lsp_types::CodeActionClientCapabilities {
                    code_action_literal_support: Some(
                        lsp_types::CodeActionLiteralSupport::default(),
                    ),
                    ..Default::default()
                }),
                hover: Some(lsp_types::HoverClientCapabilities {
                    content_format: Some(vec![lsp_types::MarkupKind::Markdown]),
                    ..Default::default()
                }),
                inlay_hint: Some(lsp_types::InlayHintClientCapabilities {
                    resolve_support: Some(lsp_types::InlayHintResolveClientCapabilities {
                        properties: vec![
                            "textEdits".to_owned(),
                            "tooltip".to_owned(),
                            "label.tooltip".to_owned(),
                            "label.location".to_owned(),
                            "label.command".to_owned(),
                        ],
                    }),
                    ..Default::default()
                }),
                ..Default::default()
            }),
            window: Some(lsp_types::WindowClientCapabilities {
                work_done_progress: Some(false),
                ..Default::default()
            }),
            ..Default::default()
        };

        let workspace_folders = Some(vec![lsp_types::WorkspaceFolder {
            uri: Url::from_directory_path(self.sandbox.t.path()).unwrap(),
            name: "test_root".to_string(),
        }]);

        self.send_request::<lsp_request!("initialize")>(lsp_types::InitializeParams {
            capabilities,
            workspace_folders,
            client_info: Some(lsp_types::ClientInfo {
                name: "e2e".to_string(),
                version: Some("1.0.0".to_string()),
            }),
            locale: Some("en".to_string()),
            ..lsp_types::InitializeParams::default()
        });

        self.send_notification::<lsp_notification!("initialized")>(lsp_types::InitializedParams {});
    }

    /// Sends a typed request to the server.
    pub fn send_request<R: lsp_types::request::Request>(&mut self, params: R::Params) -> R::Result {
        let params = serde_json::to_value(params).expect("failed to serialize request params");
        let result = self.send_request_untyped(R::METHOD, params);
        serde_json::from_value(result).expect("failed to parse response")
    }

    /// Sends a custom request to the server.
    pub fn send_request_untyped(&mut self, method: &'static str, params: Value) -> Value {
        let id = self.req_id.next();
        let message = Message::request(method, id.clone(), params);
        self.rt.block_on(async {
            self.input_tx.send(message.clone()).await.expect("failed to send request");

            while let Some(response_message) =
                self.output.recv().await.unwrap_or_else(|_| panic!("timeout: {message:?}"))
            {
                match response_message {
                    Message::Request(req) => {
                        panic!("unexpected request: {:?}", req)
                    }
                    Message::Response(res) if res.id() == &Id::Null => {
                        // This looks like a notification, skip it.
                    }
                    Message::Response(res) => {
                        let (res_id, result) = res.into_parts();
                        assert_eq!(res_id, id);
                        match result {
                            Ok(result) => return result,
                            Err(err) => panic!("error response: {:#?}", err),
                        }
                    }
                }
            }

            panic!("no response for request: {message:?}")
        })
    }

    /// Sends a typed notification to the server.
    pub fn send_notification<N: lsp_types::notification::Notification>(
        &mut self,
        params: N::Params,
    ) {
        let params = serde_json::to_value(params).expect("failed to serialize notification params");
        self.send_notification_untyped(N::METHOD, params)
    }

    /// Sends a custom notification to the server.
    pub fn send_notification_untyped(&mut self, method: &'static str, params: Value) {
        let message = Message::notification(method, params);
        self.rt.block_on(async {
            self.input_tx.send(message).await.expect("failed to send notification");
        })
    }
}

/// Quality of life helpers for interacting with the server.
impl Server {
    /// Returns a `TextDocumentIdentifier` for the given file.
    pub fn doc_id(&self, path: impl AsRef<Path>) -> lsp_types::TextDocumentIdentifier {
        lsp_types::TextDocumentIdentifier { uri: self.sandbox.get_file_url(path) }
    }

    /// Sends `textDocument/didOpen` notification to the server and waits for its completion.
    pub fn open(&mut self, path: impl AsRef<Path>) {
        // Poor man's attempt at guessing the language ID
        // by assuming that file extension represents it.
        let language_id = self
            .sandbox
            .get_absolute_path(&path)
            .extension()
            .and_then(OsStr::to_str)
            .unwrap_or_default()
            .to_string();

        self.send_notification::<lsp_notification!("textDocument/didOpen")>(
            lsp_types::DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri: self.sandbox.get_file_url(&path),
                    language_id,
                    version: 0,
                    text: self.sandbox.get_file_contents(&path),
                },
            },
        )
    }
}

struct ServerOutput {
    output_rx: mpsc::Receiver<Message>,
    trace: Vec<Message>,
}

impl ServerOutput {
    fn new(output_rx: mpsc::Receiver<Message>) -> Self {
        Self { output_rx, trace: Vec::new() }
    }

    async fn recv(&mut self) -> Result<Option<Message>, Elapsed> {
        const TIMEOUT: Duration = Duration::from_secs(2 * 60);

        let r = timeout(TIMEOUT, self.output_rx.next()).await;
        if let Ok(Some(msg)) = &r {
            self.trace.push(msg.clone());
        }
        r
    }
}
