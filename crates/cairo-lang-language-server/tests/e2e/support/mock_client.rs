use std::ffi::OsStr;
use std::path::Path;
use std::time::Duration;
use std::{future, process};

use cairo_lang_language_server::build_service_for_e2e_tests;
use futures::channel::mpsc;
use futures::{join, stream, FutureExt, SinkExt, StreamExt, TryFutureExt};
use lsp_types::{lsp_notification, lsp_request};
use serde_json::Value;
use tokio::time::error::Elapsed;
use tokio::time::timeout;
use tower_lsp::{lsp_types, ClientSocket, LanguageServer, LspService};
use tower_service::Service;

use crate::support::fixture::Fixture;
use crate::support::jsonrpc::{Message, RequestIdGenerator};
use crate::support::runtime::{AbortOnDrop, GuardedRuntime};

/// A mock language client implementation that facilitates end-to-end testing language servers.
///
/// ## Termination
///
/// The language server is terminated abruptly upon dropping of this struct.
/// The `shutdown` request and `exit` notifications are not sent at all.
/// Instead, the Tokio Runtime that is executing the server is being shut down and any running
/// blocking tasks are given a small period of time to complete.
pub struct MockClient {
    fixture: Fixture,
    rt: GuardedRuntime,
    req_id: RequestIdGenerator,
    input_tx: mpsc::Sender<Message>,
    output: ServerOutput,
    _main_loop: AbortOnDrop,
}

impl MockClient {
    /// Starts and initializes CairoLS in context of a given fixture and given client capabilities.
    ///
    /// Upon completion of this function the language server will be in the _initialized_ state
    /// (i.e. the `initialize` request and `initialized` notification both will be completed).
    #[must_use]
    pub fn start(fixture: Fixture, capabilities: lsp_types::ClientCapabilities) -> Self {
        let rt = GuardedRuntime::start();
        let (service, loopback) = build_service_for_e2e_tests();

        let (requests_tx, requests_rx) = mpsc::channel(0);
        let (responses_tx, responses_rx) = mpsc::channel(0);

        let main_loop = rt
            .spawn(Self::serve(service, loopback, requests_rx, responses_tx.clone()))
            .abort_handle()
            .into();

        let mut this = Self {
            fixture,
            rt,
            req_id: RequestIdGenerator::default(),
            input_tx: requests_tx,
            output: ServerOutput::new(responses_rx),
            _main_loop: main_loop,
        };

        this.initialize(capabilities);

        this
    }

    /// Copy-paste of [`tower_lsp::Server::serve`] that skips IO serialization.
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

    /// Performs the `initialize`/`initialized` handshake with the server synchronously.
    fn initialize(&mut self, capabilities: lsp_types::ClientCapabilities) {
        let workspace_folders = Some(vec![lsp_types::WorkspaceFolder {
            uri: self.fixture.root_url(),
            name: "hello_world".to_string(),
        }]);

        self.send_request::<lsp_request!("initialize")>(lsp_types::InitializeParams {
            process_id: Some(process::id()),
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

    /// Sends an arbitrary request to the server.
    pub fn send_request_untyped(&mut self, method: &'static str, params: Value) -> Value {
        let id = self.req_id.next();
        let message = Message::request(method, id.clone(), params);
        self.rt.block_on(async {
            self.input_tx.send(message.clone()).await.expect("failed to send request");

            while let Some(response_message) =
                self.output.recv().await.unwrap_or_else(|_| panic!("timeout: {message:?}"))
            {
                match response_message {
                    Message::Request(res) if res.id().is_none() => {
                        // This looks like a notification, skip it.
                    }
                    Message::Request(req) => {
                        panic!("unexpected request: {:?}", req)
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

    /// Sends an arbitrary notification to the server.
    pub fn send_notification_untyped(&mut self, method: &'static str, params: Value) {
        let message = Message::notification(method, params);
        self.rt.block_on(async {
            self.input_tx.send(message).await.expect("failed to send notification");
        })
    }
}

/// Quality of life helpers for interacting with the server.
impl MockClient {
    /// Returns a `TextDocumentIdentifier` for the given file.
    pub fn doc_id(&self, path: impl AsRef<Path>) -> lsp_types::TextDocumentIdentifier {
        lsp_types::TextDocumentIdentifier { uri: self.fixture.file_url(path) }
    }

    /// Sends `textDocument/didOpen` notification to the server.
    pub fn open(&mut self, path: impl AsRef<Path>) {
        // Poor man's attempt at guessing the language ID
        // by assuming that file extension represents it.
        let language_id = self
            .fixture
            .file_absolute_path(&path)
            .extension()
            .and_then(OsStr::to_str)
            .unwrap_or_default()
            .to_string();

        self.send_notification::<lsp_notification!("textDocument/didOpen")>(
            lsp_types::DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri: self.fixture.file_url(&path),
                    language_id,
                    version: 0,
                    text: self.fixture.read_file(&path),
                },
            },
        )
    }
}

/// A wrapper over message receiver that keeps a trace of all received messages and times out
/// long-running requests.
struct ServerOutput {
    output_rx: mpsc::Receiver<Message>,
    trace: Vec<Message>,
}

impl ServerOutput {
    /// Creates a new [`ServerOutput`].
    fn new(output_rx: mpsc::Receiver<Message>) -> Self {
        Self { output_rx, trace: Vec::new() }
    }

    /// Receives a message from the server.
    async fn recv(&mut self) -> Result<Option<Message>, Elapsed> {
        const TIMEOUT: Duration = Duration::from_secs(2 * 60);

        let r = timeout(TIMEOUT, self.output_rx.next()).await;
        if let Ok(Some(msg)) = &r {
            self.trace.push(msg.clone());
        }
        r
    }
}
