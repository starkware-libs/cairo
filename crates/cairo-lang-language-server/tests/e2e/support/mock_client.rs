use std::collections::VecDeque;
use std::ffi::OsStr;
use std::path::Path;
use std::time::Duration;
use std::{fmt, mem, process};

use cairo_lang_language_server::build_service_for_e2e_tests;
use lsp_server::{Message, Notification, Request, Response};
use lsp_types::request::{RegisterCapability, Request as LspRequest};
use lsp_types::{lsp_notification, lsp_request};
use serde_json::Value;

use crate::support::fixture::Fixture;
use crate::support::jsonrpc::RequestIdGenerator;

/// A mock language client implementation that facilitates end-to-end testing language servers.
///
/// ## Termination
///
/// The language server is terminated abruptly upon dropping of this struct.
/// The `shutdown` request and `exit` notifications are not sent at all.
/// Instead, the thread executing the server is being shut down and any running
/// blocking tasks are given a small period of time to complete.
pub struct MockClient {
    fixture: Fixture,
    req_id: RequestIdGenerator,
    client: lsp_server::Connection,
    trace: Vec<Message>,
    workspace_configuration: Value,
    expect_request_handlers: VecDeque<ExpectRequestHandler>,
}

impl MockClient {
    /// Starts and initializes CairoLS in the context of a given fixture and given client
    /// capabilities.
    ///
    /// Upon completion of this function, the language server will be in the _initialized_ state
    /// (i.e., the `initialize` request and `initialized` notification both will be completed).
    #[must_use]
    pub fn start(
        fixture: Fixture,
        capabilities: lsp_types::ClientCapabilities,
        workspace_configuration: Value,
    ) -> Self {
        let (init, client) = build_service_for_e2e_tests();

        let mut this = Self {
            fixture,
            client,
            req_id: RequestIdGenerator::default(),
            trace: Vec::new(),
            workspace_configuration,
            expect_request_handlers: Default::default(),
        };

        std::thread::spawn(|| init().run_for_tests());

        this.initialize(capabilities);

        this
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

        self.expect_request::<RegisterCapability>(|_req| {});

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
        let message = Message::Request(Request::new(id.clone(), method.to_owned(), params));

        let mut expect_request_handlers = mem::take(&mut self.expect_request_handlers);
        let does_expect_requests = !expect_request_handlers.is_empty();

        self.client.sender.send(message.clone()).expect("failed to send request");

        while let Some(response_message) =
            self.recv().unwrap_or_else(|err| panic!("{err:?}: {message:?}"))
        {
            match response_message {
                Message::Notification(_) => {
                    // Skip notifications.
                }

                Message::Request(req) => {
                    if does_expect_requests {
                        if let Some(handler) = expect_request_handlers.pop_front() {
                            let response = (handler.f)(&req);
                            let message = Message::Response(response);
                            self.client.sender.send(message).expect("failed to send response");
                            continue;
                        }
                    }

                    panic!("unexpected request: {:?}", req)
                }

                Message::Response(res) => {
                    let res_id = res.id;
                    let result = res.result.ok_or_else(|| res.error.unwrap());

                    assert_eq!(res_id, id);

                    match result {
                        Ok(result) => return result,
                        Err(err) => panic!("error response: {:#?}", err),
                    }
                }
            }
        }

        panic!("no response for request: {message:?}")
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
        let message = Message::Notification(Notification::new(method.to_string(), params));
        self.client.sender.send(message).expect("failed to send notification");
    }
}

/// Introspection.
impl MockClient {
    /// Gets a list of messages received from the server.
    pub fn trace(&self) -> &[Message] {
        &self.trace
    }
}

#[derive(Debug)]
enum RecvError {
    Timeout,
    NoMessage,
}

/// Receiving messages.
impl MockClient {
    /// Receives a message from the server.
    fn recv(&mut self) -> Result<Option<Message>, RecvError> {
        const TIMEOUT: Duration = Duration::from_secs(3 * 60);
        let message = match self.client.receiver.recv_timeout(TIMEOUT) {
            Ok(msg) => Some(msg),
            Err(crossbeam::channel::RecvTimeoutError::Disconnected) => None,
            Err(crossbeam::channel::RecvTimeoutError::Timeout) => return Err(RecvError::Timeout),
        };

        if let Some(message) = &message {
            self.trace.push(message.clone());

            if let Message::Request(request) = &message {
                if request.method == <lsp_request!("workspace/configuration")>::METHOD {
                    self.auto_respond_to_workspace_configuration_request(request);
                }
            }
        }

        Ok(message)
    }

    /// Looks for a message that satisfies the given predicate in message trace or waits for a new
    /// one.
    fn wait_for_message<T>(
        &mut self,
        predicate: impl Fn(&Message) -> Option<T>,
    ) -> Result<T, RecvError> {
        for message in &self.trace {
            if let Some(ret) = predicate(message) {
                return Ok(ret);
            }
        }

        loop {
            let message = self.recv()?.ok_or(RecvError::NoMessage)?;
            if let Some(ret) = predicate(&message) {
                return Ok(ret);
            }
        }
    }

    /// Looks for a client JSON-RPC request that satisfies the given predicate in message trace
    /// or waits for a new one.
    fn wait_for_rpc_notification<T>(
        &mut self,
        predicate: impl Fn(&lsp_server::Notification) -> Option<T>,
    ) -> T {
        self.wait_for_message(|message| {
            let Message::Notification(notification) = message else { return None };
            predicate(notification)
        })
        .unwrap_or_else(|err| panic!("waiting for request failed: {err:?}"))
    }

    /// Looks for a typed client notification that satisfies the given predicate in message trace
    /// or waits for a new one.
    pub fn wait_for_notification<N>(&mut self, predicate: impl Fn(&N::Params) -> bool) -> N::Params
    where
        N: lsp_types::notification::Notification,
    {
        self.wait_for_rpc_notification(|notification| {
            if notification.method != N::METHOD {
                return None;
            }
            let params = serde_json::from_value(notification.params.clone())
                .expect("failed to parse notification params");
            predicate(&params).then_some(params)
        })
    }
}

/// Methods for handling interactive requests.
impl MockClient {
    /// Expect a specified request to be received from the server while processing the next client
    /// request.
    ///
    /// The handler is expected to return a response to the caught request.
    /// Handler can validate the request by asserting its parameters.
    /// Calls to this method can be stacked sequentially, to expect a sequence of requests being
    /// received from the server.
    pub fn expect_request<R>(&mut self, handler: impl FnOnce(&R::Params) -> R::Result + 'static)
    where
        R: lsp_types::request::Request,
    {
        self.expect_request_untyped(R::METHOD, move |req| {
            assert_eq!(req.method, R::METHOD);

            let id = req.id.clone();

            let params =
                serde_json::from_value(req.params.clone()).expect("failed to parse request params");
            let result = handler(&params);
            let result = serde_json::to_value(result).expect("failed to serialize response");

            lsp_server::Response::new_ok(id, result)
        })
    }

    /// Untyped version of [`MockClient::expect_request`].
    ///
    /// The `description` parameter is used in panic messages to tell that this handler did not
    /// fire. Usually it is enough to put request method name here.
    pub fn expect_request_untyped(
        &mut self,
        description: &'static str,
        handler: impl FnOnce(&lsp_server::Request) -> lsp_server::Response + 'static,
    ) {
        self.expect_request_handlers
            .push_back(ExpectRequestHandler { description, f: Box::new(handler) })
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

    /// Waits for `textDocument/publishDiagnostics` notification for the given file.
    pub fn wait_for_diagnostics(
        &mut self,
        path: impl AsRef<Path>,
    ) -> lsp_types::PublishDiagnosticsParams {
        let uri = self.fixture.file_url(&path);
        self.wait_for_notification::<lsp_notification!("textDocument/publishDiagnostics")>(
            |params| params.uri == uri,
        )
    }

    /// Sends `textDocument/didOpen` notification to the server and then waits for matching
    /// `textDocument/publishDiagnostics` notification.
    pub fn open_and_wait_for_diagnostics(
        &mut self,
        path: impl AsRef<Path>,
    ) -> lsp_types::PublishDiagnosticsParams {
        let path = path.as_ref();
        self.open(path);
        self.wait_for_diagnostics(path)
    }
}

/// Handling workspace configuration workflow.
impl MockClient {
    /// Assuming `request` is a `workspace/configuration` request, computes and sends a response to
    /// it.
    fn auto_respond_to_workspace_configuration_request(&mut self, request: &lsp_server::Request) {
        assert_eq!(request.method, <lsp_request!("workspace/configuration") as LspRequest>::METHOD);

        let id = request.id.clone();

        let params = serde_json::from_value(request.params.clone())
            .expect("failed to parse `workspace/configuration` params");

        let result = self.compute_workspace_configuration(params);

        let result = serde_json::to_value(result)
            .expect("failed to serialize `workspace/configuration` response");

        let message = Message::Response(Response::new_ok(id, result));
        self.client
            .sender
            .send(message)
            .expect("failed to send `workspace/configuration` response");
    }

    /// Computes response to `workspace/configuration` request.
    fn compute_workspace_configuration(
        &self,
        params: <lsp_request!("workspace/configuration") as LspRequest>::Params,
    ) -> Vec<Value> {
        params
            .items
            .iter()
            .map(|item| {
                // NOTE: `scope_uri` is ignored.
                match &item.section {
                    Some(section) => {
                        // Items may ask for nested entries, with dot being the path separator.
                        section
                            .split('.')
                            .try_fold(&self.workspace_configuration, |config, key| config.get(key))
                            .cloned()
                            .unwrap_or(Value::Null)
                    }
                    None => self.workspace_configuration.clone(),
                }
            })
            .collect()
    }
}

impl AsRef<Fixture> for MockClient {
    fn as_ref(&self) -> &Fixture {
        &self.fixture
    }
}

/// A container for callbacks passed to [`MockClient::expect_request`] that also carries a text
/// telling what this callback expects.
///
/// The description is used in panic messages.
struct ExpectRequestHandler {
    description: &'static str,
    f: Box<dyn FnOnce(&lsp_server::Request) -> lsp_server::Response>,
}

impl fmt::Debug for ExpectRequestHandler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.description, f)
    }
}
