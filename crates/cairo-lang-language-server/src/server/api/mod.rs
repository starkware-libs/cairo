use lsp_server as server;
use lsp_types::notification::{
    DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles, DidCloseTextDocument,
    DidOpenTextDocument, DidSaveTextDocument, Notification,
};
use lsp_types::request::{
    CodeActionRequest, Completion, ExecuteCommand, Formatting, GotoDefinition, HoverRequest,
    Request, SemanticTokensFullRequest,
};

use crate::lsp::ext::{ExpandMacro, ProvideVirtualFile, ViewAnalyzedCrates};
use crate::server::schedule::Task;
use crate::Backend;

pub mod traits;

use super::client::Responder;
use super::schedule::BackgroundSchedule;
use crate::state::State;

pub(crate) fn request<'a>(req: server::Request) -> Task<'a> {
    let id = req.id.clone();

    match req.method.as_str() {
        CodeActionRequest::METHOD => {
            background_request_task::<CodeActionRequest>(req, BackgroundSchedule::LatencySensitive)
        }
        Completion::METHOD => {
            background_request_task::<Completion>(req, BackgroundSchedule::LatencySensitive)
        }
        ExecuteCommand::METHOD => local_request_task::<ExecuteCommand>(req),
        ExpandMacro::METHOD => {
            background_request_task::<ExpandMacro>(req, BackgroundSchedule::Worker)
        }
        Formatting::METHOD => background_request_task::<Formatting>(req, BackgroundSchedule::Fmt),
        GotoDefinition::METHOD => {
            background_request_task::<GotoDefinition>(req, BackgroundSchedule::LatencySensitive)
        }
        HoverRequest::METHOD => {
            background_request_task::<HoverRequest>(req, BackgroundSchedule::LatencySensitive)
        }
        ProvideVirtualFile::METHOD => {
            background_request_task::<ProvideVirtualFile>(req, BackgroundSchedule::LatencySensitive)
        }
        SemanticTokensFullRequest::METHOD => {
            background_request_task::<SemanticTokensFullRequest>(req, BackgroundSchedule::Worker)
        }
        ViewAnalyzedCrates::METHOD => {
            background_request_task::<ViewAnalyzedCrates>(req, BackgroundSchedule::Worker)
        }

        method => {
            tracing::warn!("Received request {method} which does not have a handler");
            return Task::nothing();
        }
    }
    .unwrap_or_else(|err| {
        tracing::error!("Encountered error when routing request with ID {id}: {err}");
        let result: Result<(), Error> = Err(err);
        Task::immediate(id, result)
    })
}

pub(crate) fn notification<'a>(notif: server::Notification) -> Task<'a> {
    match notif.method.as_str() {
        // FIXME: support cancellation from client side (add issue)
        // controller::Cancel::METHOD => local_notification_task::<controller::Cancel>(notif),
        DidChangeTextDocument::METHOD => local_notification_task::<DidChangeTextDocument>(notif),
        DidChangeConfiguration::METHOD => local_notification_task::<DidChangeConfiguration>(notif),
        DidChangeWatchedFiles::METHOD => local_notification_task::<DidChangeWatchedFiles>(notif),
        DidCloseTextDocument::METHOD => local_notification_task::<DidCloseTextDocument>(notif),
        DidOpenTextDocument::METHOD => local_notification_task::<DidOpenTextDocument>(notif),
        DidSaveTextDocument::METHOD => local_notification_task::<DidSaveTextDocument>(notif),
        method => {
            tracing::warn!("Received notification {method} which does not have a handler.");
            return Task::nothing();
        }
    }
    .unwrap_or_else(|err| {
        tracing::error!("Encountered error when routing notification: {err}");
        // show_err_msg!(
        //     "Ruff failed to handle a notification from the editor. Check the logs for more \
        //      details."
        // );
        Task::nothing()
    })
}

fn local_request_task<'a, R: traits::SyncRequestHandler>(
    req: server::Request,
) -> Result<Task<'a>, Error> {
    let (id, params) = cast_request::<R>(req)?;
    Ok(Task::local(move |state, notifier, requester, responder| {
        let result = R::run(state, notifier, requester, params);
        respond::<R>(id, result, &responder);
    }))
}

fn background_request_task<'a, R: traits::BackgroundDocumentRequestHandler>(
    req: server::Request,
    schedule: BackgroundSchedule,
) -> Result<Task<'a>, Error> {
    let (id, params) = cast_request::<R>(req)?;
    Ok(Task::background(schedule, move |state: &State| {
        let state_snapshot = state.snapshot();
        Box::new(move |notifier, responder| {
            let result =
                Backend::catch_panics(|| R::run_with_snapshot(state_snapshot, notifier, params))
                    .and_then(|res| res);
            respond::<R>(id, result, &responder);
        })
    }))
}

fn local_notification_task<'a, N: traits::SyncNotificationHandler>(
    notif: server::Notification,
) -> Result<Task<'a>, Error> {
    let (id, params) = cast_notification::<N>(notif)?;
    Ok(Task::local(move |session, notifier, requester, _| {
        if let Err(err) = N::run(session, notifier, requester, params) {
            tracing::error!("An error occurred while running {id}: {err}");
            // show_err_msg!("Ruff encountered a problem. Check the logs for more details.");
        }
    }))
}

/// Tries to cast a serialized request from the server into
/// a parameter type for a specific request handler.
/// It is *highly* recommended to not override this function in your
/// implementation.
fn cast_request<R: Request>(
    request: server::Request,
) -> Result<(server::RequestId, R::Params), Error> {
    request
        .extract(R::METHOD)
        .map_err(|err| match err {
            json_err @ server::ExtractError::JsonError { .. } => {
                anyhow::anyhow!("JSON parsing failure:\n{json_err}")
            }
            server::ExtractError::MethodMismatch(_) => {
                unreachable!(
                    "A method mismatch should not be possible here unless you've used a different \
                     handler (`R`) than the one whose method name was matched against earlier."
                )
            }
        })
        .with_failure_code(server::ErrorCode::InternalError)
}

/// Sends back a response to the server using a [`Responder`].
fn respond<R: Request>(id: server::RequestId, result: LSPResult<R::Result>, responder: &Responder) {
    if let Err(err) = &result {
        tracing::error!("An error occurred with result ID {id}: {err}");
        // show_err_msg!("Ruff encountered a problem. Check the logs for more details.");
    }
    if let Err(err) = responder.respond(id, result) {
        tracing::error!("Failed to send response: {err}");
    }
}

/// Tries to cast a serialized request from the server into
/// a parameter type for a specific request handler.
fn cast_notification<N: Notification>(
    notification: server::Notification,
) -> Result<(&'static str, N::Params), Error> {
    Ok((
        N::METHOD,
        notification
            .extract(N::METHOD)
            .map_err(|err| match err {
                json_err @ server::ExtractError::JsonError { .. } => {
                    anyhow::anyhow!("JSON parsing failure:\n{json_err}")
                }
                server::ExtractError::MethodMismatch(_) => {
                    unreachable!(
                        "A method mismatch should not be possible here unless you've used a \
                         different handler (`N`) than the one whose method name was matched \
                         against earlier."
                    )
                }
            })
            .with_failure_code(server::ErrorCode::InternalError)?,
    ))
}

pub(crate) struct Error {
    pub(crate) code: server::ErrorCode,
    pub(crate) error: anyhow::Error,
}

pub type LSPResult<T> = Result<T, Error>;

/// A trait to convert result types into the server result type, [`super::Result`].
pub trait LSPResultConversionTrait<T> {
    fn with_failure_code(self, code: server::ErrorCode) -> Result<T, Error>;
}

impl<T, E: Into<anyhow::Error>> LSPResultConversionTrait<T> for Result<T, E> {
    fn with_failure_code(self, code: server::ErrorCode) -> Result<T, Error> {
        self.map_err(|err| Error::new(err.into(), code))
    }
}

impl Error {
    pub(crate) fn new(err: anyhow::Error, code: server::ErrorCode) -> Self {
        Self { code, error: err }
    }
}

// Right now, we treat the error code as invisible data that won't
// be printed.
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.error.fmt(f)
    }
}
