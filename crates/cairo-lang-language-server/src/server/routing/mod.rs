// +------------------------------------------------------------+
// | Code adopted from:                                         |
// | Repository: https://github.com/astral-sh/ruff              |
// | File: `crates/ruff_server/src/server/api.rs`               |
// | Commit: 46a457318d8d259376a2b458b3f814b9b795fe69           |
// +------------------------------------------------------------+

use std::panic::{AssertUnwindSafe, catch_unwind};

use anyhow::anyhow;
use lsp_server::{ErrorCode, ExtractError, Notification, Request, RequestId};
use lsp_types::notification::{
    Cancel, DidChangeConfiguration, DidChangeTextDocument, DidChangeWatchedFiles,
    DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as NotificationTrait,
};
use lsp_types::request::{
    CodeActionRequest, Completion, ExecuteCommand, Formatting, GotoDefinition, HoverRequest,
    Request as RequestTrait, SemanticTokensFullRequest,
};
use tracing::{debug, error, warn};

use super::client::Responder;
use crate::lsp::ext::{ExpandMacro, ProvideVirtualFile, ViewAnalyzedCrates};
use crate::lsp::result::{LSPError, LSPResult, LSPResultEx};
use crate::server::panic::is_cancelled;
use crate::server::schedule::{BackgroundSchedule, Task};
use crate::state::State;

mod traits;

pub fn request<'a>(request: Request) -> Task<'a> {
    let id = request.id.clone();

    match request.method.as_str() {
        CodeActionRequest::METHOD => background_request_task::<CodeActionRequest>(
            request,
            BackgroundSchedule::LatencySensitive,
        ),
        Completion::METHOD => {
            background_request_task::<Completion>(request, BackgroundSchedule::LatencySensitive)
        }
        ExecuteCommand::METHOD => local_request_task::<ExecuteCommand>(request),
        ExpandMacro::METHOD => {
            background_request_task::<ExpandMacro>(request, BackgroundSchedule::Worker)
        }
        Formatting::METHOD => {
            background_request_task::<Formatting>(request, BackgroundSchedule::LatencySensitive)
        }
        GotoDefinition::METHOD => {
            background_request_task::<GotoDefinition>(request, BackgroundSchedule::LatencySensitive)
        }
        HoverRequest::METHOD => {
            background_request_task::<HoverRequest>(request, BackgroundSchedule::LatencySensitive)
        }
        ProvideVirtualFile::METHOD => background_request_task::<ProvideVirtualFile>(
            request,
            BackgroundSchedule::LatencySensitive,
        ),
        SemanticTokensFullRequest::METHOD => background_request_task::<SemanticTokensFullRequest>(
            request,
            BackgroundSchedule::Worker,
        ),
        ViewAnalyzedCrates::METHOD => {
            background_request_task::<ViewAnalyzedCrates>(request, BackgroundSchedule::Worker)
        }

        method => {
            warn!("received request {method} which does not have a handler");
            return Task::nothing();
        }
    }
    .unwrap_or_else(|error| {
        error!("encountered error when routing request with ID {id}: {error:?}");
        let result: Result<(), LSPError> = Err(error);
        Task::immediate(id, result)
    })
}

pub fn notification<'a>(notification: Notification) -> Task<'a> {
    match notification.method.as_str() {
        Cancel::METHOD => local_notification_task::<Cancel>(notification),
        DidChangeTextDocument::METHOD => {
            local_notification_task::<DidChangeTextDocument>(notification)
        }
        DidChangeConfiguration::METHOD => {
            local_notification_task::<DidChangeConfiguration>(notification)
        }
        DidChangeWatchedFiles::METHOD => {
            local_notification_task::<DidChangeWatchedFiles>(notification)
        }
        DidCloseTextDocument::METHOD => {
            local_notification_task::<DidCloseTextDocument>(notification)
        }
        DidOpenTextDocument::METHOD => local_notification_task::<DidOpenTextDocument>(notification),
        DidSaveTextDocument::METHOD => local_notification_task::<DidSaveTextDocument>(notification),
        method => {
            warn!("received notification {method} which does not have a handler");

            return Task::nothing();
        }
    }
    .unwrap_or_else(|error| {
        error!("encountered error when routing notification: {error}");

        Task::nothing()
    })
}

fn local_request_task<'a, R: traits::SyncRequestHandler>(
    request: Request,
) -> Result<Task<'a>, LSPError> {
    let (id, params) = cast_request::<R>(request)?;
    Ok(Task::local(move |state, notifier, requester, responder| {
        let result = R::run(state, notifier, requester, params);
        respond::<R>(id, result, &responder);
    }))
}

fn background_request_task<'a, R: traits::BackgroundDocumentRequestHandler>(
    request: Request,
    schedule: BackgroundSchedule,
) -> Result<Task<'a>, LSPError> {
    let (id, params) = cast_request::<R>(request)?;
    Ok(Task::background(schedule, move |state: &State| {
        let state_snapshot = state.snapshot();
        Box::new(move |notifier, responder| {
            let result = catch_unwind(AssertUnwindSafe(|| {
                R::run_with_snapshot(state_snapshot, notifier, params)
            }))
            .map_err(|err| {
                if is_cancelled(&err) {
                    debug!("LSP worker thread was cancelled");
                    LSPError::new(
                        anyhow!("LSP worker thread was cancelled"),
                        ErrorCode::ServerCancelled,
                    )
                } else {
                    error!("caught panic in LSP worker thread");
                    LSPError::new(
                        anyhow!("caught panic in LSP worker thread"),
                        ErrorCode::InternalError,
                    )
                }
            })
            .and_then(|res| res);
            respond::<R>(id, result, &responder);
        })
    }))
}

fn local_notification_task<'a, N: traits::SyncNotificationHandler>(
    notification: Notification,
) -> Result<Task<'a>, LSPError> {
    let (id, params) = cast_notification::<N>(notification)?;
    Ok(Task::local(move |session, notifier, requester, _| {
        if let Err(err) = N::run(session, notifier, requester, params) {
            error!("an error occurred while running {id}: {err}");
        }
    }))
}

/// Tries to cast a serialized request from the server into
/// a parameter type for a specific request handler.
/// It is *highly* recommended to not override this function in your
/// implementation.
fn cast_request<R: RequestTrait>(request: Request) -> Result<(RequestId, R::Params), LSPError> {
    request
        .extract(R::METHOD)
        .map_err(|error| match error {
            json_error @ ExtractError::JsonError { .. } => {
                anyhow::anyhow!("JSON parsing failure:\n{json_error}")
            }
            ExtractError::MethodMismatch(_) => {
                unreachable!(
                    "a method mismatch should not be possible here unless you've used a different \
                     handler (`R`) than the one whose method name was matched against earlier"
                )
            }
        })
        .with_failure_code(ErrorCode::InternalError)
}

/// Sends back a response to the lsp_server using a [`Responder`].
fn respond<R: RequestTrait>(id: RequestId, result: LSPResult<R::Result>, responder: &Responder) {
    if let Err(err) = &result {
        error!("an error occurred with result ID {id}: {err}");
    }
    if let Err(err) = responder.respond(id, result) {
        error!("failed to send response: {err}");
    }
}

/// Tries to cast a serialized request from the lsp_server into
/// a parameter type for a specific request handler.
fn cast_notification<N: NotificationTrait>(
    notification: Notification,
) -> Result<(&'static str, N::Params), LSPError> {
    Ok((
        N::METHOD,
        notification
            .extract(N::METHOD)
            .map_err(|error| match error {
                json_error @ ExtractError::JsonError { .. } => {
                    anyhow::anyhow!("JSON parsing failure:\n{json_error}")
                }
                ExtractError::MethodMismatch(_) => {
                    unreachable!(
                        "a method mismatch should not be possible here unless you've used a \
                         different handler (`N`) than the one whose method name was matched \
                         against earlier"
                    )
                }
            })
            .with_failure_code(ErrorCode::InternalError)?,
    ))
}
