//! Module for collecting static and dynamic capabilities the server wants to register.
//! A capability can be registered statically ONLY
//! if the client does not support dynamic registration for this capability, as per LSP spec
//! <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#client_registerCapability>:
//!
//! > Server must not register the same capability both statically through the initialize result and
//! > dynamically for the same document selector. If a server wants to support both static and
//! > dynamic
//! > registration it needs to check the client capability in the initialize request and only
//! > register
//! > the capability statically if the client doesn’t support dynamic registration for that
//! > capability.

use std::ops::Not;

use missing_lsp_types::{
    CodeActionRegistrationOptions, DefinitionRegistrationOptions,
    DocumentFormattingRegistrationOptions,
};
use serde::Serialize;
use tower_lsp::lsp_types::{
    ClientCapabilities, CodeActionProviderCapability, CompletionOptions,
    CompletionRegistrationOptions, DefinitionOptions, DidChangeWatchedFilesRegistrationOptions,
    DocumentFilter, ExecuteCommandOptions, ExecuteCommandRegistrationOptions, FileSystemWatcher,
    GlobPattern, HoverProviderCapability, HoverRegistrationOptions, OneOf, Registration,
    SaveOptions, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensRegistrationOptions, ServerCapabilities, TextDocumentChangeRegistrationOptions,
    TextDocumentRegistrationOptions, TextDocumentSaveRegistrationOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions,
};

use crate::ide::semantic_highlighting::SemanticTokenKind;
use crate::lsp::capabilities::client::ClientCapabilitiesExt;

/// Returns capabilities the server wants to register statically.
pub fn collect_server_capabilities(client_capabilities: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: client_capabilities
            .text_document_synchronization_dynamic_registration()
            .not()
            .then_some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            })),
        completion_provider: client_capabilities.completion_dynamic_registration().not().then(
            || CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                all_commit_characters: None,
                work_done_progress_options: Default::default(),
                completion_item: None,
            },
        ),
        execute_command_provider: client_capabilities
            .execute_command_dynamic_registration()
            .not()
            .then(|| ExecuteCommandOptions {
                commands: vec!["cairo.reload".to_string()],
                work_done_progress_options: Default::default(),
            }),
        semantic_tokens_provider: client_capabilities
            .semantic_tokens_dynamic_registration()
            .not()
            .then(|| {
                SemanticTokensOptions {
                    legend: SemanticTokensLegend {
                        token_types: SemanticTokenKind::legend(),
                        token_modifiers: vec![],
                    },
                    full: Some(SemanticTokensFullOptions::Bool(true)),
                    ..SemanticTokensOptions::default()
                }
                .into()
            }),
        document_formatting_provider: client_capabilities
            .formatting_dynamic_registration()
            .not()
            .then_some(OneOf::Left(true)),
        hover_provider: client_capabilities
            .hover_dynamic_registration()
            .not()
            .then_some(HoverProviderCapability::Simple(true)),
        definition_provider: client_capabilities
            .definition_dynamic_registration()
            .not()
            .then_some(OneOf::Left(true)),
        code_action_provider: client_capabilities
            .code_action_dynamic_registration()
            .not()
            .then_some(CodeActionProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
    }
}

/// Returns registrations of capabilities the server wants to register dynamically.
pub fn collect_dynamic_registrations(
    client_capabilities: &ClientCapabilities,
) -> Vec<Registration> {
    let mut registrations = vec![];

    // Relevant files.
    let document_selector = Some(vec![
        DocumentFilter {
            language: Some("cairo".to_string()),
            scheme: Some("file".to_string()),
            pattern: None,
        },
        DocumentFilter {
            language: Some("cairo".to_string()),
            scheme: Some("vfs".to_string()),
            pattern: None,
        },
    ]);
    let text_document_registration_options =
        TextDocumentRegistrationOptions { document_selector: document_selector.clone() };

    if client_capabilities.did_change_watched_files_dynamic_registration() {
        // Register patterns for the client file watcher.
        // This is used to detect changes to config files and invalidate .cairo files.
        let registration_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: ["/**/*.cairo", "/**/Scarb.toml", "/**/Scarb.lock", "/**/cairo_project.toml"]
                .map(|glob_pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(glob_pattern.to_string()),
                    kind: None,
                })
                .into(),
        };

        registrations
            .push(create_registration("workspace/didChangeWatchedFiles", registration_options));
    }

    if client_capabilities.text_document_synchronization_dynamic_registration() {
        registrations
            .push(create_registration("textDocument/didOpen", &text_document_registration_options));

        registrations.push(create_registration(
            "textDocument/didChange",
            TextDocumentChangeRegistrationOptions {
                document_selector,
                sync_kind: 1, // TextDocumentSyncKind::FULL
            },
        ));

        registrations.push(create_registration(
            "textDocument/didSave",
            TextDocumentSaveRegistrationOptions {
                include_text: Some(false),
                text_document_registration_options: text_document_registration_options.clone(),
            },
        ));

        registrations.push(create_registration(
            "textDocument/didClose",
            &text_document_registration_options,
        ));
    }

    if client_capabilities.completion_dynamic_registration() {
        let registration_options = CompletionRegistrationOptions {
            text_document_registration_options: text_document_registration_options.clone(),
            completion_options: CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
                all_commit_characters: None,
                work_done_progress_options: Default::default(),
                completion_item: None,
            },
        };

        registrations.push(create_registration("textDocument/completion", registration_options));
    }

    if client_capabilities.execute_command_dynamic_registration() {
        let registration_options = ExecuteCommandRegistrationOptions {
            commands: vec!["cairo.reload".to_string()],
            execute_command_options: ExecuteCommandOptions {
                commands: vec!["cairo.reload".to_string()],
                work_done_progress_options: Default::default(),
            },
        };

        registrations.push(create_registration("workspace/executeCommand", registration_options));
    }

    if client_capabilities.semantic_tokens_dynamic_registration() {
        let registration_options = SemanticTokensRegistrationOptions {
            text_document_registration_options: text_document_registration_options.clone(),
            semantic_tokens_options: SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: SemanticTokenKind::legend(),
                    token_modifiers: vec![],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..SemanticTokensOptions::default()
            },
            static_registration_options: Default::default(),
        };

        registrations
            .push(create_registration("textDocument/semanticTokens", registration_options));
    }

    if client_capabilities.formatting_dynamic_registration() {
        let registration_options = DocumentFormattingRegistrationOptions {
            text_document_registration_options: text_document_registration_options.clone(),
            document_formatting_options: Default::default(),
        };

        registrations.push(create_registration("textDocument/formatting", registration_options));
    }

    if client_capabilities.hover_dynamic_registration() {
        let registration_options = HoverRegistrationOptions {
            text_document_registration_options: text_document_registration_options.clone(),
            hover_options: Default::default(),
        };

        registrations.push(create_registration("textDocument/hover", registration_options));
    }

    if client_capabilities.definition_dynamic_registration() {
        let registration_options = DefinitionRegistrationOptions {
            text_document_registration_options: text_document_registration_options.clone(),
            definition_options: DefinitionOptions {
                work_done_progress_options: Default::default(),
            },
        };

        registrations.push(create_registration("textDocument/definition", registration_options));
    }

    if client_capabilities.code_action_dynamic_registration() {
        let registration_options = CodeActionRegistrationOptions {
            text_document_registration_options,
            code_action_options: Default::default(),
        };

        registrations.push(create_registration("textDocument/codeAction", registration_options));
    }

    registrations
}

fn create_registration(method: &str, registration_options: impl Serialize) -> Registration {
    Registration {
        id: method.to_string(),
        method: method.to_string(),
        register_options: Some(serde_json::to_value(registration_options).unwrap()),
    }
}

mod missing_lsp_types {
    use serde::{Deserialize, Serialize};
    use tower_lsp::lsp_types::{
        CodeActionOptions, DefinitionOptions, DocumentFormattingOptions,
        TextDocumentRegistrationOptions,
    };

    #[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct DocumentFormattingRegistrationOptions {
        #[serde(flatten)]
        pub text_document_registration_options: TextDocumentRegistrationOptions,

        #[serde(flatten)]
        pub document_formatting_options: DocumentFormattingOptions,
    }

    #[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct DefinitionRegistrationOptions {
        #[serde(flatten)]
        pub text_document_registration_options: TextDocumentRegistrationOptions,

        #[serde(flatten)]
        pub definition_options: DefinitionOptions,
    }

    #[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
    #[serde(rename_all = "camelCase")]
    pub struct CodeActionRegistrationOptions {
        #[serde(flatten)]
        pub text_document_registration_options: TextDocumentRegistrationOptions,

        #[serde(flatten)]
        pub code_action_options: CodeActionOptions,
    }
}
