use tower_lsp::lsp_types::{
    ClientCapabilities, CodeActionProviderCapability, CompletionOptions,
    DidChangeWatchedFilesRegistrationOptions, DocumentFilter, ExecuteCommandOptions,
    FileSystemWatcher, GlobPattern, HoverProviderCapability, OneOf, Registration, SaveOptions,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, ServerCapabilities,
    TextDocumentChangeRegistrationOptions, TextDocumentRegistrationOptions,
    TextDocumentSaveRegistrationOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
};

use crate::ide::semantic_highlighting::SemanticTokenKind;
use crate::lsp::capabilities::client::ClientCapabilitiesExt;

/// Returns capabilities the server wants to register statically.
pub fn collect_server_capabilities(client_capabilities: &ClientCapabilities) -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: if client_capabilities
            .text_document_synchronization_dynamic_registration()
        {
            None
        } else {
            Some(TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            }))
        },
        completion_provider: Some(CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: Default::default(),
            completion_item: None,
        }),
        execute_command_provider: Some(ExecuteCommandOptions {
            commands: vec!["cairo.reload".to_string()],
            work_done_progress_options: Default::default(),
        }),
        semantic_tokens_provider: Some(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: SemanticTokenKind::legend(),
                    token_modifiers: vec![],
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..SemanticTokensOptions::default()
            }
            .into(),
        ),
        document_formatting_provider: Some(OneOf::Left(true)),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
        ..ServerCapabilities::default()
    }
}

/// Returns registrations of capabilities the server wants to register dynamically.
pub fn collect_dynamic_registrations(
    client_capabilities: &ClientCapabilities,
) -> Vec<Registration> {
    let mut registrations = vec![];

    if client_capabilities.did_change_watched_files_dynamic_registration() {
        // Register patterns for the client file watcher.
        // This is used to detect changes to Scarb.toml and invalidate .cairo files.
        let registration_options = DidChangeWatchedFilesRegistrationOptions {
            watchers: ["/**/*.cairo", "/**/Scarb.toml", "/**/Scarb.lock", "/**/cairo_project.toml"]
                .map(|glob_pattern| FileSystemWatcher {
                    glob_pattern: GlobPattern::String(glob_pattern.to_string()),
                    kind: None,
                })
                .into(),
        };
        registrations.push(Registration {
            id: "workspace/didChangeWatchedFiles".to_string(),
            method: "workspace/didChangeWatchedFiles".to_string(),
            register_options: Some(serde_json::to_value(registration_options).unwrap()),
        });
    }

    if client_capabilities.text_document_synchronization_dynamic_registration() {
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

        registrations.push(Registration {
            id: "textDocument/didOpen".to_string(),
            method: "textDocument/didOpen".to_string(),
            register_options: Some(
                serde_json::to_value(&text_document_registration_options).unwrap(),
            ),
        });
        registrations.push(Registration {
            id: "textDocument/didChange".to_string(),
            method: "textDocument/didChange".to_string(),
            register_options: Some(
                serde_json::to_value(TextDocumentChangeRegistrationOptions {
                    document_selector,
                    sync_kind: 1, // TextDocumentSyncKind::FULL
                })
                .unwrap(),
            ),
        });
        registrations.push(Registration {
            id: "textDocument/didSave".to_string(),
            method: "textDocument/didSave".to_string(),
            register_options: Some(
                serde_json::to_value(TextDocumentSaveRegistrationOptions {
                    include_text: Some(false),
                    text_document_registration_options: text_document_registration_options.clone(),
                })
                .unwrap(),
            ),
        });
        registrations.push(Registration {
            id: "textDocument/didClose".to_string(),
            method: "textDocument/didClose".to_string(),
            register_options: Some(
                serde_json::to_value(&text_document_registration_options).unwrap(),
            ),
        });
    }
    registrations
}
