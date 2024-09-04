//! Module for collecting static and dynamic capabilities the server wants to register.
//! A capability can be registered statically ONLY
//! if the client does not support dynamic registration for this capability, as per LSP spec
//! <https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#client_registerCapability>:
//!
//! > Server must not register the same capability both statically through the `initialize` result
//! > and dynamically for the same document selector.
//! > If a server wants to support both static and dynamic registration,
//! > it needs to check the client capability in the `initialize` request and only register the
//! > capability statically
//! > if the client does not support dynamic registration for that capability.

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

use self::missing_lsp_types::{
    CodeActionRegistrationOptions, DefinitionRegistrationOptions,
    DocumentFormattingRegistrationOptions,
};
use crate::ide::semantic_highlighting::SemanticTokenKind;
use crate::lsp::capabilities::client::ClientCapabilitiesExt;

/// Returns capabilities the server wants to register statically.
pub fn collect_server_capabilities(client_capabilities: &ClientCapabilities) -> ServerCapabilities {
    collect_server_capabilities_impl(client_capabilities)
}

/// Returns registrations of capabilities the server wants to register dynamically.
pub fn collect_dynamic_registrations(
    client_capabilities: &ClientCapabilities,
) -> Vec<Registration> {
    collect_dynamic_registrations_impl(client_capabilities)
}

macro_rules! server_capabilities {
    [
        $(
            {
                client_capability = $client_capability:expr,
                $(
                    let $var_name:ident = $var_value:expr;
                )*
                $(
                    dyn {
                        method = $dyn_method:literal,
                        register_options = $dyn_register_options:expr,
                    }
                )*
                $(
                    static {
                        field = $static_field:ident,
                        capability = $static_capability:expr,
                    }
                )?
            },
        )*
    ] => {
        fn collect_server_capabilities_impl(
            client_capabilities: &ClientCapabilities
        ) -> ServerCapabilities {
            let mut capabilities = ServerCapabilities::default();

            $({
                if !$client_capability(client_capabilities) {
                    $(let $var_name = $var_value;)*
                    $(capabilities.$static_field = Some($static_capability);)?
                }
            })*

            capabilities
        }

        fn collect_dynamic_registrations_impl(
            client_capabilities: &ClientCapabilities
        ) -> Vec<Registration> {
            let mut registrations = Vec::new();

            $(
                if $client_capability(client_capabilities) {
                    $(let $var_name = $var_value;)*
                    $(
                        registrations.push(Registration {
                            id: $dyn_method.to_string(),
                            method: $dyn_method.to_string(),
                            register_options: Some(
                                serde_json::to_value(&$dyn_register_options).unwrap()
                            ),
                        });
                    )*
                }
            )*

            registrations
        }
    };
}

server_capabilities![
    {
        client_capability = ClientCapabilities::text_document_synchronization_dynamic_registration,
        dyn {
            method = "textDocument/didOpen",
            register_options = text_document_registration_options(),
        }
        dyn {
            method = "textDocument/didChange",
            register_options = TextDocumentChangeRegistrationOptions {
                document_selector: text_document_registration_options().document_selector,
                sync_kind: 1, // TextDocumentSyncKind::FULL
            },
        }
        dyn {
            method = "textDocument/didSave",
            register_options = TextDocumentSaveRegistrationOptions {
                include_text: Some(false),
                text_document_registration_options: text_document_registration_options(),
            },
        }
        dyn {
            method = "textDocument/didClose",
            register_options = text_document_registration_options(),
        }
        static {
            field = text_document_sync,
            capability = TextDocumentSyncCapability::Options(TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                will_save: Some(false),
                will_save_wait_until: Some(false),
                save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                    include_text: Some(false),
                })),
            }),
        }
    },
    {
        client_capability = ClientCapabilities::did_change_watched_files_dynamic_registration,
        dyn {
            method = "workspace/didChangeWatchedFiles",
            register_options = DidChangeWatchedFilesRegistrationOptions {
                watchers: ["/**/*.cairo", "/**/Scarb.toml", "/**/Scarb.lock", "/**/cairo_project.toml"]
                    .map(|glob_pattern| FileSystemWatcher {
                        glob_pattern: GlobPattern::String(glob_pattern.to_string()),
                        kind: None,
                    })
                    .into(),
            },
        }
    },
    {
        client_capability = ClientCapabilities::completion_dynamic_registration,
        let completion_options = CompletionOptions {
            resolve_provider: Some(false),
            trigger_characters: Some(vec![".".to_string(), ":".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: Default::default(),
            completion_item: None,
        };
        dyn {
            method = "textDocument/completion",
            register_options = CompletionRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                completion_options,
            },
        }
        static {
            field = completion_provider,
            capability = completion_options,
        }
    },
    {
        client_capability = ClientCapabilities::execute_command_dynamic_registration,
        let execute_command_options = ExecuteCommandOptions {
            commands: vec!["cairo.reload".to_string()],
            work_done_progress_options: Default::default(),
        };
        dyn {
            method = "workspace/executeCommand",
            register_options = ExecuteCommandRegistrationOptions {
                commands: execute_command_options.commands.clone(),
                execute_command_options,
            },
        }
        static {
            field = execute_command_provider,
            capability = execute_command_options,
        }
    },
    {
        client_capability = ClientCapabilities::semantic_tokens_dynamic_registration,
        let semantic_tokens_options = SemanticTokensOptions {
            legend: SemanticTokensLegend {
                token_types: SemanticTokenKind::legend(),
                token_modifiers: vec![],
            },
            full: Some(SemanticTokensFullOptions::Bool(true)),
            ..SemanticTokensOptions::default()
        };
        dyn {
            method = "textDocument/semanticTokens",
            register_options = SemanticTokensRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                semantic_tokens_options,
                static_registration_options: Default::default(),
            },
        }
        static {
            field = semantic_tokens_provider,
            capability = semantic_tokens_options.into(),
        }
    },
    {
        client_capability = ClientCapabilities::formatting_dynamic_registration,
        dyn {
            method = "textDocument/formatting",
            register_options = DocumentFormattingRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                document_formatting_options: Default::default(),
            },
        }
        static {
            field = document_formatting_provider,
            capability = OneOf::Left(true),
        }
    },
    {
        client_capability = ClientCapabilities::hover_dynamic_registration,
        dyn {
            method = "textDocument/hover",
            register_options = HoverRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                hover_options: Default::default(),
            },
        }
        static {
            field = hover_provider,
            capability = HoverProviderCapability::Simple(true),
        }
    },
    {
        client_capability = ClientCapabilities::definition_dynamic_registration,
        dyn {
            method = "textDocument/definition",
            register_options = DefinitionRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                definition_options: DefinitionOptions {
                    work_done_progress_options: Default::default(),
                },
            },
        }
        static {
            field = definition_provider,
            capability = OneOf::Left(true),
        }
    },
    {
        client_capability = ClientCapabilities::code_action_dynamic_registration,
        dyn {
            method = "textDocument/codeAction",
            register_options = CodeActionRegistrationOptions {
                text_document_registration_options: text_document_registration_options(),
                code_action_options: Default::default(),
            },
        }
        static {
            field = code_action_provider,
            capability = CodeActionProviderCapability::Simple(true),
        }
    },
];

fn text_document_registration_options() -> TextDocumentRegistrationOptions {
    TextDocumentRegistrationOptions {
        document_selector: Some(vec![
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
        ]),
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
