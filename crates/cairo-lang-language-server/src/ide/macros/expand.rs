use std::collections::VecDeque;
use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::plugin::MacroPluginMetadata;
use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_filesystem::ids::{FileId, FileKind, FileLongId, VirtualFile};
use cairo_lang_filesystem::span::TextOffset;
use cairo_lang_formatter::FormatterConfig;
use cairo_lang_parser::db::ParserGroup;
use cairo_lang_parser::parser::Parser;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_syntax::node::ast::{ExprInlineMacro, ModuleItemList};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use indoc::formatdoc;
use tower_lsp::lsp_types::TextDocumentPositionParams;

use crate::lang::db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup};
use crate::lang::lsp::{LsProtoGroup, ToCairo};

/// Tries to expand macro, returns it as string.
pub fn expand_macro(db: &AnalysisDatabase, params: &TextDocumentPositionParams) -> Option<String> {
    let file_id = db.file_for_url(&params.text_document.uri)?;
    let node = db.find_syntax_node_at_position(file_id, params.position.to_cairo())?;

    let module_id = db.find_module_file_containing_node(&node)?.0;
    let crate_id = module_id.owning_crate(db);
    let cfg_set = db
        .crate_config(crate_id)
        .and_then(|cfg| cfg.settings.cfg_set.map(Arc::new))
        .unwrap_or(db.cfg_set());
    let edition = db
        .crate_config(module_id.owning_crate(db))
        .map(|cfg| cfg.settings.edition)
        .unwrap_or_default();

    let metadata = MacroPluginMetadata {
        cfg_set: &cfg_set,
        declared_derives: &db.declared_derives(),
        allowed_features: &Default::default(),
        edition,
    };

    let module_file = db.module_main_file(module_id).ok()?;

    let item_ast_node =
        db.first_ancestor_of_kind_respective_child(node.clone(), SyntaxKind::ModuleItemList);
    let macro_ast_node = db
        .first_ancestor_of_kind_respective_child(node.clone(), SyntaxKind::ExprInlineMacro)
        .and_then(|node| node.parent());

    let (node_to_expand, is_inline_macro) = match (item_ast_node, macro_ast_node) {
        (Some(item_ast_node), Some(macro_ast_node)) => {
            if node_depth(item_ast_node.clone()) > node_depth(macro_ast_node.clone()) {
                (item_ast_node, false)
            } else {
                (macro_ast_node, true)
            }
        }
        (Some(item_ast_node), None) => (item_ast_node, false),
        (None, Some(macro_ast_node)) => (macro_ast_node, true),
        (None, None) => return None,
    };

    let files = if is_inline_macro {
        VecDeque::from([module_file])
    } else {
        expanded_macro_files(db, module_file, node_to_expand.clone(), &metadata)?
    };

    expand_inline_macros(db, node_to_expand, files, &metadata, is_inline_macro)
}

/// Expands macro plugins and returns all file generated by plugins (origin file included).
fn expanded_macro_files(
    db: &dyn DefsGroup,
    module_file: FileId,
    item_ast_node: SyntaxNode,
    metadata: &MacroPluginMetadata<'_>,
) -> Option<VecDeque<FileId>> {
    let syntax_db = db.upcast();

    let item = ModuleItemList::from_syntax_node(syntax_db, item_ast_node.parent()?)
        .elements(syntax_db)
        .into_iter()
        .find(|e| e.as_syntax_node() == item_ast_node)?;

    let mut module_queue = VecDeque::from([(module_file, vec![item])]);
    let mut files = VecDeque::new();

    while let Some((module_file, item_asts)) = module_queue.pop_front() {
        files.push_back(module_file);

        for item_ast in item_asts {
            for plugin in db.macro_plugins() {
                let result = plugin.generate_code(db.upcast(), item_ast.clone(), metadata);

                if let Some(generated) = result.code {
                    let new_file = FileLongId::Virtual(VirtualFile {
                        parent: Some(module_file),
                        name: generated.name,
                        content: generated.content.into(),
                        code_mappings: Default::default(),
                        kind: FileKind::Module,
                    })
                    .intern(db);

                    module_queue.push_back((
                        new_file,
                        db.file_module_syntax(new_file)
                            .ok()?
                            .items(syntax_db)
                            .elements(db.upcast()),
                    ));
                }
                if result.remove_original_item {
                    break;
                }
            }
        }
    }

    Some(files)
}

/// Finds the depth of the node in tree.
fn node_depth(node: SyntaxNode) -> usize {
    std::iter::successors(Some(node), SyntaxNode::parent).count()
}

/// Expands inline macros in files.
fn expand_inline_macros(
    db: &AnalysisDatabase,
    node_to_expand: SyntaxNode,
    mut files: VecDeque<FileId>,
    metadata: &MacroPluginMetadata<'_>,
    main_file_is_inline_macro: bool,
) -> Option<String> {
    let mut output = String::new();

    let main_file = files.pop_front().unwrap();

    let file_content = db.file_content(main_file)?.to_string();

    process_file(
        db,
        &file_content,
        metadata,
        main_file,
        &mut files,
        &mut output,
        FileProcessorConfig::main_file(
            db,
            node_to_expand.clone(),
            &file_content,
            main_file_is_inline_macro,
        ),
    )?;

    while let Some(file) = files.pop_front() {
        let file_content = db.file_content(file)?.to_string();

        process_file(
            db,
            &file_content,
            metadata,
            file,
            &mut files,
            &mut output,
            FileProcessorConfig::generated_file(db, file, file_content.clone())?,
        )?;
    }

    if output.is_empty() { None } else { Some(format_output(&output, main_file_is_inline_macro)) }
}

/// Formats output string.
fn format_output(output: &str, is_inline_macro: bool) -> String {
    let db = &SimpleParserDatabase::default();
    let virtual_file = FileLongId::Virtual(VirtualFile {
        parent: Default::default(),
        name: Default::default(),
        content: Default::default(),
        code_mappings: Default::default(),
        kind: FileKind::Module,
    })
    .intern(db);

    let syntax_root = if is_inline_macro {
        Parser::parse_file_expr(db, &mut DiagnosticsBuilder::default(), virtual_file, output)
            .as_syntax_node()
    } else {
        Parser::parse_file(db, &mut DiagnosticsBuilder::default(), virtual_file, output)
            .as_syntax_node()
    };

    cairo_lang_formatter::get_formatted_file(db, &syntax_root, FormatterConfig::default())
        .trim_end()
        .trim_end_matches("\n;")
        .to_owned()
}

/// Config for single file processing.
struct FileProcessorConfig {
    content: String,
    offset_correction: usize,
    macros: Vec<SyntaxNode>,
}

impl FileProcessorConfig {
    /// Config for main file.
    fn main_file(
        db: &AnalysisDatabase,
        node_to_expand: SyntaxNode,
        file_content: &str,
        is_inline_macro: bool,
    ) -> Self {
        Self {
            content: node_to_expand.get_text(db),
            offset_correction: file_content.len()
                - node_to_expand.offset().take_from(file_content).len(),
            macros: if is_inline_macro {
                vec![node_to_expand]
            } else {
                node_to_expand
                    .descendants(db)
                    .filter(|node| node.kind(db) == SyntaxKind::ExprInlineMacro)
                    .collect()
            },
        }
    }

    /// Config for generated files.
    fn generated_file(db: &AnalysisDatabase, file: FileId, file_content: String) -> Option<Self> {
        Some(Self {
            content: file_content,
            offset_correction: 0,
            macros: db
                .file_syntax(file)
                .ok()?
                .descendants(db)
                .filter(|node| node.kind(db) == SyntaxKind::ExprInlineMacro)
                .collect(),
        })
    }
}

/// Inlines macros in single file, pushes new inlined file if any macro was inlined, otherwise
/// pushes output string.
fn process_file(
    db: &AnalysisDatabase,
    file_content: &str,
    metadata: &MacroPluginMetadata<'_>,
    file: FileId,
    files: &mut VecDeque<FileId>,
    output: &mut String,
    mut config: FileProcessorConfig,
) -> Option<()> {
    let plugins = db.inline_macro_plugins();

    if config.macros.is_empty() {
        push_file_content(db, file, &config.content, output);
    } else {
        // Iterate in reversed order so positions are not affected by inlining.
        for node in config.macros.into_iter().rev() {
            let inline_macro = ExprInlineMacro::from_syntax_node(db, node.clone());
            let code = plugins
                .get(&inline_macro.path(db).as_syntax_node().get_text_without_trivia(db))?
                .generate_code(db, &inline_macro, metadata)
                .code?
                .content;

            let offset = file_content.len()
                - node.offset().take_from(file_content).len()
                - config.offset_correction;
            let width = file_content.len()
                - TextOffset::default().add_width(node.width(db)).take_from(file_content).len();

            config.content.replace_range(offset..(width + offset), code.as_str());
        }
        let new_file = FileLongId::Virtual(VirtualFile {
            parent: Some(file),
            name: file.file_name(db).into(),
            content: config.content.into(),
            code_mappings: Default::default(),
            kind: file.kind(db),
        })
        .intern(db);

        files.push_back(new_file);
    };

    Some(())
}

/// Extends output with single file.
fn push_file_content(db: &AnalysisDatabase, file: FileId, file_content: &str, output: &mut String) {
    let file_name = file.file_name(db);
    let file_underscore = "-".repeat(file_name.chars().count());

    output.push_str(&formatdoc!(
        "
        // {file_name}
        // {file_underscore}


        "
    ));

    output.push_str(file_content.trim_end_matches('\n'));
    output.push_str("\n\n");
}
