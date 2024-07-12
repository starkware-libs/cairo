use crate::{
    lang::{
        db::{AnalysisDatabase, LsSemanticGroup, LsSyntaxGroup},
        lsp::LsProtoGroup,
    },
    lsp::ext::ExpandMacroParams,
};
use cairo_lang_defs::{db::DefsGroup, ids::ModuleId, plugin::MacroPluginMetadata};
use cairo_lang_diagnostics::Maybe;
use cairo_lang_filesystem::{
    db::FilesGroup,
    ids::{FileId, FileKind, FileLongId, VirtualFile},
    span::{TextOffset, TextPosition},
};
use cairo_lang_parser::{db::ParserGroup, utils::SimpleParserDatabase};
use cairo_lang_syntax::node::{
    ast::{ExprInlineMacro, ModuleItem, ModuleItemList},
    kind::SyntaxKind,
    SyntaxNode, TypedSyntaxNode,
};
use cairo_lang_utils::Intern;
use std::{collections::VecDeque, sync::Arc};

fn expanded_macro_files(
    db: &dyn DefsGroup,
    module_id: ModuleId,
    item: ModuleItem,
    metadata: &MacroPluginMetadata,
) -> Maybe<Vec<FileId>> {
    let syntax_db = db.upcast();
    let module_file = db.module_main_file(module_id)?;

    let mut module_queue = VecDeque::new();
    module_queue.push_back((module_file, vec![item]));
    let mut files = Vec::new();

    while let Some((module_file, item_asts)) = module_queue.pop_front() {
        files.push(module_file);

        'ast_items: for item_ast in item_asts {
            for plugin in db.macro_plugins() {
                let result = plugin.generate_code(db.upcast(), item_ast.clone(), &metadata);

                if let Some(generated) = result.code {
                    let new_file = FileLongId::Virtual(VirtualFile {
                        parent: Some(module_file),
                        name: generated.name,
                        content: Arc::new(generated.content),
                        code_mappings: Arc::new(generated.code_mappings),
                        kind: FileKind::Module,
                    })
                    .intern(db);

                    module_queue.push_back((
                        new_file,
                        db.file_module_syntax(new_file)?.items(syntax_db).elements(db.upcast()),
                    ));
                }
                if result.remove_original_item {
                    continue 'ast_items;
                }
            }
        }
    }

    Ok(files)
}

fn expand_inline_macros(db: &AnalysisDatabase, node: SyntaxNode) -> Option<String> {
    let item_ast =
        db.first_ancestor_of_kind_respective_child(node.clone(), SyntaxKind::ModuleItemList)?;
    let file = db.find_module_file_containing_node(&item_ast)?;

    let module_id = file.0;

    let item_ast = ModuleItemList::from_syntax_node(db, item_ast.parent()?)
        .elements(db)
        .into_iter()
        .find(|e| e.as_syntax_node() == item_ast)?;

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
        edition,
    };

    let files = expanded_macro_files(db, module_id, item_ast, &metadata).ok()?;

    let plugins = db.inline_macro_plugins();

    let mut output = String::new();

    for file in files {
        let macros: Vec<_> = db
            .file_syntax(file)
            .ok()?
            .descendants(db)
            .filter(|node| node.kind(db) == SyntaxKind::ExprInlineMacro)
            .collect();

        let mut file_content = db.file_content(file)?.to_string();

        // iterate in reversed order so positions are not affected by inlining
        for node in macros.into_iter().rev() {
            let inline_macro = ExprInlineMacro::from_syntax_node(db, node.clone());
            let content = plugins
                .get(&inline_macro.path(db).as_syntax_node().get_text_without_trivia(db))?
                .generate_code(db, &inline_macro, &metadata)
                .code?
                .content;

            let node_offset = node.offset();
            let width = node.width(db);

            let offset = file_content.len() - node_offset.take_from(&file_content).len();
            let width = file_content.len()
                - TextOffset::default().add_width(width).take_from(&file_content).len();

            file_content.replace_range(offset..(width + offset), content.as_str());
        }

        output.push_str(&file_content);
        output.push_str("\n\n// ------------\n\n");
    }

    return Some(
        cairo_lang_formatter::format_string(&SimpleParserDatabase::default(), output)
            .trim_end()
            .trim_end_matches("\n;")
            .to_owned(),
    );
}

pub fn expand_macro(db: &AnalysisDatabase, params: &ExpandMacroParams) -> Option<String> {
    let file_id = db.file_for_url(&params.text_document.uri)?;
    let node = db.find_syntax_node_at_position(
        file_id,
        TextPosition { line: params.position.line, col: params.position.character },
    )?;
    expand_inline_macros(db, node)
}
