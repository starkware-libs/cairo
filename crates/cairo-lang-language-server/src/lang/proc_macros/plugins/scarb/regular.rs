use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{PluginGeneratedFile, PluginResult};
use cairo_lang_filesystem::ids::{CodeMapping, CodeOrigin};
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_macro::{TokenStream, TokenStreamMetadata};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::ast::{self, Expr, PathSegment};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::ids::SyntaxStablePtrId;
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use convert_case::{Case, Casing};
use scarb_proc_macro_server_types::methods::expand::{ExpandAttributeParams, ExpandDeriveParams};
use scarb_stable_hash::StableHasher;

use super::{FromSyntaxNode, into_cairo_diagnostics};
use crate::lang::db::AnalysisDatabase;
use crate::lang::proc_macros::db::ProcMacroCacheGroup;

// scarb code

const DERIVE_ATTR: &str = "derive";

enum AttrExpansionFound {
    Some { name: String, args: TokenStream, stable_ptr: SyntaxStablePtrId },
    None,
    Last { name: String, args: TokenStream, stable_ptr: SyntaxStablePtrId },
}

fn calculate_metadata(db: &dyn SyntaxGroup, item_ast: ast::ModuleItem) -> TokenStreamMetadata {
    fn short_hash(hashable: impl std::hash::Hash) -> String {
        let mut hasher = StableHasher::new();
        hashable.hash(&mut hasher);
        hasher.finish_as_short_hash()
    }
    let stable_ptr = item_ast.clone().stable_ptr().untyped();
    let file_path = stable_ptr.file_id(db).full_path(db.upcast());
    let file_id = short_hash(file_path.clone());
    TokenStreamMetadata::new(file_path, file_id)
}

pub fn macro_generate_code(
    db: &AnalysisDatabase,
    item_ast: ast::ModuleItem,
    defined_attributes: &[String],
    defined_derives: &[String],
) -> PluginResult {
    let stream_metadata = calculate_metadata(db, item_ast.clone());

    // Expand first attribute.
    // Note that we only expand the first attribute, as we assume that the rest of the attributes
    // will be handled by a subsequent call to this function.
    let (input, body) = parse_attribute(db, defined_attributes, item_ast.clone());

    if let Some(result) = match input {
        AttrExpansionFound::Last { name, args, stable_ptr } => Some((name, args, stable_ptr, true)),
        AttrExpansionFound::Some { name, args, stable_ptr } => {
            Some((name, args, stable_ptr, false))
        }
        AttrExpansionFound::None => None,
    }
    .map(|(name, args, stable_ptr, last)| {
        let token_stream = body.with_metadata(stream_metadata.clone());
        let span = item_ast.as_syntax_node().span(db);
        expand_attribute(db, name, last, args, token_stream, span, stable_ptr)
    }) {
        return result;
    }

    // Expand all derives.
    // Note that all proc macro attributes should be already expanded at this point.
    if let Some(result) =
        expand_derives(db, defined_derives, item_ast.clone(), stream_metadata.clone())
    {
        return result;
    }

    // No expansions can be applied.
    PluginResult { code: None, diagnostics: Vec::new(), remove_original_item: false }
}

/// Find first attribute procedural macros that should be expanded.
///
/// Remove the attribute from the code.
fn parse_attribute(
    db: &dyn SyntaxGroup,
    defined_attributes: &[String],
    item_ast: ast::ModuleItem,
) -> (AttrExpansionFound, TokenStream) {
    let mut item_builder = PatchBuilder::new(db, &item_ast);
    let input = match item_ast.clone() {
        ast::ModuleItem::Struct(struct_ast) => {
            let attrs = struct_ast.attributes(db).elements(db);
            let expansion =
                parse_attrs(db, defined_attributes, &mut item_builder, attrs, &item_ast);
            item_builder.add_node(struct_ast.visibility(db).as_syntax_node());
            item_builder.add_node(struct_ast.struct_kw(db).as_syntax_node());
            item_builder.add_node(struct_ast.name(db).as_syntax_node());
            item_builder.add_node(struct_ast.generic_params(db).as_syntax_node());
            item_builder.add_node(struct_ast.lbrace(db).as_syntax_node());
            item_builder.add_node(struct_ast.members(db).as_syntax_node());
            item_builder.add_node(struct_ast.rbrace(db).as_syntax_node());
            expansion
        }
        ast::ModuleItem::Enum(enum_ast) => {
            let attrs = enum_ast.attributes(db).elements(db);
            let expansion =
                parse_attrs(db, defined_attributes, &mut item_builder, attrs, &item_ast);
            item_builder.add_node(enum_ast.visibility(db).as_syntax_node());
            item_builder.add_node(enum_ast.enum_kw(db).as_syntax_node());
            item_builder.add_node(enum_ast.name(db).as_syntax_node());
            item_builder.add_node(enum_ast.generic_params(db).as_syntax_node());
            item_builder.add_node(enum_ast.lbrace(db).as_syntax_node());
            item_builder.add_node(enum_ast.variants(db).as_syntax_node());
            item_builder.add_node(enum_ast.rbrace(db).as_syntax_node());
            expansion
        }
        ast::ModuleItem::ExternType(extern_type_ast) => {
            let attrs = extern_type_ast.attributes(db).elements(db);
            let expansion =
                parse_attrs(db, defined_attributes, &mut item_builder, attrs, &item_ast);
            item_builder.add_node(extern_type_ast.visibility(db).as_syntax_node());
            item_builder.add_node(extern_type_ast.extern_kw(db).as_syntax_node());
            item_builder.add_node(extern_type_ast.type_kw(db).as_syntax_node());
            item_builder.add_node(extern_type_ast.name(db).as_syntax_node());
            item_builder.add_node(extern_type_ast.generic_params(db).as_syntax_node());
            item_builder.add_node(extern_type_ast.semicolon(db).as_syntax_node());
            expansion
        }
        ast::ModuleItem::ExternFunction(extern_func_ast) => {
            let attrs = extern_func_ast.attributes(db).elements(db);
            let expansion =
                parse_attrs(db, defined_attributes, &mut item_builder, attrs, &item_ast);
            item_builder.add_node(extern_func_ast.visibility(db).as_syntax_node());
            item_builder.add_node(extern_func_ast.extern_kw(db).as_syntax_node());
            item_builder.add_node(extern_func_ast.declaration(db).as_syntax_node());
            item_builder.add_node(extern_func_ast.semicolon(db).as_syntax_node());
            expansion
        }
        ast::ModuleItem::FreeFunction(free_func_ast) => {
            let attrs = free_func_ast.attributes(db).elements(db);
            let expansion =
                parse_attrs(db, defined_attributes, &mut item_builder, attrs, &item_ast);
            item_builder.add_node(free_func_ast.visibility(db).as_syntax_node());
            item_builder.add_node(free_func_ast.declaration(db).as_syntax_node());
            item_builder.add_node(free_func_ast.body(db).as_syntax_node());
            expansion
        }
        _ => AttrExpansionFound::None,
    };
    let token_stream = TokenStream::new(item_builder.build().0);
    (input, token_stream)
}

fn parse_attrs(
    db: &dyn SyntaxGroup,
    defined_attributes: &[String],
    builder: &mut PatchBuilder<'_>,
    attrs: Vec<ast::Attribute>,
    item_ast: &ast::ModuleItem,
) -> AttrExpansionFound {
    // Note this function does not affect the executable attributes,
    // as it only pulls `ExpansionKind::Attr` from the plugin.
    // This means that executable attributes will neither be removed from the item,
    // nor will they cause the item to be rewritten.
    let mut expansion = None;
    let mut last = true;
    for attr in attrs {
        // We ensure that this flag is changed *after* the expansion is found.
        if last {
            let structured_attr = attr.clone().structurize(db);
            let found = defined_attributes.contains(&structured_attr.id.into());

            if found {
                if expansion.is_none() {
                    let mut args_builder = PatchBuilder::new(db, item_ast);
                    args_builder.add_node(attr.arguments(db).as_syntax_node());
                    let args = TokenStream::new(args_builder.build().0);
                    expansion = Some((
                        attr.attr(db).as_syntax_node().get_text(db),
                        args,
                        attr.stable_ptr().untyped(),
                    ));
                    // Do not add the attribute for found expansion.
                    continue;
                } else {
                    last = false;
                }
            }
        }
        builder.add_node(attr.as_syntax_node());
    }
    match (expansion, last) {
        (Some((name, args, stable_ptr)), true) => {
            AttrExpansionFound::Last { name, args, stable_ptr }
        }
        (Some((name, args, stable_ptr)), false) => {
            AttrExpansionFound::Some { name, args, stable_ptr }
        }
        (None, _) => AttrExpansionFound::None,
    }
}

/// Handle `#[derive(...)]` attribute.
///
/// Returns a list of expansions that this plugin should apply.
fn parse_derive(
    db: &dyn SyntaxGroup,
    defined_derives: &[String],
    item_ast: ast::ModuleItem,
) -> Vec<String> {
    let attrs = match item_ast {
        ast::ModuleItem::Struct(struct_ast) => Some(struct_ast.query_attr(db, DERIVE_ATTR)),
        ast::ModuleItem::Enum(enum_ast) => Some(enum_ast.query_attr(db, DERIVE_ATTR)),
        _ => None,
    };

    attrs
        .unwrap_or_default()
        .iter()
        .map(|attr| attr.clone().structurize(db))
        .flat_map(|attr| attr.args.into_iter())
        .filter_map(|attr| {
            let AttributeArgVariant::Unnamed(value) = attr.clone().variant else {
                return None;
            };
            let Expr::Path(path) = value else {
                return None;
            };
            let path = path.elements(db);
            let path = path.last()?;
            let PathSegment::Simple(segment) = path else {
                return None;
            };
            let ident = segment.ident(db);
            let value = ident.text(db).to_string();

            defined_derives.iter().find(|derive| derive == &&value.to_case(Case::Snake)).cloned()
        })
        .collect()
}

fn expand_derives(
    db: &AnalysisDatabase,
    defined_derives: &[String],
    item_ast: ast::ModuleItem,
    stream_metadata: TokenStreamMetadata,
) -> Option<PluginResult> {
    let stable_ptr = item_ast.clone().stable_ptr().untyped();
    let span = item_ast.as_syntax_node().span(db);
    let token_stream =
        TokenStream::from_syntax_node(db, &item_ast).with_metadata(stream_metadata.clone());

    // All derives to be applied.
    let derives = parse_derive(db, defined_derives, item_ast.clone());
    let any_derives = !derives.is_empty();

    if any_derives {
        let result = db.get_derive_expansion(ExpandDeriveParams { derives, item: token_stream });

        return Some(PluginResult {
            code: if result.token_stream.is_empty() {
                None
            } else {
                let content = result.token_stream.to_string();

                Some(PluginGeneratedFile {
                    name: "proc_macro_derive".into(),
                    code_mappings: vec![CodeMapping {
                        origin: CodeOrigin::Span(span),
                        span: TextSpan {
                            start: TextOffset::default(),
                            end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                        },
                    }],
                    content,
                    aux_data: None,
                })
            },
            diagnostics: into_cairo_diagnostics(result.diagnostics, stable_ptr),
            // Note that we don't remove the original item here, unlike for attributes.
            // We do not add the original code to the generated file either.
            remove_original_item: false,
        });
    }

    None
}

fn expand_attribute(
    db: &AnalysisDatabase,
    name: String,
    last: bool,
    args: TokenStream,
    token_stream: TokenStream,
    span: TextSpan,
    stable_ptr: SyntaxStablePtrId,
) -> PluginResult {
    let result = db.get_attribute_expansion(ExpandAttributeParams {
        args,
        attr: name.clone(),
        item: token_stream.clone(),
    });

    // Handle token stream.
    if result.token_stream.is_empty() {
        // Remove original code
        return PluginResult {
            diagnostics: into_cairo_diagnostics(result.diagnostics, stable_ptr),
            code: None,
            remove_original_item: true,
        };
    }

    // This is a minor optimization.
    // If the expanded macro attribute is the only one that will be expanded by `ProcMacroHost`
    // in this `generate_code` call (i.e. all the other macro attributes has been expanded by
    // previous calls), and the expansion did not produce any changes, we can skip rewriting the
    // expanded node by simply returning no generated code, and leaving the original item as is.
    // However, if we have other macro attributes to expand, we must rewrite the node even if no
    // changes have been produced, so that we can parse the attributes once again and expand them.
    // In essence, `code: None, remove_original_item: false` means `ProcMacroHost` will not be
    // called again for this AST item.
    // This optimization limits the number of generated nodes a bit.
    if last && token_stream.to_string() == result.token_stream.to_string() {
        return PluginResult {
            code: None,
            remove_original_item: false,
            diagnostics: into_cairo_diagnostics(result.diagnostics, stable_ptr),
        };
    }

    let file_name = format!("proc_macro_{}", name);
    let content = result.token_stream.to_string();
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: file_name.into(),
            code_mappings: vec![CodeMapping {
                origin: CodeOrigin::Span(span),
                span: TextSpan {
                    start: TextOffset::default(),
                    end: TextOffset::default().add_width(TextWidth::from_str(&content)),
                },
            }],
            content,
            aux_data: None,
        }),
        diagnostics: into_cairo_diagnostics(result.diagnostics, stable_ptr),
        remove_original_item: true,
    }
}
