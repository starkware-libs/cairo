use std::vec;

use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_filesystem::cfg::{Cfg, CfgSet};
use cairo_lang_syntax::attribute::structured::{
    Attribute, AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{BodyItems, QueryAttrs};
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;

/// Represents a predicate tree used to evaluate configuration attributes to handle nested
/// predicates, such as logical `not` operations, and evaluate them based on a given set of
/// configuration flags (`CfgSet`).
#[derive(Debug, Clone)]
enum PredicateTree {
    Cfg(Cfg),
    Not(Box<PredicateTree>),
}

impl PredicateTree {
    fn evaluate(&self, cfg_set: &CfgSet) -> bool {
        match self {
            PredicateTree::Cfg(cfg) => cfg_set.contains(cfg),
            PredicateTree::Not(inner) => !inner.evaluate(cfg_set),
        }
    }
}
/// Plugin that enables ignoring modules not involved in the current config.
///
/// Mostly useful for marking test modules to prevent usage of their functionality out of tests,
/// and reduce compilation time when the tests data isn't required.
#[derive(Debug, Default)]
#[non_exhaustive]
pub struct ConfigPlugin;

const CFG_ATTR: &str = "cfg";

impl MacroPlugin for ConfigPlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let mut diagnostics = vec![];

        if should_drop(db, metadata.cfg_set, &item_ast, &mut diagnostics) {
            PluginResult { code: None, diagnostics, remove_original_item: true }
        } else if let Some(builder) =
            handle_undropped_item(db, metadata.cfg_set, item_ast, &mut diagnostics)
        {
            let (content, code_mappings) = builder.build();
            PluginResult {
                code: Some(PluginGeneratedFile {
                    name: "config".into(),
                    content,
                    code_mappings,
                    aux_data: None,
                    diagnostics_note: Default::default(),
                }),
                diagnostics,
                remove_original_item: true,
            }
        } else {
            PluginResult { code: None, diagnostics, remove_original_item: false }
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![CFG_ATTR.to_string()]
    }
}

/// Iterator over the items that are included in the given config set, among the given items in
/// `iterator`.
pub struct ItemsInCfg<'a, Item: QueryAttrs> {
    db: &'a dyn SyntaxGroup,
    cfg_set: &'a CfgSet,
    iterator: <Vec<Item> as IntoIterator>::IntoIter,
}

impl<Item: QueryAttrs> Iterator for ItemsInCfg<'_, Item> {
    type Item = Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iterator.find(|item| !should_drop(self.db, self.cfg_set, item, &mut vec![]))
    }
}

/// Extension trait for `BodyItems` filtering out items that are not included in the cfg.
pub trait HasItemsInCfgEx<Item: QueryAttrs>: BodyItems<Item = Item> {
    fn iter_items_in_cfg<'a>(
        &self,
        db: &'a dyn SyntaxGroup,
        cfg_set: &'a CfgSet,
    ) -> ItemsInCfg<'a, Item>;
}

impl<Item: QueryAttrs, Body: BodyItems<Item = Item>> HasItemsInCfgEx<Item> for Body {
    fn iter_items_in_cfg<'a>(
        &self,
        db: &'a dyn SyntaxGroup,
        cfg_set: &'a CfgSet,
    ) -> ItemsInCfg<'a, Item> {
        ItemsInCfg { db, cfg_set, iterator: self.items_vec(db).into_iter() }
    }
}

/// Handles an item that is not dropped from the AST completely due to not matching the config.
/// In case it includes dropped elements and needs to be rewritten, it returns the appropriate
/// PatchBuilder. Otherwise returns `None`, and it won't be rewritten or dropped.
fn handle_undropped_item<'a>(
    db: &'a dyn SyntaxGroup,
    cfg_set: &CfgSet,
    item_ast: ast::ModuleItem,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<PatchBuilder<'a>> {
    match item_ast {
        ast::ModuleItem::Trait(trait_item) => {
            let body = try_extract_matches!(trait_item.body(db), ast::MaybeTraitBody::Some)?;
            let items = get_kept_items_nodes(db, cfg_set, &body.items_vec(db), diagnostics)?;
            let mut builder = PatchBuilder::new(db, &trait_item);
            builder.add_node(trait_item.attributes(db).as_syntax_node());
            builder.add_node(trait_item.trait_kw(db).as_syntax_node());
            builder.add_node(trait_item.name(db).as_syntax_node());
            builder.add_node(trait_item.generic_params(db).as_syntax_node());
            builder.add_node(body.lbrace(db).as_syntax_node());
            for item in items {
                builder.add_node(item);
            }
            builder.add_node(body.rbrace(db).as_syntax_node());
            Some(builder)
        }
        ast::ModuleItem::Impl(impl_item) => {
            let body = try_extract_matches!(impl_item.body(db), ast::MaybeImplBody::Some)?;
            let items = get_kept_items_nodes(db, cfg_set, &body.items_vec(db), diagnostics)?;
            let mut builder = PatchBuilder::new(db, &impl_item);
            builder.add_node(impl_item.attributes(db).as_syntax_node());
            builder.add_node(impl_item.impl_kw(db).as_syntax_node());
            builder.add_node(impl_item.name(db).as_syntax_node());
            builder.add_node(impl_item.generic_params(db).as_syntax_node());
            builder.add_node(impl_item.of_kw(db).as_syntax_node());
            builder.add_node(impl_item.trait_path(db).as_syntax_node());
            builder.add_node(body.lbrace(db).as_syntax_node());
            for item in items {
                builder.add_node(item);
            }
            builder.add_node(body.rbrace(db).as_syntax_node());
            Some(builder)
        }
        _ => None,
    }
}

/// Gets the list of items that should be kept in the AST.
/// Returns `None` if all items should be kept.
fn get_kept_items_nodes<Item: QueryAttrs + TypedSyntaxNode>(
    db: &dyn SyntaxGroup,
    cfg_set: &CfgSet,
    all_items: &[Item],
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<Vec<cairo_lang_syntax::node::SyntaxNode>> {
    let mut any_dropped = false;
    let mut kept_items_nodes = vec![];
    for item in all_items {
        if should_drop(db, cfg_set, item, diagnostics) {
            any_dropped = true;
        } else {
            kept_items_nodes.push(item.as_syntax_node());
        }
    }
    if any_dropped { Some(kept_items_nodes) } else { None }
}

/// Check if the given item should be dropped from the AST.
fn should_drop<Item: QueryAttrs>(
    db: &dyn SyntaxGroup,
    cfg_set: &CfgSet,
    item: &Item,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> bool {
    item.query_attr(db, CFG_ATTR).into_iter().any(|attr| {
        if let Some(predicate_tree) = parse_predicate(db, attr.structurize(db), diagnostics) {
            !predicate_tree.evaluate(cfg_set)
        } else {
            false
        }
    })
}

/// Parse `#[cfg(not(ghf)...)]` attribute arguments as a predicate matching [`Cfg`] items.
fn parse_predicate(
    db: &dyn SyntaxGroup,
    attr: Attribute,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<PredicateTree> {
    let predicates: Vec<_> = attr
        .args
        .into_iter()
        .filter_map(|arg| {
            // NOTE: Try to parse each item eagerly, so that we will report any possible issues for
            // all   arguments at once. Take into account that Rust's
            // `Iterator::collect::<Option<_>>`   by itself would stop collection on
            // first `None`.
            let result = parse_predicate_item(db, &arg, diagnostics);
            if result.is_none() {
                diagnostics.push(PluginDiagnostic::error(
                    arg.arg.as_syntax_node().stable_ptr(),
                    "Failed to parse argument.".into(),
                ));
            }
            result
        })
        .collect();
    if predicates.len() == 1 { Some(predicates.into_iter().next().unwrap()) } else { None }
}

/// Parse single `#[cfg(...)]` attribute argument as a [`Cfg`] item.
fn parse_predicate_item(
    db: &dyn SyntaxGroup,
    arg: &AttributeArg,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<PredicateTree> {
    match &arg.variant {
        AttributeArgVariant::FieldInitShorthand(_) => {
            diagnostics.push(PluginDiagnostic::error(
                arg.arg.as_syntax_node().stable_ptr(),
                "This attribute does not support field initialization shorthands.".into(),
            ));
            None
        }
        AttributeArgVariant::Named { name, value } => {
            let value_text = match value {
                ast::Expr::ShortString(terminal) => terminal.string_value(db).unwrap_or_default(),
                ast::Expr::String(terminal) => terminal.string_value(db).unwrap_or_default(),
                _ => {
                    diagnostics.push(PluginDiagnostic::error(
                        value.as_syntax_node().stable_ptr(),
                        "Expected a string or short-string literal.".into(),
                    ));
                    return None;
                }
            };
            Some(PredicateTree::Cfg(Cfg::kv(name.text.to_string(), value_text)))
        }
        AttributeArgVariant::Unnamed(value) => {
            if let ast::Expr::FunctionCall(call) = value {
                let path = call.path(db);
                if let Some(ast::PathSegment::Simple(segment)) = path.elements(db).first() {
                    let function_name = segment.ident(db).text(db);

                    if function_name == "not" {
                        let args = call.arguments(db).arguments(db).elements(db);

                        let mut predicates = vec![];
                        for arg in args {
                            let arg_clause = arg.arg_clause(db);

                            match arg_clause {
                                ast::ArgClause::Unnamed(inner_expr) => {
                                    let inner_attribute_arg = AttributeArg {
                                        variant: AttributeArgVariant::Unnamed(inner_expr.value(db)),
                                        arg: arg.clone(),
                                        modifiers: vec![],
                                    };

                                    if let Some(parsed) =
                                        parse_predicate_item(db, &inner_attribute_arg, diagnostics)
                                    {
                                        predicates.push(parsed);
                                    }
                                }
                                ast::ArgClause::Named(named) => {
                                    let key_terminal = named.name(db);
                                    let value_expr = named.value(db);

                                    let key_text = key_terminal.text(db);
                                    let value_text = match value_expr {
                                        ast::Expr::ShortString(terminal) => {
                                            terminal.string_value(db).unwrap_or_default()
                                        }
                                        ast::Expr::String(terminal) => {
                                            terminal.string_value(db).unwrap_or_default()
                                        }
                                        _ => {
                                            diagnostics.push(PluginDiagnostic::error(
                                                value_expr.as_syntax_node().stable_ptr(),
                                                "Expected a string or short-string literal.".into(),
                                            ));
                                            return None;
                                        }
                                    };

                                    predicates.push(PredicateTree::Cfg(Cfg::kv(
                                        key_text.to_string(),
                                        value_text,
                                    )));
                                }
                                _ => {
                                    diagnostics.push(PluginDiagnostic::error(
                                        arg_clause.as_syntax_node().stable_ptr(),
                                        "Expected unnamed or named ArgClause containing an \
                                         expression."
                                            .into(),
                                    ));
                                    return None;
                                }
                            }
                        }

                        if predicates.is_empty() {
                            diagnostics.push(PluginDiagnostic::error(
                                call.as_syntax_node().stable_ptr(),
                                "`not` operator requires at least one argument.".into(),
                            ));
                            return None;
                        }

                        let combined = if predicates.len() == 1 {
                            predicates.into_iter().next().unwrap()
                        } else {
                            PredicateTree::Not(Box::new(PredicateTree::Cfg(Cfg::name(
                                "group".to_string(),
                            ))))
                        };

                        return Some(PredicateTree::Not(Box::new(combined)));
                    }
                }
            }

            if let ast::Expr::Path(path) = value {
                if let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] {
                    let key = segment.ident(db).text(db);
                    return Some(PredicateTree::Cfg(Cfg::name(key.to_string())));
                }
            }

            diagnostics.push(PluginDiagnostic::error(
                value.as_syntax_node().stable_ptr(),
                "Expected `not(...)` or identifier.".into(),
            ));
            None
        }
    }
}
