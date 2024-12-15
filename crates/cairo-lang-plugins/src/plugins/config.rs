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
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::try_extract_matches;
use itertools::Itertools;

/// Represents a predicate tree used to evaluate configuration attributes to handle nested
/// predicates, such as logical `not` operations, and evaluate them based on a given set of
/// configuration flags (`CfgSet`).
#[derive(Debug, Clone)]
enum PredicateTree {
    Cfg(Cfg),
    Not(Box<PredicateTree>),
    And(Vec<PredicateTree>),
    Or(Vec<PredicateTree>),
}

impl PredicateTree {
    /// Evaluates the predicate tree against the provided configuration set (`CfgSet`) by traversing
    /// the `PredicateTree` and determines whether the predicate is satisfied by the given
    /// `cfg_set`.
    fn evaluate(&self, cfg_set: &CfgSet) -> bool {
        match self {
            PredicateTree::Cfg(cfg) => cfg_set.contains(cfg),
            PredicateTree::Not(inner) => !inner.evaluate(cfg_set),
            PredicateTree::And(predicates) => predicates.iter().all(|p| p.evaluate(cfg_set)),
            PredicateTree::Or(predicates) => predicates.iter().any(|p| p.evaluate(cfg_set)),
        }
    }
}

/// Represents a part of a configuration predicate.
pub enum ConfigPredicatePart {
    /// A configuration item, either a key-value pair or a simple name.
    Cfg(Cfg),
    /// A function call in the predicate (`not`, `and`, `or`).
    Call(ast::ExprFunctionCall),
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
        match parse_predicate(db, attr.structurize(db), diagnostics) {
            Some(predicate_tree) => !predicate_tree.evaluate(cfg_set),
            None => false,
        }
    })
}

/// Parse `#[cfg(not(ghf)...)]` attribute arguments as a predicate matching [`Cfg`] items.
fn parse_predicate(
    db: &dyn SyntaxGroup,
    attr: Attribute,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<PredicateTree> {
    Some(PredicateTree::And(
        attr.args
            .into_iter()
            .filter_map(|arg| parse_predicate_item(db, arg, diagnostics))
            .collect(),
    ))
}

/// Parse single `#[cfg(...)]` attribute argument as a [`Cfg`] item.
fn parse_predicate_item(
    db: &dyn SyntaxGroup,
    item: AttributeArg,
    diagnostics: &mut Vec<PluginDiagnostic>,
) -> Option<PredicateTree> {
    match extract_config_predicate_part(db, &item) {
        Some(ConfigPredicatePart::Cfg(cfg)) => Some(PredicateTree::Cfg(cfg)),
        Some(ConfigPredicatePart::Call(call)) => {
            let operator = call.path(db).as_syntax_node().get_text(db);
            let args = call
                .arguments(db)
                .arguments(db)
                .elements(db)
                .iter()
                .map(|arg| AttributeArg::from_ast(arg.clone(), db))
                .collect_vec();

            match operator.as_str() {
                "not" => {
                    if args.len() != 1 {
                        diagnostics.push(PluginDiagnostic::error(
                            call.stable_ptr(),
                            "`not` operator expects exactly one argument.".into(),
                        ));
                        None
                    } else {
                        Some(PredicateTree::Not(Box::new(parse_predicate_item(
                            db,
                            args[0].clone(),
                            diagnostics,
                        )?)))
                    }
                }
                "and" => {
                    if args.len() < 2 {
                        diagnostics.push(PluginDiagnostic::error(
                            call.stable_ptr(),
                            "`and` operator expects at least two arguments.".into(),
                        ));
                        None
                    } else {
                        Some(PredicateTree::And(
                            args.into_iter()
                                .filter_map(|arg| parse_predicate_item(db, arg, diagnostics))
                                .collect(),
                        ))
                    }
                }
                "or" => {
                    if args.len() < 2 {
                        diagnostics.push(PluginDiagnostic::error(
                            call.stable_ptr(),
                            "`or` operator expects at least two arguments.".into(),
                        ));
                        None
                    } else {
                        Some(PredicateTree::Or(
                            args.into_iter()
                                .filter_map(|arg| parse_predicate_item(db, arg, diagnostics))
                                .collect(),
                        ))
                    }
                }
                _ => {
                    diagnostics.push(PluginDiagnostic::error(
                        call.stable_ptr(),
                        format!("Unsupported operator: `{}`.", operator),
                    ));
                    None
                }
            }
        }
        None => {
            diagnostics.push(PluginDiagnostic::error(
                item.arg.stable_ptr().untyped(),
                "Invalid configuration argument.".into(),
            ));
            None
        }
    }
}

/// Extracts a configuration predicate part from an attribute argument.
fn extract_config_predicate_part(
    db: &dyn SyntaxGroup,
    arg: &AttributeArg,
) -> Option<ConfigPredicatePart> {
    match &arg.variant {
        AttributeArgVariant::Unnamed(ast::Expr::Path(path)) => {
            let segments = path.elements(db);
            if let [ast::PathSegment::Simple(segment)] = &segments[..] {
                Some(ConfigPredicatePart::Cfg(Cfg::name(segment.ident(db).text(db).to_string())))
            } else {
                None
            }
        }
        AttributeArgVariant::Unnamed(ast::Expr::FunctionCall(call)) => {
            Some(ConfigPredicatePart::Call(call.clone()))
        }
        AttributeArgVariant::Named { name, value } => {
            let value_text = match value {
                ast::Expr::String(terminal) => terminal.string_value(db).unwrap_or_default(),
                ast::Expr::ShortString(terminal) => terminal.string_value(db).unwrap_or_default(),
                _ => return None,
            };

            Some(ConfigPredicatePart::Cfg(Cfg::kv(name.text.to_string(), value_text)))
        }
        _ => None,
    }
}
