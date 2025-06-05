use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{TypedSyntaxNode, ast};

use super::utils::PluginTypeInfo;
use crate::plugins::DOC_ATTR;

mod clone;
mod debug;
mod default;
mod destruct;
mod hash;
mod panic_destruct;
mod partial_eq;
mod serde;

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct DerivePlugin;

const DERIVE_ATTR: &str = "derive";

impl MacroPlugin for DerivePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        generate_derive_code_for_type(
            db,
            metadata,
            match PluginTypeInfo::new(db, &item_ast) {
                Some(info) => info,
                None => {
                    let maybe_error = item_ast.find_attr(db, DERIVE_ATTR).map(|derive_attr| {
                        vec![PluginDiagnostic::error(
                            derive_attr.as_syntax_node().stable_ptr(db),
                            "`derive` may only be applied to `struct`s and `enum`s".to_string(),
                        )]
                    });

                    return PluginResult {
                        diagnostics: maybe_error.unwrap_or_default(),
                        ..PluginResult::default()
                    };
                }
            },
        )
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![DERIVE_ATTR.to_string(), default::DEFAULT_ATTR.to_string()]
    }

    fn declared_derives(&self) -> Vec<String> {
        vec![
            "Copy".to_string(),
            "Drop".to_string(),
            "Clone".to_string(),
            "Debug".to_string(),
            "Default".to_string(),
            "Destruct".to_string(),
            "Hash".to_string(),
            "PanicDestruct".to_string(),
            "PartialEq".to_string(),
            "Serde".to_string(),
        ]
    }
}

/// Adds an implementation for all requested derives for the type.
fn generate_derive_code_for_type(
    db: &dyn SyntaxGroup,
    metadata: &MacroPluginMetadata<'_>,
    info: PluginTypeInfo,
) -> PluginResult {
    let mut diagnostics = vec![];
    let mut builder = PatchBuilder::new(db, &info.attributes);
    for attr in info.attributes.query_attr(db, DERIVE_ATTR) {
        let attr = attr.structurize(db);

        if attr.args.is_empty() {
            diagnostics
                .push(PluginDiagnostic::error(attr.args_stable_ptr, "Expected args.".into()));
            continue;
        }

        for arg in attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed(ast::Expr::Path(derived_path)),
                ..
            } = arg
            else {
                diagnostics
                    .push(PluginDiagnostic::error(arg.arg.stable_ptr(db), "Expected path.".into()));
                continue;
            };

            let derived = derived_path.as_syntax_node().get_text_without_trivia(db);
            if let Some(mut code) = match derived.as_str() {
                "Copy" | "Drop" => Some(get_empty_impl(&derived, &info)),
                "Clone" => Some(clone::handle_clone(&info)),
                "Debug" => Some(debug::handle_debug(&info)),
                "Default" => default::handle_default(db, &info, &derived_path, &mut diagnostics),
                "Destruct" => Some(destruct::handle_destruct(&info)),
                "Hash" => Some(hash::handle_hash(&info)),
                "PanicDestruct" => Some(panic_destruct::handle_panic_destruct(&info)),
                "PartialEq" => Some(partial_eq::handle_partial_eq(&info)),
                "Serde" => Some(serde::handle_serde(&info)),
                _ => {
                    if !metadata.declared_derives.contains(&derived) {
                        diagnostics.push(PluginDiagnostic::error(
                            derived_path.stable_ptr(db),
                            format!("Unknown derive `{derived}` - a plugin might be missing."),
                        ));
                    }
                    None
                }
            } {
                if let Some(doc_attr) = info.attributes.find_attr(db, DOC_ATTR) {
                    code =
                        format!("{}\n{code}", doc_attr.as_syntax_node().get_text_without_trivia(db))
                }
                builder.add_modified(RewriteNode::mapped_text(code, db, &derived_path));
            }
        }
    }
    let (content, code_mappings) = builder.build();
    PluginResult {
        code: (!content.is_empty()).then(|| PluginGeneratedFile {
            name: "impls".into(),
            code_mappings,
            content,
            aux_data: None,
            diagnostics_note: Default::default(),
        }),
        diagnostics,
        remove_original_item: false,
    }
}

fn get_empty_impl(derived_trait: &str, info: &PluginTypeInfo) -> String {
    let derive_trait = format!("core::traits::{derived_trait}");
    format!("{};\n", info.impl_header(&derive_trait, &[&derive_trait]))
}
