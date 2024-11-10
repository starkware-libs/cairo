use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_syntax::attribute::consts::IMPLICIT_PRECEDENCE_ATTR;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{OptionWrappedGenericParamListHelper, QueryAttrs};
use indoc::formatdoc;
use itertools::Itertools;

pub const RUNNABLE_ATTR: &str = "runnable";
pub const RUNNABLE_RAW_ATTR: &str = "runnable_raw";
pub const RUNNABLE_PREFIX: &str = "__runnable_wrapper__";
const IMPLICIT_PRECEDENCE: &[&str] = &[
    "core::pedersen::Pedersen",
    "core::RangeCheck",
    "core::integer::Bitwise",
    "core::ec::EcOp",
    "core::poseidon::Poseidon",
    "core::circuit::RangeCheck96",
    "core::circuit::AddMod",
    "core::circuit::MulMod",
];

#[derive(Debug, Default)]
#[non_exhaustive]
pub struct RunnablePlugin;

impl MacroPlugin for RunnablePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let ast::ModuleItem::FreeFunction(item) = item_ast else {
            return PluginResult::default();
        };
        if !item.has_attr(db, RUNNABLE_ATTR) {
            return PluginResult::default();
        }
        let mut diagnostics = vec![];
        let mut builder = PatchBuilder::new(db, &item);
        let declaration = item.declaration(db);
        let generics = declaration.generic_params(db);
        if !generics.is_empty(db) {
            diagnostics.push(PluginDiagnostic::error(
                &generics,
                "Runnable functions cannot have generic params.".to_string(),
            ));
            return PluginResult { code: None, diagnostics, remove_original_item: false };
        }
        let name = declaration.name(db);
        let implicits_precedence =
            RewriteNode::Text(format!("#[{IMPLICIT_PRECEDENCE_ATTR}({})]", {
                IMPLICIT_PRECEDENCE.iter().join(", ")
            }));
        builder.add_modified(RewriteNode::interpolate_patched(&formatdoc! {"

                $implicit_precedence$
                #[{RUNNABLE_RAW_ATTR}]
                fn {RUNNABLE_PREFIX}$function_name$(mut input: Span<felt252>, ref output: Array<felt252>) {{\n
            "},
            &[
                ("implicit_precedence".into(), implicits_precedence,),
                ("function_name".into(), RewriteNode::from_ast(&name))
            ].into()
        ));
        let params = declaration.signature(db).parameters(db).elements(db);
        for (param_idx, param) in params.iter().enumerate() {
            builder.add_modified(
                RewriteNode::Text(format!(
                    "    let __param{RUNNABLE_PREFIX}{param_idx} = Serde::deserialize(ref \
                     input).expect('Failed to deserialize param #{param_idx}');\n"
                ))
                .mapped(db, param),
            );
        }
        builder.add_str(
            "    assert(core::array::SpanTrait::is_empty(input), 'Input too long for params.');\n",
        );
        builder.add_modified(RewriteNode::interpolate_patched(
            "    let __result = @$function_name$(\n",
            &[("function_name".into(), RewriteNode::from_ast(&name))].into(),
        ));
        for (param_idx, param) in params.iter().enumerate() {
            builder.add_modified(
                RewriteNode::Text(format!("        __param{RUNNABLE_PREFIX}{param_idx},\n"))
                    .mapped(db, param),
            );
        }
        builder.add_str("    );\n");
        let mut serialize_node = RewriteNode::text("    Serde::serialize(__result, ref output);\n");
        if let ast::OptionReturnTypeClause::ReturnTypeClause(clause) =
            declaration.signature(db).ret_ty(db)
        {
            serialize_node = serialize_node.mapped(db, &clause);
        }
        builder.add_modified(serialize_node);
        builder.add_str("}\n");
        let (content, code_mappings) = builder.build();
        PluginResult {
            code: Some(PluginGeneratedFile {
                name: "runnable".into(),
                content,
                code_mappings,
                aux_data: None,
            }),
            diagnostics,
            remove_original_item: false,
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![RUNNABLE_ATTR.to_string(), RUNNABLE_RAW_ATTR.to_string()]
    }

    fn executable_attributes(&self) -> Vec<String> {
        vec![RUNNABLE_RAW_ATTR.to_string()]
    }
}
