use cairo_lang_defs::ids::ModuleId;
use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    MacroPlugin, MacroPluginMetadata, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::plugin::{AnalyzerPlugin, PluginSuite};
use cairo_lang_semantic::{GenericArgumentId, Mutability, corelib};
use cairo_lang_syntax::attribute::consts::IMPLICIT_PRECEDENCE_ATTR;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{OptionWrappedGenericParamListHelper, QueryAttrs};
use cairo_lang_syntax::node::{TypedStablePtr, ast};
use indoc::formatdoc;
use itertools::Itertools;

pub const EXECUTABLE_ATTR: &str = "executable";
pub const EXECUTABLE_RAW_ATTR: &str = "executable_raw";
pub const EXECUTABLE_PREFIX: &str = "__executable_wrapper__";

/// Returns a plugin suite with the `ExecutablePlugin` and `RawExecutableAnalyzer`.
pub fn executable_plugin_suite() -> PluginSuite {
    std::mem::take(
        PluginSuite::default()
            .add_plugin::<ExecutablePlugin>()
            .add_analyzer_plugin::<RawExecutableAnalyzer>(),
    )
}

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
struct ExecutablePlugin;

impl MacroPlugin for ExecutablePlugin {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        item_ast: ast::ModuleItem,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> PluginResult {
        let ast::ModuleItem::FreeFunction(item) = item_ast else {
            return PluginResult::default();
        };
        if !item.has_attr(db, EXECUTABLE_ATTR) {
            return PluginResult::default();
        }
        let mut diagnostics = vec![];
        let mut builder = PatchBuilder::new(db, &item);
        let declaration = item.declaration(db);
        let generics = declaration.generic_params(db);
        if !generics.is_empty(db) {
            diagnostics.push(PluginDiagnostic::error(
                &generics,
                "Executable functions cannot have generic params.".to_string(),
            ));
        }
        let name = declaration.name(db);
        let implicits_precedence =
            RewriteNode::Text(format!("#[{IMPLICIT_PRECEDENCE_ATTR}({})]", {
                IMPLICIT_PRECEDENCE.iter().join(", ")
            }));
        builder.add_modified(RewriteNode::interpolate_patched(&formatdoc! {"

                $implicit_precedence$
                #[{EXECUTABLE_RAW_ATTR}]
                fn {EXECUTABLE_PREFIX}$function_name$(mut input: Span<felt252>, ref output: Array<felt252>) {{\n
            "},
            &[
                ("implicit_precedence".into(), implicits_precedence,),
                ("function_name".into(), RewriteNode::from_ast(&name))
            ].into()
        ));
        let params = declaration.signature(db).parameters(db).elements(db);
        for (param_idx, param) in params.iter().enumerate() {
            for modifier in param.modifiers(db).elements(db) {
                if let ast::Modifier::Ref(terminal_ref) = modifier {
                    diagnostics.push(PluginDiagnostic::error(
                        &terminal_ref,
                        "Parameters of an `#[executable]` function can't be `ref`.".into(),
                    ));
                }
            }
            builder.add_modified(
                RewriteNode::Text(format!(
                    "    let __param{EXECUTABLE_PREFIX}{param_idx} = Serde::deserialize(ref \
                     input).expect('Failed to deserialize param #{param_idx}');\n"
                ))
                .mapped(db, param),
            );
        }
        if !diagnostics.is_empty() {
            return PluginResult { code: None, diagnostics, remove_original_item: false };
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
                RewriteNode::Text(format!("        __param{EXECUTABLE_PREFIX}{param_idx},\n"))
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
                name: "executable".into(),
                content,
                code_mappings,
                aux_data: None,
                diagnostics_note: Default::default(),
            }),
            diagnostics,
            remove_original_item: false,
        }
    }

    fn declared_attributes(&self) -> Vec<String> {
        vec![EXECUTABLE_ATTR.to_string(), EXECUTABLE_RAW_ATTR.to_string()]
    }

    fn executable_attributes(&self) -> Vec<String> {
        vec![EXECUTABLE_RAW_ATTR.to_string()]
    }
}

/// Plugin to add diagnostics on bad `#[executable_raw]` annotations.
#[derive(Default, Debug)]
struct RawExecutableAnalyzer;

impl AnalyzerPlugin for RawExecutableAnalyzer {
    fn diagnostics(&self, db: &dyn SemanticGroup, module_id: ModuleId) -> Vec<PluginDiagnostic> {
        let syntax_db = db.upcast();
        let mut diagnostics = vec![];
        let Ok(free_functions) = db.module_free_functions(module_id) else {
            return diagnostics;
        };
        for (id, item) in free_functions.iter() {
            if !item.has_attr(syntax_db, EXECUTABLE_RAW_ATTR) {
                continue;
            }
            let Ok(signature) = db.free_function_signature(*id) else {
                continue;
            };
            if signature.return_type != corelib::unit_ty(db) {
                diagnostics.push(PluginDiagnostic::error(
                    &signature.stable_ptr.lookup(syntax_db).ret_ty(syntax_db),
                    "Invalid return type for `#[executable_raw]` function, expected `()`."
                        .to_string(),
                ));
            }
            let [input, output] = &signature.params[..] else {
                diagnostics.push(PluginDiagnostic::error(
                    &signature.stable_ptr.lookup(syntax_db).parameters(syntax_db),
                    "Invalid number of params for `#[executable_raw]` function, expected 2."
                        .to_string(),
                ));
                continue;
            };
            if input.ty
                != corelib::get_core_ty_by_name(
                    db,
                    "Span".into(),
                    vec![GenericArgumentId::Type(db.core_info().felt252)],
                )
            {
                diagnostics.push(PluginDiagnostic::error(
                    input.stable_ptr.untyped(),
                    "Invalid first param type for `#[executable_raw]` function, expected \
                     `Span<felt252>`."
                        .to_string(),
                ));
            }
            if input.mutability == Mutability::Reference {
                diagnostics.push(PluginDiagnostic::error(
                    input.stable_ptr.untyped(),
                    "Invalid first param mutability for `#[executable_raw]` function, got \
                     unexpected `ref`."
                        .to_string(),
                ));
            }
            if output.ty != corelib::core_array_felt252_ty(db) {
                diagnostics.push(PluginDiagnostic::error(
                    output.stable_ptr.untyped(),
                    "Invalid second param type for `#[executable_raw]` function, expected \
                     `Array<felt252>`."
                        .to_string(),
                ));
            }
            if output.mutability != Mutability::Reference {
                diagnostics.push(PluginDiagnostic::error(
                    output.stable_ptr.untyped(),
                    "Invalid second param mutability for `#[executable_raw]` function, expected \
                     `ref`."
                        .to_string(),
                ));
            }
        }
        diagnostics
    }
}
