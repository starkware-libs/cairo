use std::collections::HashMap;

use cairo_lang_defs::plugin::{
    DynGeneratedFileAuxData, PluginDiagnostic, PluginGeneratedFile, PluginResult,
};
use cairo_lang_semantic::patcher::{ModifiedNode, PatchBuilder, RewriteNode};
use cairo_lang_semantic::plugin::DynPluginAuxData;
use cairo_lang_syntax::node::ast::{self, MaybeTraitBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};
use indoc::formatdoc;

use super::aux_data::StarkNetABIAuxData;
use super::consts::EVENT_ATTR;
use super::utils::is_ref_param;
use super::ABI_ATTR;

/// If the trait is annotated with ABI_ATTR, generate the relevant dispatcher logic.
pub fn handle_trait(db: &dyn SyntaxGroup, trait_ast: ast::ItemTrait) -> PluginResult {
    let attrs = trait_ast.attributes(db).elements(db);
    if !attrs.iter().any(|attr| attr.attr(db).text(db) == ABI_ATTR) {
        return PluginResult::default();
    }
    let body = match trait_ast.body(db) {
        MaybeTraitBody::Some(body) => body,
        MaybeTraitBody::None(empty_body) => {
            return PluginResult {
                code: None,
                diagnostics: vec![PluginDiagnostic {
                    message: "ABIs without body are not supported.".to_string(),
                    stable_ptr: empty_body.stable_ptr().untyped(),
                }],
                remove_original_item: false,
            };
        }
    };

    let mut diagnostics = vec![];
    let mut functions = vec![];
    for item_ast in body.items(db).elements(db) {
        match item_ast {
            ast::TraitItem::Function(func) => {
                // Ignore events.
                if func.has_attr(db, EVENT_ATTR) {
                    continue;
                }

                let declaration = func.declaration(db);

                let mut skip_generation = false;
                let mut serialization_code = vec![];
                let signature = declaration.signature(db);
                for param in signature.parameters(db).elements(db) {
                    if is_ref_param(db, &param) {
                        skip_generation = true;

                        diagnostics.push(PluginDiagnostic {
                            message: "`ref` parameters are not supported in the ABI of a contract."
                                .to_string(),
                            stable_ptr: param.modifiers(db).stable_ptr().untyped(),
                        })
                    }

                    let param_type = param.type_clause(db).ty(db);
                    let type_name = &param_type.as_syntax_node().get_text(db);
                    serialization_code.push(RewriteNode::interpolate_patched(
                        &formatdoc!(
                            "        serde::Serde::<{type_name}>::serialize(ref calldata, \
                             $arg_name$);\n"
                        ),
                        HashMap::from([(
                            "arg_name".to_string(),
                            RewriteNode::Trimmed(param.name(db).as_syntax_node()),
                        )]),
                    ));
                }

                if skip_generation {
                    // TODO(ilya): Consider generating an empty wrapper to avoid:
                    // Unknown function error.
                    continue;
                }

                let ret_decode = match signature.ret_ty(db) {
                    OptionReturnTypeClause::Empty(_) => "".to_string(),
                    OptionReturnTypeClause::ReturnTypeClause(ty) => {
                        let ret_type_ast = ty.ty(db);
                        let type_name = ret_type_ast.as_syntax_node().get_text(db);
                        format!(
                            "
        serde::Serde::<{type_name}>::deserialize(ref ret_data).expect(
            'Returned data too short')"
                        )
                    }
                };

                let mut func_declaration = RewriteNode::from_ast(&declaration);
                func_declaration
                    .modify_child(db, ast::FunctionDeclaration::INDEX_SIGNATURE)
                    .modify_child(db, ast::FunctionSignature::INDEX_PARAMETERS)
                    .modify(db)
                    .children
                    .splice(
                        0..0,
                        [
                            RewriteNode::Text("contract_address: ContractAddress".to_string()),
                            RewriteNode::Text(", ".to_string()),
                        ],
                    );

                functions.push(RewriteNode::interpolate_patched(
                    "$func_decl$ {
        let mut calldata = array_new();
$serialization_code$
        let mut ret_data = starknet::call_contract_syscall(
            contract_address,
            calldata,
        ).unwrap_syscall();
$deserialization_code$
    }
",
                    HashMap::from([
                        ("func_decl".to_string(), func_declaration),
                        (
                            "serialization_code".to_string(),
                            RewriteNode::Modified(ModifiedNode { children: serialization_code }),
                        ),
                        ("deserialization_code".to_string(), RewriteNode::Text(ret_decode)),
                    ]),
                ));
            }
        }
    }

    let mut builder = PatchBuilder::new(db);
    let dispatcher_name = format!("{}Dispatcher", trait_ast.name(db).text(db));
    builder.add_modified(RewriteNode::interpolate_patched(
        &formatdoc!(
            "mod {dispatcher_name} {{
                use starknet::SyscallResultTrait;
                use starknet::SyscallResultTraitImpl;
                use option::OptionTrait;
                use option::OptionTraitImpl;

            $body$
            }}",
        ),
        HashMap::from([(
            "body".to_string(),
            RewriteNode::Modified(ModifiedNode { children: functions }),
        )]),
    ));
    PluginResult {
        code: Some(PluginGeneratedFile {
            name: dispatcher_name.into(),
            content: builder.code,
            aux_data: DynGeneratedFileAuxData::new(DynPluginAuxData::new(StarkNetABIAuxData {
                patches: builder.patches,
            })),
        }),
        diagnostics,
        remove_original_item: false,
    }
}
