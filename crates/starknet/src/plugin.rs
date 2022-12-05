use defs::db::{MacroPlugin, PluginResult};
use genco::prelude::*;
use itertools::join;
use syntax::node::ast::{MaybeImplBody, Param};
use syntax::node::db::SyntaxGroup;
use syntax::node::helpers::GetIdentifier;
use syntax::node::{ast, Terminal, TypedSyntaxNode};

static CONTRACT_IMPL_ATTR: &str = "ContractImpl";

#[cfg(test)]
#[path = "plugin_test.rs"]
mod test;

#[derive(Debug)]
pub struct StarkNetPlugin {}

impl MacroPlugin for StarkNetPlugin {
    // TODO(yuval): diagnostic for each early return.
    fn generate_code(&self, db: &dyn SyntaxGroup, item_ast: ast::Item) -> PluginResult {
        let impl_ast = match item_ast {
            ast::Item::Impl(impl_ast) => impl_ast,
            _ => return PluginResult::default(),
        };

        let attrs = impl_ast.attributes(db).elements(db);
        if !attrs.iter().any(|attr| attr.attr(db).text(db) == CONTRACT_IMPL_ATTR) {
            return PluginResult::default();
        }

        let body = match impl_ast.body(db) {
            MaybeImplBody::Some(body) => body,
            MaybeImplBody::None(_) => return PluginResult::default(),
        };

        let items = body.items(db);
        let mut functions_tokens = rust::Tokens::new();
        functions_tokens.append(items.as_syntax_node().get_text(db).as_str());

        for item in items.elements(db) {
            let item_function = match item {
                ast::Item::FreeFunction(f) => f,
                _ => break,
            };
            let sig = item_function.signature(db);
            let params = sig.parameters(db).elements(db);
            // TODO(yuval): support types of size >1.
            let params_len = params.len();
            let mut params_iter = params.into_iter();

            // Assert the first parameter is SyscallPtr.
            // TODO(yuval): get_text includes trivia... This wouldn't always work.
            let first_param = params_iter.next().unwrap();
            let first_param_type = first_param.type_clause(db).ty(db).as_syntax_node().get_text(db);
            if first_param_type != "SyscallPtr" {
                return PluginResult::default();
            }
            if !is_ref_param(db, &first_param) {
                return PluginResult::default();
            }

            let mut arg_names = Vec::new();
            let mut arg_definitions = quote! {};
            let mut ref_appends = quote! {};
            for (idx, param) in params_iter.enumerate() {
                let arg_name = format!("__arg_{}", param.name(db).identifier(db));
                let is_ref = is_ref_param(db, &param);

                arg_names.push(arg_name.clone());
                let mut_modifier = if is_ref { "mut " } else { "" };
                // TODO(yuval): use panicable version of `array_at` once panic_with supports generic
                // params.
                arg_definitions.append(quote! {let $mut_modifier$(arg_name.clone()): felt = match array::array_at::<felt>(data, $idx) {
                    Some(x) => x,
                    None(()) => panic(array::array_new::<felt>()),
                };});

                if is_ref {
                    ref_appends.append(quote! {array::array_append::<felt>(arr, $arg_name);});
                }
            }
            let param_names_tokens = join(arg_names.into_iter(), ", ");

            let wrapped_name = item_function.name(db).text(db).to_string();
            let wrapper_name = format!("__wrapper_{}", wrapped_name.clone());
            // TODO(yuval): change to != once it's supported.
            // TODO(yuval): change to uint128 literal once it's supported.
            functions_tokens.append(quote! {
                func $wrapper_name(ref syscall_ptr: SyscallPtr, data: Array::<felt>) -> Array::<felt> {
                    if array::array_len::<felt>(data) == integer::uint128_from_felt($params_len) {
                    } else {
                        panic(array::array_new::<felt>());
                    }
                    $arg_definitions
                    let res = $wrapped_name(syscall_ptr, $param_names_tokens);
                    let mut arr = array::array_new::<felt>();
                    $ref_appends
                    array::array_append::<felt>(arr, res);
                    arr
                }
            });
        }

        PluginResult {
            code: Some(("entry_points".into(), functions_tokens.to_string().unwrap())),
            diagnostics: vec![],
        }
    }
}

fn is_ref_param(db: &dyn SyntaxGroup, param: &Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    param_modifiers.len() == 1 && param_modifiers[0].identifier(db) == "ref"
}
