use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::{ModifiedNode, RewriteNode};
use cairo_lang_syntax::node::ast::{FunctionWithBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};

use super::utils::is_ref_param;

/// Generates Cairo code for an entry point wrapper.
pub fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &FunctionWithBody,
) -> Result<RewriteNode, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);
    let diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();
    let input_data_short_err = "'Input too short for arguments'";
    for param in params {
        let arg_name = format!("__arg_{}", param.name(db).text(db));
        let arg_type_ast = param.type_clause(db).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db);

        let is_ref = is_ref_param(db, &param);
        let ref_modifier = if is_ref { "ref " } else { "" };
        arg_names.push(format!("{ref_modifier}{arg_name}"));
        let mut_modifier = if is_ref { "mut " } else { "" };
        // TODO(yuval): use panicable version of deserializations when supported.
        let arg_definition = format!(
            "
            let {mut_modifier}{arg_name} =
                match serde::Serde::<{type_name}>::deserialize(ref data) {{
                    Option::Some(x) => x,
                    Option::None(()) => {{
                        let mut err_data = array_new();
                        array_append(ref err_data, {input_data_short_err});
                        panic(err_data)
                    }},
                }};"
        );
        arg_definitions.push(arg_definition);

        if is_ref {
            ref_appends.push(RewriteNode::Text(format!(
                "\n            serde::Serde::<{type_name}>::serialize(ref arr, {arg_name});"
            )));
        }
    }
    let arg_names_str = arg_names.join(", ");

    let function_name = RewriteNode::Trimmed(declaration.name(db).as_syntax_node());
    let wrapped_name = RewriteNode::interpolate_patched(
        "super::$function_name$",
        HashMap::from([("function_name".to_string(), function_name.clone())]),
    );
    let (let_res, append_res) = match sig.ret_ty(db) {
        OptionReturnTypeClause::Empty(_) => ("", "".to_string()),
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_ast = ty.ty(db);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db);
            (
                "\n            let res = ",
                format!("\n            serde::Serde::<{ret_type_name}>::serialize(ref arr, res);"),
            )
        }
    };
    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";

    let arg_definitions = arg_definitions.join("\n");
    // TODO(yuval): use panicable version of `get_gas` once inlining is supported.
    Ok(RewriteNode::interpolate_patched(
        format!(
            "fn $function_name$(mut data: Array::<felt>) -> Array::<felt> {{
            internal::revoke_ap_tracking();
            match get_gas() {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, {oog_err});
                    panic(err_data)
                }},
            }}
            {arg_definitions}
            if !array::ArrayTrait::is_empty(ref data) {{
                // Force the inclusion of `System` in the list of implicits.
                starknet::use_system_implicit();

                let mut err_data = array_new();
                array_append(ref err_data, {input_data_long_err});
                panic(err_data);
            }}
            match get_gas_all(get_builtin_costs()) {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array_new();
                    array_append(ref err_data, {oog_err});
                    panic(err_data)
                }},
            }}
            {let_res}$wrapped_name$({arg_names_str});
            let mut arr = array_new();
            // References.$ref_appends$
            // Result.{append_res}
            arr
        }}"
        )
        .as_str(),
        HashMap::from([
            ("function_name".to_string(), function_name),
            ("wrapped_name".to_string(), wrapped_name),
            (
                "ref_appends".to_string(),
                RewriteNode::Modified(ModifiedNode { children: ref_appends }),
            ),
            ("nothing".to_string(), RewriteNode::Text("".to_string())),
        ]),
    ))
}
