use std::collections::HashMap;

use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_semantic::patcher::RewriteNode;
use cairo_lang_syntax::node::ast::{FunctionWithBody, OptionReturnTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};

use super::consts::{CONSTRUCTOR_ATTR, EXTERNAL_ATTR, L1_HANDLER_ATTR, RAW_OUTPUT_ATTR, VIEW_ATTR};
use super::utils::{is_felt252_span, is_ref_param};

/// Kind of an entry point. Determined by the entry point's attributes.
pub enum EntryPointKind {
    External,
    View,
    Constructor,
    L1Handler,
}
impl EntryPointKind {
    /// Returns the entry point kind if the given function is indeed an entry point.
    pub fn try_from_function_with_body(
        db: &dyn SyntaxGroup,
        item_function: &FunctionWithBody,
    ) -> Option<Self> {
        if item_function.has_attr(db, EXTERNAL_ATTR) {
            Some(EntryPointKind::External)
        } else if item_function.has_attr(db, VIEW_ATTR) {
            Some(EntryPointKind::View)
        } else if item_function.has_attr(db, CONSTRUCTOR_ATTR) {
            Some(EntryPointKind::Constructor)
        } else if item_function.has_attr(db, L1_HANDLER_ATTR) {
            Some(EntryPointKind::L1Handler)
        } else {
            None
        }
    }

    /// Returns the relevant attribute of the entry point kind.
    pub fn get_attr(&self) -> &str {
        match self {
            EntryPointKind::External => EXTERNAL_ATTR,
            EntryPointKind::View => VIEW_ATTR,
            EntryPointKind::Constructor => CONSTRUCTOR_ATTR,
            EntryPointKind::L1Handler => L1_HANDLER_ATTR,
        }
    }
}

/// Generates Cairo code for an entry point wrapper.
pub fn generate_entry_point_wrapper(
    db: &dyn SyntaxGroup,
    function: &FunctionWithBody,
) -> Result<RewriteNode, Vec<PluginDiagnostic>> {
    let declaration = function.declaration(db);
    let sig = declaration.signature(db);
    let params = sig.parameters(db).elements(db);
    let mut diagnostics = vec![];
    let mut arg_names = Vec::new();
    let mut arg_definitions = Vec::new();
    let mut ref_appends = Vec::new();

    let raw_output = function.has_attr(db, RAW_OUTPUT_ATTR);

    let input_data_short_err = "'Input too short for arguments'";
    for param in params {
        let arg_name = format!("__arg_{}", param.name(db).text(db));
        let arg_type_ast = param.type_clause(db).ty(db);
        let type_name = arg_type_ast.as_syntax_node().get_text_without_trivia(db);

        let is_ref = is_ref_param(db, &param);
        if raw_output && is_ref {
            diagnostics.push(PluginDiagnostic {
                message: format!("`{RAW_OUTPUT_ATTR}` functions cannot have `ref` parameters."),
                stable_ptr: param.modifiers(db).stable_ptr().untyped(),
            });
        }

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
                        let mut err_data = array::array_new();
                        array::array_append(ref err_data, {input_data_short_err});
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

    let function_name = RewriteNode::new_trimmed(declaration.name(db).as_syntax_node());
    let wrapped_name = RewriteNode::interpolate_patched(
        "super::$function_name$",
        HashMap::from([("function_name".to_string(), function_name.clone())]),
    );

    let ret_ty = sig.ret_ty(db);
    let (let_res, append_res, return_ty_is_felt252_span, ret_type_ptr) = match &ret_ty {
        OptionReturnTypeClause::Empty(type_clause_ast) => {
            ("", "".to_string(), false, type_clause_ast.stable_ptr().untyped())
        }
        OptionReturnTypeClause::ReturnTypeClause(ty) => {
            let ret_type_ast = ty.ty(db);

            let return_ty_is_felt252_span = is_felt252_span(db, &ret_type_ast);
            let ret_type_name = ret_type_ast.as_syntax_node().get_text_without_trivia(db);
            (
                "\n            let res = ",
                format!("\n            serde::Serde::<{ret_type_name}>::serialize(ref arr, res);"),
                return_ty_is_felt252_span,
                ret_type_ast.stable_ptr().untyped(),
            )
        }
    };

    if raw_output && !return_ty_is_felt252_span {
        diagnostics.push(PluginDiagnostic {
            message: format!("`{RAW_OUTPUT_ATTR}` functions must return `Span::<felt252>`."),
            stable_ptr: ret_type_ptr,
        });
    }

    if !diagnostics.is_empty() {
        return Err(diagnostics);
    }

    let oog_err = "'Out of gas'";
    let input_data_long_err = "'Input too long for arguments'";

    let output_handling_string = if raw_output {
        format!("$wrapped_name$({arg_names_str})")
    } else {
        format!(
            "{let_res}$wrapped_name$({arg_names_str});
            let mut arr = array::array_new();
            // References.$ref_appends$
            // Result.{append_res}
            array::ArrayTrait::span(@arr)"
        )
    };

    let output_handling = RewriteNode::interpolate_patched(
        &output_handling_string,
        HashMap::from([
            ("wrapped_name".to_string(), wrapped_name),
            ("ref_appends".to_string(), RewriteNode::new_modified(ref_appends)),
        ]),
    );

    let arg_definitions = arg_definitions.join("\n");
    // TODO(yuval): use panicable version of `withdraw_gas` once inlining is supported.
    Ok(RewriteNode::interpolate_patched(
        format!(
            "fn $function_name$(mut data: Span::<felt252>) -> Span::<felt252> {{
            internal::revoke_ap_tracking();
            match gas::withdraw_gas() {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array::array_new();
                    array::array_append(ref err_data, {oog_err});
                    panic(err_data)
                }},
            }}
            {arg_definitions}
            if !array::SpanTrait::is_empty(data) {{
                // Force the inclusion of `System` in the list of implicits.
                starknet::use_system_implicit();

                let mut err_data = array::array_new();
                array::array_append(ref err_data, {input_data_long_err});
                panic(err_data);
            }}
            match gas::withdraw_gas_all(get_builtin_costs()) {{
                Option::Some(_) => {{
                }},
                Option::None(_) => {{
                    let mut err_data = array::array_new();
                    array::array_append(ref err_data, {oog_err});
                    panic(err_data)
                }},
            }}
            $output_handling$
        }}"
        )
        .as_str(),
        HashMap::from([
            ("function_name".to_string(), function_name),
            ("output_handling".to_string(), output_handling),
        ]),
    ))
}
