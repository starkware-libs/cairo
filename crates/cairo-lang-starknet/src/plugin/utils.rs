use cairo_lang_syntax::node::ast::{self, Modifier};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::Terminal;

/// Checks if the parameter is defined as a ref parameter.
pub fn is_ref_param(db: &dyn SyntaxGroup, param: &ast::Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    matches!(param_modifiers[..], [Modifier::Ref(_)])
}

/// Checks if the parameter is defined as a mut parameter.
pub fn is_mut_param(db: &dyn SyntaxGroup, param: &ast::Param) -> bool {
    let param_modifiers = param.modifiers(db).elements(db);
    // TODO(yuval): This works only if "mut" is the only modifier. If the expansion was at the
    // semantic level, we could just ask if it's a reference.
    matches!(param_modifiers[..], [Modifier::Mut(_)])
}

/// Returns true if type_ast is `felt252`.
/// Does not resolve paths or type aliases.
pub fn is_felt252(db: &dyn SyntaxGroup, type_ast: &ast::Expr) -> bool {
    let ast::Expr::Path(type_path) = type_ast else {
        return false;
    };

    let type_path_elements = type_path.elements(db);
    let [ast::PathSegment::Simple(arg_segment)] = type_path_elements.as_slice() else {
        return false;
    };

    arg_segment.ident(db).text(db) == "felt252"
}

/// Returns true if type_ast is `Span::<felt252>`.
/// Does not resolve paths or type aliases.
pub fn is_felt252_span(db: &dyn SyntaxGroup, type_ast: &ast::Expr) -> bool {
    let ast::Expr::Path(type_path) = type_ast else {
        return false;
    };

    let type_path_elements = type_path.elements(db);
    let [ast::PathSegment::WithGenericArgs(path_segment_with_generics)
        ] = type_path_elements.as_slice() else {
        return false;
    };

    if path_segment_with_generics.ident(db).text(db) != "Span" {
        return false;
    }
    let args = path_segment_with_generics.generic_args(db).generic_args(db).elements(db);
    let [ast::GenericArg::Expr(arg_expr)] = args.as_slice() else {
        return false;
    };

    is_felt252(db, &arg_expr.value(db))
}

/// Strips one preceding underscore from the given string slice, if any.
pub fn maybe_strip_underscore(s: &str) -> &str {
    match s.strip_prefix('_') {
        Some(stripped) => stripped,
        None => s,
    }
}
