use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::node::ast::{self, Attribute, Modifier, OptionArgListParenthesized};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::{Terminal, TypedSyntaxNode};

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

/// Helper trait for syntax queries on `ast::Expr`.
pub trait AstPathExtract {
    /// Returns true if `self` matches `identifier`.
    /// Does not resolve paths or type aliases.
    fn is_identifier(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool;
    /// Returns true if `type_path` matches `$name$<$generic_arg$>`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_name_with_arg(&self, db: &dyn SyntaxGroup, name: &str, generic_arg: &str) -> bool;
    /// Returns true if `self` is `felt252`.
    /// Does not resolve paths or type aliases.
    fn is_felt252(&self, db: &dyn SyntaxGroup) -> bool {
        self.is_identifier(db, "felt252")
    }
    /// Returns true if `type_ast` matches `Span<felt252>`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_felt252_span(&self, db: &dyn SyntaxGroup) -> bool {
        self.is_name_with_arg(db, "Span", "felt252")
    }
}
impl AstPathExtract for ast::ExprPath {
    fn is_identifier(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool {
        let type_path_elements = self.elements(db);
        let [ast::PathSegment::Simple(arg_segment)] = type_path_elements.as_slice() else {
            return false;
        };

        arg_segment.ident(db).text(db) == identifier
    }

    fn is_name_with_arg(&self, db: &dyn SyntaxGroup, name: &str, generic_arg: &str) -> bool {
        let type_path_elements = self.elements(db);
        let [ast::PathSegment::WithGenericArgs(path_segment_with_generics)] =
            type_path_elements.as_slice()
        else {
            return false;
        };

        if path_segment_with_generics.ident(db).text(db) != name {
            return false;
        }
        let args = path_segment_with_generics.generic_args(db).generic_args(db).elements(db);
        let [ast::GenericArg::Unnamed(arg_expr)] = args.as_slice() else {
            return false;
        };
        let ast::GenericArgValue::Expr(arg_expr) = arg_expr.value(db) else {
            return false;
        };

        arg_expr.expr(db).is_identifier(db, generic_arg)
    }
}
impl AstPathExtract for ast::Expr {
    fn is_identifier(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool {
        if let ast::Expr::Path(type_path) = self {
            type_path.is_identifier(db, identifier)
        } else {
            false
        }
    }

    fn is_name_with_arg(&self, db: &dyn SyntaxGroup, name: &str, generic_arg: &str) -> bool {
        if let ast::Expr::Path(type_path) = self {
            type_path.is_name_with_arg(db, name, generic_arg)
        } else {
            false
        }
    }
}

/// Helper trait for syntax queries on `ast::GenericParam`.
pub trait GenericParamExtract {
    /// Returns the trait_path of the generic param if
    fn trait_path(&self, db: &dyn SyntaxGroup) -> Option<ast::ExprPath>;
    /// Returns true if `self` matches an impl of `$trait_name$<$generic_arg$>`.
    /// Does not resolve paths or type aliases.
    fn is_trait_impl(&self, db: &dyn SyntaxGroup, trait_name: &str, generic_arg: &str) -> bool {
        if let Some(path) = self.trait_path(db) {
            path.is_name_with_arg(db, trait_name, generic_arg)
        } else {
            false
        }
    }
}
impl GenericParamExtract for ast::GenericParam {
    fn trait_path(&self, db: &dyn SyntaxGroup) -> Option<ast::ExprPath> {
        match self {
            ast::GenericParam::Type(_) | ast::GenericParam::Const(_) => None,
            ast::GenericParam::ImplNamed(i) => Some(i.trait_path(db)),
            ast::GenericParam::ImplAnonymous(i) => Some(i.trait_path(db)),
        }
    }
}

/// Strips one preceding underscore from the given string slice, if any.
pub fn maybe_strip_underscore(s: &str) -> &str {
    match s.strip_prefix('_') {
        Some(stripped) => stripped,
        None => s,
    }
}

// === Attributes utilities ===

/// Checks if the given (possibly-attributed-)object is attributed with the given `attr_name`. Also
/// validates that the attribute is v0.
pub fn has_v0_attribute(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    object: &impl QueryAttrs,
    attr_name: &str,
) -> bool {
    let Some(attr) = object.find_attr(db, attr_name) else {
        return false;
    };
    validate_v0(db, diagnostics, &attr, attr_name);
    true
}

/// Assuming the attribute is `name`, validates it's #[`name`(v0)].
fn validate_v0(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    attr: &Attribute,
    name: &str,
) {
    if !is_arg_v0(db, attr) {
        diagnostics.push(PluginDiagnostic {
            message: format!("Only #[{name}(v0)] is supported."),
            stable_ptr: attr.stable_ptr().untyped(),
        });
    }
}

/// Checks if the only arg of the given attribute is "v0".
fn is_arg_v0(db: &dyn SyntaxGroup, attr: &Attribute) -> bool {
    match attr.arguments(db) {
        OptionArgListParenthesized::ArgListParenthesized(y) => {
            matches!(&y.args(db).elements(db)[..],
            [arg] if arg.as_syntax_node().get_text_without_trivia(db) == "v0")
        }
        OptionArgListParenthesized::Empty(_) => false,
    }
}
