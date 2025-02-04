use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::ast::{self, Attribute, Modifier, OptionTypeClause};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::{QueryAttrs, is_single_arg_attr};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::{extract_matches, require, try_extract_matches};

use super::consts::{CONSTRUCTOR_ATTR, EXTERNAL_ATTR, L1_HANDLER_ATTR};

/// Helper trait for syntax queries on `ast::Param`.
pub trait ParamEx {
    /// Checks if the parameter is defined as a ref parameter.
    fn is_ref_param(&self, db: &dyn SyntaxGroup) -> bool;
    /// Checks if the parameter is defined as a mutable parameter.
    fn is_mut_param(&self, db: &dyn SyntaxGroup) -> bool;
    /// Extracts the snapshot type if the parameter's type is a snapshot. Otherwise, returns None.
    fn try_extract_snapshot(&self, db: &dyn SyntaxGroup) -> Option<ast::Expr>;
}
impl ParamEx for ast::Param {
    fn is_ref_param(&self, db: &dyn SyntaxGroup) -> bool {
        let param_modifiers = self.modifiers(db).elements(db);
        // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
        // semantic level, we could just ask if it's a reference.
        matches!(param_modifiers[..], [Modifier::Ref(_)])
    }

    fn is_mut_param(&self, db: &dyn SyntaxGroup) -> bool {
        let param_modifiers = self.modifiers(db).elements(db);
        // TODO(yuval): This works only if "mut" is the only modifier. If the expansion was at the
        // semantic level, we could just ask if it's a reference.
        matches!(param_modifiers[..], [Modifier::Mut(_)])
    }

    fn try_extract_snapshot(&self, db: &dyn SyntaxGroup) -> Option<ast::Expr> {
        let unary = try_extract_matches!(
            extract_matches!(self.type_clause(db), OptionTypeClause::TypeClause).ty(db),
            ast::Expr::Unary
        )?;
        require(matches!(unary.op(db), ast::UnaryOperator::At(_)))?;
        Some(unary.expr(db))
    }
}

/// Helper trait for syntax queries on `ast::Expr`.
pub trait AstPathExtract {
    /// Returns true if `self` matches `identifier`.
    /// Does not resolve paths or type aliases.
    fn is_identifier(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool;
    /// Returns true if `self` matches `$name$<$generic_arg$>`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_name_with_arg(&self, db: &dyn SyntaxGroup, name: &str, generic_arg: &str) -> bool;
    /// Returns true if `self` is dependent on `identifier` in an internal type.
    /// For example given identifier `T` will return true for:
    /// `T`, `Array<T>`, `Array<Array<T>>`, `(T, felt252)`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool;
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

    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool {
        let segments = self.elements(db);
        let Some((last, head)) = segments.split_last() else { return false };
        match last {
            ast::PathSegment::Simple(arg_segment) => {
                head.is_empty() && arg_segment.ident(db).text(db) == identifier
            }
            ast::PathSegment::WithGenericArgs(with_generics) => {
                with_generics.generic_args(db).generic_args(db).elements(db).iter().any(|arg| {
                    let generic_arg_value = match arg {
                        ast::GenericArg::Named(named) => named.value(db),
                        ast::GenericArg::Unnamed(unnamed) => unnamed.value(db),
                    };
                    match generic_arg_value {
                        ast::GenericArgValue::Expr(arg_expr) => {
                            arg_expr.expr(db).is_dependent_type(db, identifier)
                        }
                        ast::GenericArgValue::Underscore(_) => false,
                    }
                })
            }
        }
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

    fn is_dependent_type(&self, db: &dyn SyntaxGroup, identifier: &str) -> bool {
        match self {
            ast::Expr::Path(type_path) => type_path.is_dependent_type(db, identifier),
            ast::Expr::Unary(unary) => unary.expr(db).is_dependent_type(db, identifier),
            ast::Expr::Binary(binary) => {
                binary.lhs(db).is_dependent_type(db, identifier)
                    || binary.rhs(db).is_dependent_type(db, identifier)
            }
            ast::Expr::Tuple(tuple) => tuple
                .expressions(db)
                .elements(db)
                .iter()
                .any(|expr| expr.is_dependent_type(db, identifier)),
            _ => false,
        }
    }
}

/// Helper trait for syntax queries on `ast::GenericParam`.
pub trait GenericParamExtract {
    /// Returns the trait_path of the generic param if it is an impl.
    fn trait_path(&self, db: &dyn SyntaxGroup) -> Option<ast::ExprPath>;
    /// Returns true if `self` matches an impl of `$trait_name$<$generic_arg$>`.
    /// Does not resolve paths or type aliases.
    fn is_impl_of(&self, db: &dyn SyntaxGroup, trait_name: &str, generic_arg: &str) -> bool {
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
            ast::GenericParam::NegativeImpl(i) => Some(i.trait_path(db)),
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
    has_v0_attribute_ex(db, diagnostics, object, attr_name, || None)
}

/// Checks if the given (possibly-attributed-)object is attributed with the given `attr_name`. Also
/// validates that the attribute is v0, and adds a warning if supplied `deprecated` returns a value.
pub fn has_v0_attribute_ex(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    object: &impl QueryAttrs,
    attr_name: &str,
    deprecated: impl FnOnce() -> Option<String>,
) -> bool {
    let Some(attr) = object.find_attr(db, attr_name) else {
        return false;
    };
    validate_v0(db, diagnostics, &attr, attr_name);
    if let Some(deprecated) = deprecated() {
        diagnostics.push(PluginDiagnostic::warning(attr.stable_ptr().untyped(), deprecated));
    }
    true
}

/// Assuming the attribute is `name`, validates it's #[`name`(v0)].
pub fn validate_v0(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    attr: &Attribute,
    name: &str,
) {
    if !is_single_arg_attr(db, attr, "v0") {
        diagnostics.push(PluginDiagnostic::error(
            attr.stable_ptr().untyped(),
            format!("Only #[{name}(v0)] is supported."),
        ));
    }
}

/// Forbids `#[external]`, `#[l1_handler]` and `#[constructor]` attributes in the given impl.
pub fn forbid_attributes_in_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_item: &ast::ImplItem,
    embedded_impl_attr: &str,
) {
    for attr in [EXTERNAL_ATTR, CONSTRUCTOR_ATTR, L1_HANDLER_ATTR] {
        forbid_attribute_in_impl(db, diagnostics, impl_item, attr, embedded_impl_attr);
    }
}

/// Forbids the given attribute in the given impl, assuming it's marked `embedded_impl_attr`.
pub fn forbid_attribute_in_impl(
    db: &dyn SyntaxGroup,
    diagnostics: &mut Vec<PluginDiagnostic>,
    impl_item: &ast::ImplItem,
    attr_name: &str,
    embedded_impl_attr: &str,
) {
    if let Some(attr) = impl_item.find_attr(db, attr_name) {
        diagnostics.push(PluginDiagnostic::error(
            attr.stable_ptr().untyped(),
            format!(
                "The `{attr_name}` attribute is not allowed inside an impl marked as \
                 `{embedded_impl_attr}`."
            ),
        ));
    }
}

/// Returns true if the type has a derive attribute with the given type.
pub fn has_derive<T: QueryAttrs>(
    with_attrs: &T,
    db: &dyn SyntaxGroup,
    derived_type: &str,
) -> Option<ast::Arg> {
    with_attrs.query_attr(db, "derive").into_iter().find_map(|attr| {
        let attr = attr.structurize(db);
        for arg in attr.args {
            let AttributeArg {
                variant: AttributeArgVariant::Unnamed(ast::Expr::Path(path)),
                arg,
                ..
            } = arg
            else {
                continue;
            };
            if path.as_syntax_node().get_text_without_trivia(db) == derived_type {
                return Some(arg);
            }
        }
        None
    })
}
