use cairo_lang_defs::plugin::PluginDiagnostic;
use cairo_lang_syntax::attribute::structured::{
    AttributeArg, AttributeArgVariant, AttributeStructurize,
};
use cairo_lang_syntax::node::TypedSyntaxNode;
use cairo_lang_syntax::node::ast::{self, Attribute, Modifier, OptionTypeClause};
use cairo_lang_syntax::node::helpers::{GetIdentifier, QueryAttrs, is_single_arg_attr};
use cairo_lang_utils::{extract_matches, require, try_extract_matches};
use itertools::Itertools;
use salsa::Database;

use super::consts::{CONSTRUCTOR_ATTR, EXTERNAL_ATTR, L1_HANDLER_ATTR};

/// Helper trait for syntax queries on `ast::Param`.
pub trait ParamEx<'db> {
    /// Checks if the parameter is defined as a ref parameter.
    fn is_ref_param(&self, db: &'db dyn Database) -> bool;
    /// Checks if the parameter is defined as a mutable parameter.
    fn is_mut_param(&self, db: &'db dyn Database) -> bool;
    /// Extracts the snapshot type if the parameter's type is a snapshot. Otherwise, returns None.
    fn try_extract_snapshot(&self, db: &'db dyn Database) -> Option<ast::Expr<'db>>;
}
impl<'db> ParamEx<'db> for ast::Param<'db> {
    fn is_ref_param(&self, db: &dyn Database) -> bool {
        let param_modifiers = self.modifiers(db).elements(db).collect_array();
        // TODO(yuval): This works only if "ref" is the only modifier. If the expansion was at the
        // semantic level, we could just ask if it's a reference.
        matches!(param_modifiers, Some([Modifier::Ref(_)]))
    }

    fn is_mut_param(&self, db: &dyn Database) -> bool {
        let param_modifiers = self.modifiers(db).elements(db).collect_array();
        // TODO(yuval): This works only if "mut" is the only modifier. If the expansion was at the
        // semantic level, we could just ask if it's a reference.
        matches!(param_modifiers, Some([Modifier::Mut(_)]))
    }

    fn try_extract_snapshot(&self, db: &'db dyn Database) -> Option<ast::Expr<'db>> {
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
    fn is_identifier(&self, db: &dyn Database, identifier: &str) -> bool;
    /// Returns true if `self` matches `$name$<$generic_arg$>`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_name_with_arg(&self, db: &dyn Database, name: &str, generic_arg: &str) -> bool;
    /// Returns true if `self` is `felt252`.
    /// Does not resolve paths or type aliases.
    fn is_felt252(&self, db: &dyn Database) -> bool {
        self.is_identifier(db, "felt252")
    }
    /// Returns true if `type_ast` matches `Span<felt252>`.
    /// Does not resolve paths, type aliases or named generics.
    fn is_felt252_span(&self, db: &dyn Database) -> bool {
        self.is_name_with_arg(db, "Span", "felt252")
    }
}
impl<'db> AstPathExtract for ast::ExprPath<'db> {
    fn is_identifier(&self, db: &dyn Database, identifier: &str) -> bool {
        let segments = self.segments(db);
        let type_path_elements = segments.elements(db);
        let Some([ast::PathSegment::Simple(arg_segment)]) = type_path_elements.collect_array()
        else {
            return false;
        };

        arg_segment.identifier(db).long(db) == identifier
    }

    fn is_name_with_arg(&self, db: &dyn Database, name: &str, generic_arg: &str) -> bool {
        let segments = self.segments(db);
        let type_path_elements = segments.elements(db);
        let Some([ast::PathSegment::WithGenericArgs(path_segment_with_generics)]) =
            type_path_elements.collect_array()
        else {
            return false;
        };

        if path_segment_with_generics.identifier(db).long(db) != name {
            return false;
        }
        let generic_args = path_segment_with_generics.generic_args(db).generic_args(db);
        let args = generic_args.elements(db);
        let Some([ast::GenericArg::Unnamed(arg_expr)]) = args.collect_array() else {
            return false;
        };
        let ast::GenericArgValue::Expr(arg_expr) = arg_expr.value(db) else {
            return false;
        };

        arg_expr.expr(db).is_identifier(db, generic_arg)
    }
}
impl<'db> AstPathExtract for ast::Expr<'db> {
    fn is_identifier(&self, db: &dyn Database, identifier: &str) -> bool {
        if let ast::Expr::Path(type_path) = self {
            type_path.is_identifier(db, identifier)
        } else {
            false
        }
    }

    fn is_name_with_arg(&self, db: &dyn Database, name: &str, generic_arg: &str) -> bool {
        if let ast::Expr::Path(type_path) = self {
            type_path.is_name_with_arg(db, name, generic_arg)
        } else {
            false
        }
    }
}

/// Helper trait for syntax queries on `ast::GenericParam`.
pub trait GenericParamExtract<'db> {
    /// Returns the trait_path of the generic param if it is an impl.
    fn trait_path(&self, db: &'db dyn Database) -> Option<ast::ExprPath<'db>>;
    /// Returns true if `self` matches an impl of `$trait_name$<$generic_arg$>`.
    /// Does not resolve paths or type aliases.
    fn is_impl_of(&self, db: &'db dyn Database, trait_name: &str, generic_arg: &str) -> bool {
        if let Some(path) = self.trait_path(db) {
            path.is_name_with_arg(db, trait_name, generic_arg)
        } else {
            false
        }
    }
}
impl<'db> GenericParamExtract<'db> for ast::GenericParam<'db> {
    fn trait_path(&self, db: &'db dyn Database) -> Option<ast::ExprPath<'db>> {
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
pub fn has_v0_attribute<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    object: &impl QueryAttrs<'db>,
    attr_name: &'db str,
) -> bool {
    has_v0_attribute_ex(db, diagnostics, object, attr_name, || None)
}

/// Checks if the given (possibly-attributed-)object is attributed with the given `attr_name`. Also
/// validates that the attribute is v0, and adds a warning if supplied `deprecated` returns a value.
pub fn has_v0_attribute_ex<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    object: &impl QueryAttrs<'db>,
    attr_name: &'db str,
    deprecated: impl FnOnce() -> Option<String>,
) -> bool {
    let Some(attr) = object.find_attr(db, attr_name) else {
        return false;
    };
    validate_v0(db, diagnostics, &attr, attr_name);
    if let Some(deprecated) = deprecated() {
        diagnostics.push(PluginDiagnostic::warning(attr.stable_ptr(db), deprecated));
    }
    true
}

/// Assuming the attribute is `name`, validates it's in the form "#[name(v0)]".
pub fn validate_v0<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    attr: &Attribute<'db>,
    name: &str,
) {
    if !is_single_arg_attr(db, attr, "v0") {
        diagnostics.push(PluginDiagnostic::error(
            attr.stable_ptr(db),
            format!("Only #[{name}(v0)] is supported."),
        ));
    }
}

/// Forbids `#[external]`, `#[l1_handler]` and `#[constructor]` attributes in the given impl.
pub fn forbid_attributes_in_impl<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    impl_item: &ast::ImplItem<'db>,
    embedded_impl_attr: &str,
) {
    for attr in [EXTERNAL_ATTR, CONSTRUCTOR_ATTR, L1_HANDLER_ATTR] {
        forbid_attribute_in_impl(db, diagnostics, impl_item, attr, embedded_impl_attr);
    }
}

/// Forbids the given attribute in the given impl, assuming it's marked `embedded_impl_attr`.
pub fn forbid_attribute_in_impl<'db>(
    db: &'db dyn Database,
    diagnostics: &mut Vec<PluginDiagnostic<'db>>,
    impl_item: &ast::ImplItem<'db>,
    attr_name: &'db str,
    embedded_impl_attr: &str,
) {
    if let Some(attr) = impl_item.find_attr(db, attr_name) {
        diagnostics.push(PluginDiagnostic::error(
            attr.stable_ptr(db),
            format!(
                "The `{attr_name}` attribute is not allowed inside an impl marked as \
                 `{embedded_impl_attr}`."
            ),
        ));
    }
}

/// Returns true if the type has a derive attribute with the given type.
pub fn has_derive<'db, T: QueryAttrs<'db>>(
    with_attrs: &T,
    db: &'db dyn Database,
    derived_type: &str,
) -> Option<ast::Arg<'db>> {
    with_attrs.query_attr(db, "derive").find_map(|attr| {
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
            if path.as_syntax_node().get_text_without_trivia(db).long(db) == derived_type {
                return Some(arg);
            }
        }
        None
    })
}
