use std::collections::HashMap;

use cairo_lang_debug::DebugWithDb;
use cairo_lang_defs::db::get_all_path_leaves;
use cairo_lang_defs::diagnostic_utils::StableLocation;
use cairo_lang_defs::ids::{
    ConstantLongId, EnumLongId, ExternFunctionLongId, ExternTypeLongId, FreeFunctionLongId,
    ImplAliasLongId, ImplConstantDefLongId, ImplDefLongId, ImplFunctionLongId, ImplItemId,
    LanguageElementId, LookupItemId, MacroDeclarationLongId, ModuleId, ModuleItemId,
    ModuleTypeAliasLongId, StructLongId, TraitConstantLongId, TraitFunctionLongId, TraitImplLongId,
    TraitItemId, TraitLongId, TraitTypeLongId, UseLongId,
};
use cairo_lang_filesystem::db::FilesGroup;
use cairo_lang_lowering::Location;
use cairo_lang_lowering::ids::LocationId;
use cairo_lang_semantic::Expr;
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::pattern::QueryPatternVariablesFromDb;
use cairo_lang_semantic::items::function_with_body::{
    FunctionWithBodySemantic, SemanticExprLookup,
};
use cairo_lang_semantic::lookup_item::LookupItemEx;
use cairo_lang_semantic::lsp_helpers::LspHelpers;
use cairo_lang_semantic::resolve::ResolvedGenericItem;
use cairo_lang_sierra::ids::{FunctionId, VarId};
use cairo_lang_syntax::node::ast::{
    BinaryOperator, ExprBinary, ParamList, StatementLet, TerminalIdentifier,
};
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Terminal, TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use crate::debug_info::function_debug_info::serializable::{
    CairoVariableName, SerializableAllFunctionsDebugInfo, SerializableFunctionDebugInfo,
};
use crate::debug_info::{SourceCodeSpan, SourceFileFullPath, maybe_code_location};

pub mod serializable;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AllFunctionsDebugInfo<'db>(OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>);

impl<'db> AllFunctionsDebugInfo<'db> {
    pub fn new(value: OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>) -> Self {
        Self(value)
    }

    pub fn extract_serializable_debug_info(
        &self,
        db: &'db dyn Database,
    ) -> SerializableAllFunctionsDebugInfo {
        SerializableAllFunctionsDebugInfo(
            self.0
                .iter()
                .filter_map(|(function_id, function_debug_info)| {
                    Some((
                        function_id.clone(),
                        function_debug_info.extract_serializable_debug_info(db)?,
                    ))
                })
                .collect(),
        )
    }
}

/// The debug info of a sierra function.
/// Contains a signature location and locations of sierra variables of this function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDebugInfo<'db> {
    pub signature_location: LocationId<'db>,
    pub variables_locations: OrderedHashMap<VarId, LocationId<'db>>,
}

impl<'db> FunctionDebugInfo<'db> {
    fn extract_serializable_debug_info(
        &self,
        db: &'db dyn Database,
    ) -> Option<SerializableFunctionDebugInfo> {
        let (function_file_path, function_code_span) = self.extract_location(db)?;
        let sierra_to_cairo_variable =
            self.extract_variables_mapping(db, &function_file_path, &function_code_span);

        Some(SerializableFunctionDebugInfo {
            function_file_path,
            function_code_span,
            sierra_to_cairo_variable,
        })
    }

    /// Extracts mapping from a sierra variable to a cairo variable (its name and definition span).
    /// The sierra variable value corresponds to the cairo variable value at some point during
    /// execution of the function code.
    fn extract_variables_mapping(
        &self,
        db: &'db dyn Database,
        function_file_path: &SourceFileFullPath,
        function_code_span: &SourceCodeSpan,
    ) -> HashMap<VarId, (CairoVariableName, SourceCodeSpan)> {
        self.variables_locations
            .iter()
            .filter_map(|(sierra_var, code_location)| {
                // Ignore inline locations for now as this function is supposed to be called only
                // on when sierra was compiled with inlining set to `avoid`.
                // TODO: handle them to make sure user function marked #[inline(always)] work.
                let Location { stable_location: sierra_var_location, notes: _, inline_locations } =
                    code_location.long(db);

                eprintln!(
                    "STABLE_LOC: {:?}",
                    sierra_var_location.span_in_file(db).user_location(db).debug(db)
                );

                let identifier = find_identifier_corresponding_to_assigned_variable(
                    db,
                    sierra_var_location.syntax_node(db),
                )?;

                let module_id = db.find_module_containing_node(identifier.as_syntax_node())?;

                let lookup_items: Vec<_> = identifier
                    .as_syntax_node()
                    .ancestors_with_self(db)
                    .flat_map(|node| lookup_item_from_ast(db, module_id, node).unwrap_or_default())
                    .collect();

                let definition =
                    try_variable_declaration(db, &identifier, &lookup_items).or_else(|| {
                        lookup_variable_in_resolved_items(db, &identifier, &lookup_items)
                    })?;

                let location = match definition {
                    // Extract only param name.
                    cairo_lang_defs::ids::VarId::Param(param) => StableLocation::new(
                        param.stable_ptr(db).lookup(db).name(db).stable_ptr(db).untyped(),
                    ),
                    x => x.stable_location(db),
                };

                let (path, span, _) = maybe_code_location(db, location)?;

                // Sanity check.
                // TODO: check if it occurs at all except for inlined function calls.
                if !(function_file_path == &path && function_code_span.contains(&span)) {
                    eprintln!("heee heeeeeeeeeee inline empty: {}", inline_locations.is_empty());
                    return None;
                }

                let user_location = location.span_in_file(db).user_location(db);
                let var_name = user_location
                    .span
                    .take(db.file_content(user_location.file_id).unwrap())
                    .to_string();

                eprintln!("RESOLVED_LOC: {:?}", user_location.debug(db));
                eprintln!("VAR_RESOLVED: {:?}", var_name);

                Some((sierra_var.clone(), (var_name, span)))
            })
            .collect()
    }

    /// Extracts the location of the function - path to the user file it comes from and its span.
    fn extract_location(
        &self,
        db: &'db dyn Database,
    ) -> Option<(SourceFileFullPath, SourceCodeSpan)> {
        let function_ptr = self
            .signature_location
            .long(db)
            .stable_location
            .syntax_node(db)
            .ancestor_of_kinds(db, &[SyntaxKind::FunctionWithBody, SyntaxKind::TraitItemFunction])?
            .stable_ptr(db);
        let (function_file_path, function_code_span, _) =
            maybe_code_location(db, StableLocation::new(function_ptr))?;

        Some((function_file_path, function_code_span))
    }
}

/// This function gets a node that a sierra variable was mapped to.
/// It tries to make an educated guess to find an identifier corresponding to a cairo variable
/// which value in the given location is represented by the sierra variable.
///
/// The algorithm is to find an identifier corresponding to a variable which either:
/// - is assigned a value in the statement the `node` is a part of,
/// - is part of a function's param, and `node` is a part of the function's params list.
///
/// If there is no such identifier - `None` is returned.
fn find_identifier_corresponding_to_assigned_variable<'db>(
    db: &'db dyn Database,
    node: SyntaxNode<'db>,
) -> Option<TerminalIdentifier<'db>> {
    let is_param = node.ancestor_of_type::<ParamList<'_>>(db).is_some();
    if is_param {
        return find_identifier_in_node(db, node);
    }

    let assignment_expr = node.ancestors_with_self(db).find_map(|node| {
        let binary = ExprBinary::cast(db, node)?;
        match binary.op(db) {
            BinaryOperator::MulEq(_)
            | BinaryOperator::DivEq(_)
            | BinaryOperator::ModEq(_)
            | BinaryOperator::PlusEq(_)
            | BinaryOperator::MinusEq(_)
            | BinaryOperator::Eq(_) => Some(binary),
            _ => None,
        }
    });

    if let Some(assignment_expr) = assignment_expr {
        let lhs = assignment_expr.lhs(db).as_syntax_node();

        // Case when sierra var maps to a part of lhs.
        if node.ancestors_with_self(db).any(|node| node == lhs) {
            return find_identifier_in_node(db, node);
        }

        // Sometimes a sierra var maps to the whole expression like `x += y`.
        if node == assignment_expr.as_syntax_node() {
            return find_identifier_in_node(db, lhs);
        }

        // If the original node covers whole rhs of the assignment and the operator is a simple eq,
        // we should try to search for the identifier on the lhs.
        if assignment_expr.op(db).as_syntax_node().kind(db) == SyntaxKind::TerminalEq
            && node == assignment_expr.rhs(db).as_syntax_node()
        {
            return find_identifier_in_node(db, lhs);
        }
    }

    if let Some(let_statement) = node.ancestors(db).find_map(|node| StatementLet::cast(db, node)) {
        let pattern = let_statement.pattern(db).as_syntax_node();

        let is_on_pattern = node.ancestors_with_self(db).any(|node| node == pattern);
        if is_on_pattern {
            return find_identifier_in_node(db, node);
        }

        // If the original node covers whole rhs of the let statement, we should try to search
        // for the identifier on the pattern.
        if node == let_statement.rhs(db).as_syntax_node() {
            return find_identifier_in_node(db, pattern);
        }
    }

    None
}

fn find_identifier_in_node<'db>(
    db: &'db dyn Database,
    node: SyntaxNode<'db>,
) -> Option<TerminalIdentifier<'db>> {
    TerminalIdentifier::cast_token(db, node).or_else(|| TerminalIdentifier::cast(db, node)).or_else(
        || {
            let mut terminal_descendants: Vec<_> = node
                .descendants(db)
                .filter_map(|node| TerminalIdentifier::cast_token(db, node))
                .collect();
            if terminal_descendants.len() == 1 { terminal_descendants.pop() } else { None }
        },
    )
}

/// If the ast node is a lookup item, return corresponding ids.
/// Otherwise, returns `None`.
///
/// Code from <https://github.com/software-mansion/cairols/blob/6ee588fec02b14071bbd70b405e88fb7215b1d68/src/lang/db/semantic.rs#L417>.
fn lookup_item_from_ast<'db>(
    db: &'db dyn Database,
    module_id: ModuleId<'db>,
    node: SyntaxNode<'db>,
) -> Option<Vec<LookupItemId<'db>>> {
    let syntax_db = db;

    let is_in_impl = node.ancestor_of_kind(syntax_db, SyntaxKind::ItemImpl).is_some();

    Some(match node.kind(syntax_db) {
        SyntaxKind::ItemConstant => {
            if is_in_impl {
                vec![LookupItemId::ImplItem(ImplItemId::Constant(
                    ImplConstantDefLongId(
                        module_id,
                        ast::ItemConstant::from_syntax_node(syntax_db, node).stable_ptr(syntax_db),
                    )
                    .intern(db),
                ))]
            } else {
                vec![LookupItemId::ModuleItem(ModuleItemId::Constant(
                    ConstantLongId(
                        module_id,
                        ast::ItemConstant::from_syntax_node(db, node).stable_ptr(db),
                    )
                    .intern(db),
                ))]
            }
        }
        SyntaxKind::FunctionWithBody => {
            if is_in_impl {
                vec![LookupItemId::ImplItem(ImplItemId::Function(
                    ImplFunctionLongId(
                        module_id,
                        ast::FunctionWithBody::from_syntax_node(db, node).stable_ptr(db),
                    )
                    .intern(db),
                ))]
            } else {
                vec![LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
                    FreeFunctionLongId(
                        module_id,
                        ast::FunctionWithBody::from_syntax_node(db, node).stable_ptr(db),
                    )
                    .intern(db),
                ))]
            }
        }
        SyntaxKind::ItemExternFunction => {
            vec![LookupItemId::ModuleItem(ModuleItemId::ExternFunction(
                ExternFunctionLongId(
                    module_id,
                    ast::ItemExternFunction::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemExternType => vec![LookupItemId::ModuleItem(ModuleItemId::ExternType(
            ExternTypeLongId(
                module_id,
                ast::ItemExternType::from_syntax_node(db, node).stable_ptr(db),
            )
            .intern(db),
        ))],
        SyntaxKind::ItemTrait => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Trait(
                TraitLongId(module_id, ast::ItemTrait::from_syntax_node(db, node).stable_ptr(db))
                    .intern(db),
            ))]
        }
        SyntaxKind::TraitItemConstant => {
            vec![LookupItemId::TraitItem(TraitItemId::Constant(
                TraitConstantLongId(
                    module_id,
                    ast::TraitItemConstant::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::TraitItemFunction => {
            vec![LookupItemId::TraitItem(TraitItemId::Function(
                TraitFunctionLongId(
                    module_id,
                    ast::TraitItemFunction::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::TraitItemImpl => {
            vec![LookupItemId::TraitItem(TraitItemId::Impl(
                TraitImplLongId(
                    module_id,
                    ast::TraitItemImpl::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::TraitItemType => {
            vec![LookupItemId::TraitItem(TraitItemId::Type(
                TraitTypeLongId(
                    module_id,
                    ast::TraitItemType::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        SyntaxKind::ItemImpl => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Impl(
                ImplDefLongId(module_id, ast::ItemImpl::from_syntax_node(db, node).stable_ptr(db))
                    .intern(db),
            ))]
        }
        SyntaxKind::ItemStruct => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Struct(
                StructLongId(module_id, ast::ItemStruct::from_syntax_node(db, node).stable_ptr(db))
                    .intern(db),
            ))]
        }
        SyntaxKind::ItemEnum => {
            vec![LookupItemId::ModuleItem(ModuleItemId::Enum(
                EnumLongId(module_id, ast::ItemEnum::from_syntax_node(db, node).stable_ptr(db))
                    .intern(db),
            ))]
        }
        SyntaxKind::ItemUse => {
            // Item use is not a lookup item, so we need to collect all UseLeaf, which are lookup
            // items.
            let item_use = ast::ItemUse::from_syntax_node(db, node);
            get_all_path_leaves(db, &item_use)
                .into_iter()
                .map(|leaf| {
                    let use_long_id = UseLongId(module_id, leaf.stable_ptr(syntax_db));
                    LookupItemId::ModuleItem(ModuleItemId::Use(use_long_id.intern(db)))
                })
                .collect()
        }
        SyntaxKind::ItemTypeAlias => vec![LookupItemId::ModuleItem(ModuleItemId::TypeAlias(
            ModuleTypeAliasLongId(
                module_id,
                ast::ItemTypeAlias::from_syntax_node(db, node).stable_ptr(db),
            )
            .intern(db),
        ))],
        SyntaxKind::ItemImplAlias => vec![LookupItemId::ModuleItem(ModuleItemId::ImplAlias(
            ImplAliasLongId(
                module_id,
                ast::ItemImplAlias::from_syntax_node(db, node).stable_ptr(db),
            )
            .intern(db),
        ))],
        SyntaxKind::ItemMacroDeclaration => {
            vec![LookupItemId::ModuleItem(ModuleItemId::MacroDeclaration(
                MacroDeclarationLongId(
                    module_id,
                    ast::ItemMacroDeclaration::from_syntax_node(db, node).stable_ptr(db),
                )
                .intern(db),
            ))]
        }
        _ => return None,
    })
}

/// Lookups if the identifier is a declaration of a variable/param in one of the lookup items.
///
/// Declaration identifiers aren't kept in `ResolvedData`, which is searched for by
/// `lookup_resolved_generic_item_by_ptr` and `lookup_resolved_concrete_item_by_ptr`.
/// Therefore, we have to look for these ourselves.
///
/// Code from <https://github.com/software-mansion/cairols/blob/3f21ccd34862a48702b2b36f992de95402aa4f2d/src/lang/defs/finder.rs#L498>.
fn try_variable_declaration<'db>(
    db: &'db dyn Database,
    identifier: &TerminalIdentifier<'db>,
    lookup_items: &[LookupItemId<'db>],
) -> Option<cairo_lang_defs::ids::VarId<'db>> {
    let function_id = lookup_items.first()?.function_with_body()?;

    // Look at function parameters.
    if let Some(param) = identifier
        .as_syntax_node()
        .parent_of_type::<ast::Param<'_>>(db)
        .filter(|param| param.name(db) == *identifier)
    {
        // Closures have different semantic model structures than regular functions.
        let params = if let Some(expr_closure_ast) =
            param.as_syntax_node().ancestor_of_type::<ast::ExprClosure<'_>>(db)
        {
            let expr_id =
                db.lookup_expr_by_ptr(function_id, expr_closure_ast.stable_ptr(db).into()).ok()?;

            let Expr::ExprClosure(expr_closure_semantic) = db.expr_semantic(function_id, expr_id)
            else {
                // Break in case Expr::Missing was here.
                return None;
            };
            expr_closure_semantic.params
        } else {
            let signature = db.function_with_body_signature(function_id).ok()?;
            signature.params.clone()
        };

        if let Some(param) =
            params.into_iter().find(|param| param.stable_ptr == identifier.stable_ptr(db))
        {
            let var_id = cairo_lang_defs::ids::VarId::Param(param.id);
            return Some(var_id);
        }
    }

    // Look at identifier patterns in the function body.
    if let Some(pattern_ast) = identifier.as_syntax_node().ancestor_of_type::<ast::Pattern<'_>>(db)
    {
        let pattern_id = db.lookup_pattern_by_ptr(function_id, pattern_ast.stable_ptr(db)).ok()?;
        let pattern = db.pattern_semantic(function_id, pattern_id);

        let pattern_variable = pattern
            .variables(&QueryPatternVariablesFromDb(db, function_id))
            .into_iter()
            .find(|var| var.name == identifier.text(db))?;
        let var_id = cairo_lang_defs::ids::VarId::Local(pattern_variable.var.id);
        return Some(var_id);
    }

    None
}

/// Searches for a variable corresponding to the identifier using
/// `lookup_resolved_generic_item_by_ptr`.
fn lookup_variable_in_resolved_items<'db>(
    db: &'db dyn Database,
    identifier: &TerminalIdentifier<'db>,
    lookup_items: &[LookupItemId<'db>],
) -> Option<cairo_lang_defs::ids::VarId<'db>> {
    let ptr = identifier.stable_ptr(db);

    for &lookup_item_id in lookup_items {
        if let Some(ResolvedGenericItem::Variable(var)) =
            db.lookup_resolved_generic_item_by_ptr(lookup_item_id, ptr)
        {
            return Some(var);
        }
    }

    None
}
