#[expect(clippy::disallowed_types)]
use std::collections::HashMap;

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
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use cairo_lang_utils::{CloneableDatabase, Intern};
use itertools::Itertools;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use salsa::Database;

use crate::debug_info::function_debug_info::serializable::{
    CairoVariableName, SerializableAllFunctionsDebugInfo, SerializableFunctionDebugInfo,
    SerializableParameterInfo, SierraFunctionId, SierraVarId,
};
use crate::debug_info::{
    SourceCodeLocation, SourceCodeSpan, SourceFileFullPath, maybe_code_location,
};

pub mod serializable;

/// The debug info of all Sierra functions in the program.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AllFunctionsDebugInfo<'db>(OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>);

impl<'db> AllFunctionsDebugInfo<'db> {
    pub fn new(value: OrderedHashMap<FunctionId, FunctionDebugInfo<'db>>) -> Self {
        Self(value)
    }

    pub fn extract_serializable_debug_info(
        &self,
        db: &'db dyn CloneableDatabase,
    ) -> SerializableAllFunctionsDebugInfo {
        SerializableAllFunctionsDebugInfo(
            self.0
                .iter()
                .collect_vec()
                .into_par_iter()
                .map_with(db.dyn_clone(), |db, (function_id, function_debug_info)| {
                    Some((
                        SierraFunctionId(function_id.id),
                        function_debug_info.extract_serializable_debug_info(db.as_ref())?,
                    ))
                })
                .flatten()
                .collect(),
        )
    }
}

/// The debug info of a Sierra function.
/// Contains a signature location and locations of Sierra variables of this function.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunctionDebugInfo<'db> {
    pub signature_location: LocationId<'db>,
    /// All source locations a Sierra variable is referenced from (allocation site plus every
    /// statement that reads or writes it).
    pub variables_locations: OrderedHashMap<VarId, Vec<LocationId<'db>>>,
    /// Sierra var ids of function parameters, in declaration order.
    pub parameter_var_ids: Vec<VarId>,
}

impl<'db> FunctionDebugInfo<'db> {
    fn extract_serializable_debug_info(
        &self,
        db: &'db dyn Database,
    ) -> Option<SerializableFunctionDebugInfo> {
        let (function_file_path, function_code_span) = self.extract_location(db)?;
        let sierra_to_cairo_variables = self.extract_variables_mapping(db);
        let parameters = self.extract_parameters(db);

        Some(SerializableFunctionDebugInfo {
            function_file_path,
            function_code_span,
            sierra_to_cairo_variables,
            parameters,
        })
    }

    /// Extracts mapping from a sierra variable to the cairo variables it corresponds to during
    /// execution of the function code.
    #[expect(clippy::disallowed_types)]
    fn extract_variables_mapping(
        &self,
        db: &'db dyn Database,
    ) -> HashMap<SierraVarId, Vec<(CairoVariableName, SourceCodeSpan)>> {
        self.variables_locations
            .iter()
            .map(|(sierra_var, locations)| {
                let mut infos: Vec<(CairoVariableName, SourceCodeSpan)> = Vec::new();
                for loc in locations {
                    if let Some(info) = resolve_location_to_cairo_info(db, *loc)
                        && !infos.contains(&info)
                    {
                        infos.push(info);
                    }
                }
                (SierraVarId(sierra_var.id), infos)
            })
            .filter(|(_, infos)| !infos.is_empty())
            .collect()
    }

    /// Extracts the debug info of the function's parameters in declaration order.
    fn extract_parameters(&self, db: &'db dyn Database) -> Vec<SerializableParameterInfo> {
        let Some(param_list) = self.function_param_list(db) else {
            return Vec::new();
        };
        let params = param_list.elements(db);

        // Lowering prepends implicit parameters (gas builtin, range check, ...) to
        // `Lowered::parameters`, so `parameter_var_ids` may have leading entries with no AST
        // counterpart. `saturating_sub` because the inverse (AST having more params than the
        // lowered list) cannot happen.
        let implicit_count = self.parameter_var_ids.len().saturating_sub(params.len());

        self.parameter_var_ids
            .iter()
            .skip(implicit_count)
            .zip(params)
            .map(|(sierra_var, param_ast)| {
                let name_ident = param_ast.name(db);
                let name: CairoVariableName = name_ident.text(db).to_string(db);
                let (_, definition_span, _) = maybe_code_location(
                    db,
                    StableLocation::new(name_ident.stable_ptr(db).untyped()),
                )
                .expect("parameter AST nodes must have a source location");
                SerializableParameterInfo {
                    sierra_var_id: SierraVarId(sierra_var.id),
                    name,
                    definition_span,
                }
            })
            .collect()
    }

    /// Returns the AST `ParamList` of the function this debug info belongs to.
    fn function_param_list(&self, db: &'db dyn Database) -> Option<ParamList<'db>> {
        let function_node =
            self.signature_location.long(db).stable_location.syntax_node(db).ancestor_of_kinds(
                db,
                &[SyntaxKind::FunctionWithBody, SyntaxKind::TraitItemFunction],
            )?;
        let signature = match function_node.kind(db) {
            SyntaxKind::FunctionWithBody => {
                ast::FunctionWithBody::from_syntax_node(db, function_node)
                    .declaration(db)
                    .signature(db)
            }
            SyntaxKind::TraitItemFunction => {
                ast::TraitItemFunction::from_syntax_node(db, function_node)
                    .declaration(db)
                    .signature(db)
            }
            _ => return None,
        };
        Some(signature.parameters(db))
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

/// Resolves a single source location to a `(name, span)` tuple for the corresponding Cairo
/// variable, or `None` if no Cairo identifier can be associated with the location.
fn resolve_location_to_cairo_info<'db>(
    db: &'db dyn Database,
    code_location: LocationId<'db>,
) -> Option<(CairoVariableName, SourceCodeSpan)> {
    // Ignore inline locations for now as this function is supposed to be called only
    // when sierra was compiled with inlining set to `avoid`.
    // TODO(pm): handle them to make sure user function marked #[inline(always)] work.
    //  https://github.com/software-mansion-labs/cairo-debugger/issues/41
    let Location { stable_location: sierra_var_location, notes: _, inline_locations: _ } =
        code_location.long(db);

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

    let definition = try_variable_declaration(db, &identifier, &lookup_items)
        .or_else(|| lookup_variable_in_resolved_items(db, &identifier, &lookup_items))?;

    let location = match definition {
        // Extract only param name.
        cairo_lang_defs::ids::VarId::Param(param) => {
            StableLocation::new(param.stable_ptr(db).lookup(db).name(db).stable_ptr(db).untyped())
        }
        x => x.stable_location(db),
    };
    let user_location = location.span_in_file(db).user_location(db);

    let var_name =
        user_location.span.take(db.file_content(user_location.file_id).unwrap()).to_string();

    let position = user_location.span.position_in_file(db, user_location.file_id)?;
    let source_location = SourceCodeSpan {
        start: SourceCodeLocation { col: position.start.col, line: position.start.line },
        end: SourceCodeLocation { col: position.end.col, line: position.end.line },
    };

    Some((var_name, source_location))
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

/// Tries to find an identifier in the node unambiguously, meaning if either:
/// - the node is an identifier
/// - the node is a token of an identifier,
/// - there is exactly one identifier in descendants of the node,
///
/// it returns the aforementioned identifier.
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

#[cfg(test)]
mod tests {
    // NOTE: `resolve_location_to_cairo_info` relies on `find_module_containing_node`,
    // which only resolves modules in `CrateLongId::Real` crates. The test harness
    // creates `Virtual` crates, so per-Sierra-var Cairo identifier resolution silently
    // returns `None` here. These tests therefore cover what is reachable end-to-end:
    // parameter extraction (independent of that lookup) and the per-var location
    // aggregation in the in-memory `FunctionDebugInfo`.
    use cairo_lang_filesystem::ids::FileLongId;
    use cairo_lang_semantic::test_utils::setup_test_crate;

    use super::*;
    use crate::db::SierraGenGroup;
    use crate::test_utils::SierraGenDatabaseForTesting;

    /// Compiles `content` against `db` and invokes `f` with the first `FunctionDebugInfo`
    /// belonging to the test source (i.e. not pulled from the corelib).
    fn with_first_test_function<R>(
        db: &SierraGenDatabaseForTesting,
        content: &str,
        f: impl FnOnce(&SierraGenDatabaseForTesting, &FunctionDebugInfo<'_>) -> R,
    ) -> R {
        let crate_id = setup_test_crate(db, content);
        let program = db.get_sierra_program(vec![crate_id]).expect("get_sierra_program failed");
        let info = program
            .debug_info
            .functions_info
            .0
            .values()
            .find(|info| {
                let file_id = info.signature_location.long(db).stable_location.file_id(db);
                matches!(
                    file_id.long(db),
                    FileLongId::Virtual(virt) if virt.name.long(db).as_str() == "lib.cairo"
                )
            })
            .expect("test function should be present");
        f(db, info)
    }

    /// Convenience over [`with_first_test_function`] for tests that only inspect the
    /// serializable V2 output. Uses the no-withdraw-gas db so most functions don't pick
    /// up implicit parameters.
    fn serializable_debug_info_for(content: &str) -> SerializableFunctionDebugInfo {
        let db = SierraGenDatabaseForTesting::without_add_withdraw_gas();
        with_first_test_function(&db, content, |db, info| {
            info.extract_serializable_debug_info(db).expect("serializable info")
        })
    }

    #[test]
    fn parameters_emitted_with_source_name() {
        let info = serializable_debug_info_for("fn foo(x: felt252) -> felt252 { let y = x; y }");
        assert_eq!(info.parameters.len(), 1);
        assert_eq!(info.parameters[0].name, "x");
    }

    #[test]
    fn unused_parameter_still_appears() {
        let info = serializable_debug_info_for("fn foo(x: felt252) {}");
        assert_eq!(info.parameters.len(), 1);
        assert_eq!(info.parameters[0].name, "x");
    }

    #[test]
    fn mut_parameter_name_strips_modifier() {
        let info = serializable_debug_info_for("fn foo(mut x: felt252) -> felt252 { x }");
        assert_eq!(info.parameters.len(), 1);
        assert_eq!(info.parameters[0].name, "x");
    }

    #[test]
    fn multiple_parameters_in_declaration_order() {
        let info =
            serializable_debug_info_for("fn foo(a: felt252, b: felt252) -> felt252 { a + b }");
        let names: Vec<_> = info.parameters.iter().map(|p| p.name.clone()).collect();
        assert_eq!(names, vec!["a".to_string(), "b".to_string()]);
    }

    #[test]
    fn parameters_skip_prepended_implicits() {
        // u32 arithmetic pulls in implicit lowering parameters (range check, panic) that
        // are prepended to `Lowered::parameters` and surface as extra leading entries in
        // `parameter_var_ids`. The AST `ParamList` has none, so we must align by the
        // explicit tail.
        let db = SierraGenDatabaseForTesting::default();
        with_first_test_function(&db, "fn foo(x: u32, y: u32) -> u32 { x + y }", |db, info| {
            assert!(
                info.parameter_var_ids.len() > 2,
                "expected implicit params to inflate parameter_var_ids, got {}",
                info.parameter_var_ids.len()
            );
            let serializable = info.extract_serializable_debug_info(db).expect("serializable info");
            let names: Vec<_> = serializable.parameters.iter().map(|p| p.name.clone()).collect();
            assert_eq!(names, vec!["x".to_string(), "y".to_string()]);
        });
    }

    #[test]
    fn alias_chain_aggregates_multiple_locations_per_var() {
        // `let y = x; let z = y; z` collapses to a single Sierra var at lowering, but
        // body statements reference it from multiple sites — so the aggregated location
        // list must contain more than one entry, which is the prerequisite for
        // recovering every Cairo name once the resolver path is reachable.
        let db = SierraGenDatabaseForTesting::without_add_withdraw_gas();
        let max_locations = with_first_test_function(
            &db,
            "fn foo(x: felt252) -> felt252 { let y = x; let z = y; z }",
            |_db, info| info.variables_locations.values().map(|locs| locs.len()).max().unwrap_or(0),
        );
        assert!(
            max_locations >= 2,
            "expected at least one Sierra var with multiple locations, got max={max_locations}"
        );
    }
}
