use std::vec;

use db_utils::define_short_id;
use defs::ids::{
    GenericFunctionId, GenericParamId, ImplFunctionId, ImplFunctionLongId, ImplId,
    LanguageElementId, ModuleId,
};
use diagnostics::Diagnostics;
use diagnostics_proc_macros::DebugWithDb;
use syntax::node::ast::{self, Item, MaybeImplBody};
use syntax::node::db::SyntaxGroup;
use syntax::node::ids::SyntaxStablePtrId;
use syntax::node::TypedSyntaxNode;
use utils::ordered_hash_map::OrderedHashMap;
use utils::{extract_matches, try_extract_matches, OptionHelper};

use super::attribute::{ast_attributes_to_semantic, Attribute};
use super::enm::SemanticEnumEx;
use super::functions::{
    function_signature_implicit_parameters, function_signature_params,
    function_signature_return_type,
};
use super::generics::semantic_generic_params;
use super::strct::SemanticStructEx;
use crate::corelib::{copy_trait, drop_trait};
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::SemanticDiagnostics;
use crate::expr::compute::Environment;
use crate::resolve_path::{ResolvedConcreteItem, ResolvedGenericItem, Resolver};
use crate::{
    semantic, ConcreteTraitId, ConcreteTraitLongId, GenericArgumentId, SemanticDiagnostic, TypeId,
    TypeLongId,
};

#[cfg(test)]
#[path = "imp_test.rs"]
mod test;

#[derive(Clone, Debug, Hash, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ConcreteImplLongId {
    pub impl_id: ImplId,
    pub generic_args: Vec<GenericArgumentId>,
}
define_short_id!(ConcreteImplId, ConcreteImplLongId, SemanticGroup, lookup_intern_concrete_impl);

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    generic_params: Vec<GenericParamId>,
    /// The concrete trait this impl implements, or None if cannot be resolved.
    concrete_trait: Option<ConcreteTraitId>,
    attributes: Vec<Attribute>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_declaration_diagnostics].
pub fn impl_semantic_declaration_diagnostics(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_declaration_data(impl_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [crate::db::SemanticGroup::impl_generic_params].
pub fn impl_generic_params(db: &dyn SemanticGroup, impl_id: ImplId) -> Option<Vec<GenericParamId>> {
    Some(db.priv_impl_declaration_data(impl_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_declaration_data].
pub fn priv_impl_declaration_data(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Option<ImplDeclarationData> {
    // TODO(spapini): When asts are rooted on items, don't query module_data directly. Use a
    // selector.
    let module_id = impl_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let module_data = db.module_data(module_id)?;
    let impl_ast = module_data.impls.get(&impl_id)?;
    let syntax_db = db.upcast();

    // Generic params.
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &impl_ast.generic_params(syntax_db),
    );
    let mut resolver = Resolver::new(db, module_id, &generic_params);

    let trait_path_syntax = impl_ast.trait_path(syntax_db);
    let concrete_trait = resolver
        .resolve_concrete_path(&mut diagnostics, &trait_path_syntax)
        .and_then(|option_concrete_path| {
            try_extract_matches!(option_concrete_path, ResolvedConcreteItem::Trait)
                .on_none(|| diagnostics.report(&trait_path_syntax, NotATrait))
        });

    let attributes = ast_attributes_to_semantic(syntax_db, impl_ast.attributes(syntax_db));
    Some(ImplDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        concrete_trait,
        attributes,
    })
}

#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplDefinitionData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    function_asts: OrderedHashMap<ImplFunctionId, ast::ItemFreeFunction>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_semantic_definition_diagnostics].
pub fn impl_semantic_definition_diagnostics(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Diagnostics<SemanticDiagnostic> {
    db.priv_impl_definition_data(impl_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// An helper function to report diagnostics in priv_impl_definition_data.
fn report_invalid_in_impl<Terminal: syntax::node::Terminal>(
    syntax_db: &dyn SyntaxGroup,
    diagnostics: &mut SemanticDiagnostics,
    kw_terminal: Terminal,
) {
    diagnostics.report_by_ptr(
        kw_terminal.as_syntax_node().stable_ptr(),
        InvalidImplItem { item_kw: kw_terminal.text(syntax_db) },
    );
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_definition_data].
pub fn priv_impl_definition_data(
    db: &dyn SemanticGroup,
    impl_id: ImplId,
) -> Option<ImplDefinitionData> {
    let module_id = impl_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);

    let declaration_data = db.priv_impl_declaration_data(impl_id)?;
    let concrete_trait = declaration_data.concrete_trait?;

    let module_data = db.module_data(module_id)?;
    let impl_ast = module_data.impls.get(&impl_id)?;
    let syntax_db = db.upcast();

    let lookup_context = ImplLookupContext {
        module_id,
        extra_modules: vec![],
        generic_params: declaration_data.generic_params,
    };
    check_special_impls(
        db,
        &mut diagnostics,
        lookup_context,
        concrete_trait,
        impl_ast.stable_ptr().untyped(),
    );

    let mut function_asts = OrderedHashMap::default();

    if let MaybeImplBody::Some(body) = impl_ast.body(syntax_db) {
        for item in body.items(syntax_db).elements(syntax_db) {
            match item {
                Item::Module(module) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, module.module_kw(syntax_db))
                }

                Item::Use(use_item) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, use_item.use_kw(syntax_db))
                }
                Item::ExternFunction(extern_func) => report_invalid_in_impl(
                    syntax_db,
                    &mut diagnostics,
                    extern_func.extern_kw(syntax_db),
                ),
                Item::ExternType(extern_type) => report_invalid_in_impl(
                    syntax_db,
                    &mut diagnostics,
                    extern_type.extern_kw(syntax_db),
                ),
                Item::Trait(trt) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, trt.trait_kw(syntax_db))
                }
                Item::Impl(imp) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, imp.impl_kw(syntax_db))
                }
                Item::Struct(strct) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, strct.struct_kw(syntax_db))
                }
                Item::Enum(enm) => {
                    report_invalid_in_impl(syntax_db, &mut diagnostics, enm.enum_kw(syntax_db))
                }

                Item::FreeFunction(func) => {
                    function_asts.insert(
                        db.intern_impl_function(ImplFunctionLongId(module_id, func.stable_ptr())),
                        func,
                    );
                }
            }
        }
    }

    Some(ImplDefinitionData { diagnostics: diagnostics.build(), function_asts })
}

/// Query implementation of [crate::db::SemanticGroup::impl_functions].
pub fn impl_functions(db: &dyn SemanticGroup, impl_id: ImplId) -> Option<Vec<ImplFunctionId>> {
    Some(db.priv_impl_definition_data(impl_id)?.function_asts.keys().copied().collect())
}

/// Handle special cases such as Copy and Drop checking.
fn check_special_impls(
    db: &dyn SemanticGroup,
    diagnostics: &mut SemanticDiagnostics,
    lookup_context: ImplLookupContext,
    concrete_trait: ConcreteTraitId,
    stable_ptr: SyntaxStablePtrId,
) -> Option<()> {
    let ConcreteTraitLongId { trait_id, generic_args } =
        db.lookup_intern_concrete_trait(concrete_trait);
    let copy = copy_trait(db);
    let drop = drop_trait(db);

    if trait_id == copy {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty))
            .all(|info| info.duplicatable)
        {
            diagnostics.report_by_ptr(stable_ptr, InvalidCopyTraitImpl);
            return None;
        }
    }
    if trait_id == drop {
        let tys = get_inner_types(db, extract_matches!(generic_args[0], GenericArgumentId::Type))?;
        if !tys
            .into_iter()
            .filter_map(|ty| db.type_info(lookup_context.clone(), ty))
            .all(|info| info.droppable)
        {
            diagnostics.report_by_ptr(stable_ptr, InvalidDropTraitImpl);
            return None;
        }
    }

    Some(())
}

/// Retrieves all the inner types (members of a struct / tuple or variants of an enum).
/// These are the types that are required to implement some trait,
/// in order for the original type to be able to implement this trait.
fn get_inner_types(db: &dyn SemanticGroup, ty: TypeId) -> Option<Vec<TypeId>> {
    Some(match db.lookup_intern_type(ty) {
        TypeLongId::Concrete(concrete_type_id) => {
            // Look for Copy and Drop trait in the defining module.
            match concrete_type_id {
                crate::ConcreteTypeId::Struct(concrete_struct_id) => db
                    .concrete_struct_members(concrete_struct_id)?
                    .values()
                    .map(|member| member.ty)
                    .collect(),
                crate::ConcreteTypeId::Enum(concrete_enum_id) => db
                    .concrete_enum_variants(concrete_enum_id)?
                    .into_iter()
                    .map(|variant| variant.ty)
                    .collect(),
                crate::ConcreteTypeId::Extern(_) => vec![],
            }
        }
        TypeLongId::Tuple(tys) => tys,
        TypeLongId::GenericParameter(_) | TypeLongId::Never | TypeLongId::Missing => {
            return None;
        }
    })
}

/// Query implementation of [crate::db::SemanticGroup::find_impls_at_module].
pub fn find_impls_at_module(
    db: &dyn SemanticGroup,
    module_id: ModuleId,
    concrete_trait_id: ConcreteTraitId,
) -> Option<Vec<ConcreteImplId>> {
    let mut res = Vec::new();
    let impls = db.module_data(module_id)?.impls;
    // TODO(spapini): Index better.
    for impl_id in impls.keys().copied() {
        let Some(imp_data)= db.priv_impl_declaration_data(impl_id) else {continue};
        if !imp_data.generic_params.is_empty() {
            // TODO(spapini): Infer generics and substitute.
            continue;
        }

        if imp_data.concrete_trait == Some(concrete_trait_id) {
            let concrete_impl_id =
                db.intern_concrete_impl(ConcreteImplLongId { impl_id, generic_args: vec![] });
            res.push(concrete_impl_id);
        }
    }
    Some(res)
}

#[allow(dead_code)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ImplLookupContext {
    pub module_id: ModuleId,
    pub extra_modules: Vec<ModuleId>,
    pub generic_params: Vec<GenericParamId>,
}

/// Finds all the implementations for a concrete trait, in a specific lookup context.
pub fn find_impls_at_context(
    db: &dyn SemanticGroup,
    lookup_context: &ImplLookupContext,
    concrete_trait_id: ConcreteTraitId,
) -> Option<Vec<ConcreteImplId>> {
    let mut res = Vec::new();
    // TODO(spapini): Lookup in generic_params once impl generic params are supported.
    res.extend(find_impls_at_module(db, lookup_context.module_id, concrete_trait_id)?);
    for module_id in &lookup_context.extra_modules {
        if let Some(imps) = find_impls_at_module(db, *module_id, concrete_trait_id) {
            res.extend(imps);
        }
    }
    for submodule in db.module_submodules(lookup_context.module_id)? {
        res.extend(find_impls_at_module(db, submodule, concrete_trait_id)?);
    }
    for use_id in db.module_data(lookup_context.module_id)?.uses.keys() {
        if let Some(ResolvedGenericItem::Module(submodule)) = db.use_resolved_item(*use_id) {
            res.extend(find_impls_at_module(db, submodule, concrete_trait_id)?);
        }
    }
    Some(res)
}

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb)]
#[debug_db(dyn SemanticGroup + 'static)]
pub struct ImplFunctionDeclarationData {
    diagnostics: Diagnostics<SemanticDiagnostic>,
    signature: semantic::Signature,
    generic_params: Vec<GenericParamId>,
    // TODO(ilya): Do we need Environment like in a free function?
    attributes: Vec<Attribute>,
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_signature].
pub fn impl_function_signature(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Option<semantic::Signature> {
    Some(db.priv_impl_function_data(impl_function_id)?.signature)
}

/// Query implementation of [crate::db::SemanticGroup::impl_function_generic_params].
pub fn impl_function_generic_params(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Option<Vec<GenericParamId>> {
    Some(db.priv_impl_function_data(impl_function_id)?.generic_params)
}

/// Query implementation of [crate::db::SemanticGroup::priv_impl_function_data].
pub fn priv_impl_function_declaration_data(
    db: &dyn SemanticGroup,
    impl_function_id: ImplFunctionId,
) -> Option<ImplFunctionDeclarationData> {
    let module_id = impl_function_id.module(db.upcast());
    let mut diagnostics = SemanticDiagnostics::new(module_id);
    let impl_id = impl_function_id.impl_id(db.upcast());
    let data = db.priv_impl_definition_data(impl_id)?;
    let function_syntax = &data.function_asts[impl_function_id];
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        module_id,
        &function_syntax.generic_params(db.upcast()),
    );
    let mut resolver = Resolver::new(db, module_id, &generic_params);
    let syntax_db = db.upcast();
    let signature_syntax = function_syntax.signature(syntax_db);
    let return_type =
        function_signature_return_type(&mut diagnostics, db, &mut resolver, &signature_syntax);
    let mut environment = Environment::default();
    let params = function_signature_params(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        GenericFunctionId::ImplFunction(impl_function_id),
        &mut environment,
    );

    // TODO(ilya): Check that the signature is consistent with the relevant item in the trait.

    let implicits = function_signature_implicit_parameters(
        &mut diagnostics,
        db,
        &mut resolver,
        &signature_syntax,
        GenericFunctionId::ImplFunction(impl_function_id),
        &mut environment,
    );

    let attributes = ast_attributes_to_semantic(syntax_db, function_syntax.attributes(syntax_db));

    Some(ImplFunctionDeclarationData {
        diagnostics: diagnostics.build(),
        signature: semantic::Signature { params, return_type, implicits },
        generic_params,
        attributes,
    })
}
