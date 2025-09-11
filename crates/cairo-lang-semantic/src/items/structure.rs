use std::sync::Arc;

use cairo_lang_defs::db::DefsGroup;
use cairo_lang_defs::ids::{
    LanguageElementId, LookupItemId, MemberId, MemberLongId, ModuleItemId, StructId,
};
use cairo_lang_diagnostics::{Diagnostics, Maybe};
use cairo_lang_filesystem::ids::StrRef;
use cairo_lang_proc_macros::{DebugWithDb, SemanticObject};
use cairo_lang_syntax::attribute::structured::{Attribute, AttributeListStructurize};
use cairo_lang_syntax::node::{Terminal, TypedStablePtr, TypedSyntaxNode};
use cairo_lang_utils::Intern;
use cairo_lang_utils::ordered_hash_map::OrderedHashMap;
use salsa::Database;

use super::attribute::SemanticQueryAttrs;
use super::generics::{GenericParamsData, semantic_generic_params};
use super::visibility::Visibility;
use crate::diagnostic::SemanticDiagnosticKind::*;
use crate::diagnostic::{SemanticDiagnostics, SemanticDiagnosticsBuilder};
use crate::expr::inference::InferenceId;
use crate::expr::inference::canonic::ResultNoErrEx;
use crate::resolve::{Resolver, ResolverData};
use crate::substitution::{GenericSubstitution, SemanticRewriter};
use crate::types::{ConcreteStructId, add_type_based_diagnostics, resolve_type};
use crate::{GenericParam, SemanticDiagnostic, semantic};

#[cfg(test)]
#[path = "structure_test.rs"]
mod test;

// TODO(spapini): Check for bad recursive types - those that will fail in Sierra generation.

// Declaration.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct StructDeclarationData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    generic_params: Vec<semantic::GenericParam<'db>>,
    attributes: Vec<Attribute<'db>>,
    resolver_data: Arc<ResolverData<'db>>,
}

/// Implementation of [StructSemantic::priv_struct_declaration_data].
fn priv_struct_declaration_data<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<StructDeclarationData<'db>> {
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?;

    // Generic params.
    let generic_params_data = db.struct_generic_params_data(struct_id)?;
    let generic_params = generic_params_data.generic_params;
    let inference_id = InferenceId::LookupItemDeclaration(LookupItemId::ModuleItem(
        ModuleItemId::Struct(struct_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    let attributes = struct_ast.attributes(db).structurize(db);

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(StructDeclarationData {
        diagnostics: diagnostics.build(),
        generic_params,
        attributes,
        resolver_data,
    })
}

/// Query implementation of [StructSemantic::priv_struct_declaration_data].
#[salsa::tracked]
fn priv_struct_declaration_data_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<StructDeclarationData<'db>> {
    priv_struct_declaration_data(db, struct_id)
}

/// Implementation of [StructSemantic::struct_declaration_diagnostics].
fn struct_declaration_diagnostics<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    db.priv_struct_declaration_data(struct_id).map(|data| data.diagnostics).unwrap_or_default()
}

/// Query implementation of [StructSemantic::struct_declaration_diagnostics].
#[salsa::tracked]
fn struct_declaration_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    struct_declaration_diagnostics(db, struct_id)
}

/// Implementation of [StructSemantic::struct_generic_params].
fn struct_generic_params<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    db.struct_generic_params_data(struct_id).map(|data| data.generic_params)
}

/// Query implementation of [StructSemantic::struct_generic_params].
#[salsa::tracked]
fn struct_generic_params_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Vec<GenericParam<'db>>> {
    struct_generic_params(db, struct_id)
}

/// Implementation of [StructSemantic::struct_generic_params_data].
fn struct_generic_params_data<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    let module_file_id = struct_id.module_file_id(db);
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?;
    // Generic params.
    let inference_id =
        InferenceId::LookupItemGenerics(LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id)));
    let mut resolver = Resolver::new(db, module_file_id, inference_id);
    resolver.set_feature_config(&struct_id, &struct_ast, &mut diagnostics);
    let generic_params = semantic_generic_params(
        db,
        &mut diagnostics,
        &mut resolver,
        module_file_id,
        &struct_ast.generic_params(db),
    );
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr(db).untyped());

    let generic_params = inference.rewrite(generic_params).no_err();
    let resolver_data = Arc::new(resolver.data);
    Ok(GenericParamsData { generic_params, diagnostics: diagnostics.build(), resolver_data })
}

/// Query implementation of [StructSemantic::struct_generic_params_data].
#[salsa::tracked]
fn struct_generic_params_data_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<GenericParamsData<'db>> {
    struct_generic_params_data(db, struct_id)
}

/// Implementation of [StructSemantic::struct_attributes].
fn struct_attributes<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.attributes)
}

/// Query implementation of [StructSemantic::struct_attributes].
#[salsa::tracked]
fn struct_attributes_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Vec<Attribute<'db>>> {
    struct_attributes(db, struct_id)
}

/// Implementation of [StructSemantic::struct_declaration_resolver_data].
fn struct_declaration_resolver_data<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_struct_declaration_data(struct_id)?.resolver_data)
}

/// Query implementation of [StructSemantic::struct_declaration_resolver_data].
#[salsa::tracked]
fn struct_declaration_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    struct_declaration_resolver_data(db, struct_id)
}

// Definition.
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, salsa::Update)]
#[debug_db(dyn Database)]
pub struct StructDefinitionData<'db> {
    diagnostics: Diagnostics<'db, SemanticDiagnostic<'db>>,
    members: Arc<OrderedHashMap<StrRef<'db>, Member<'db>>>,
    resolver_data: Arc<ResolverData<'db>>,
}
#[derive(Clone, Debug, PartialEq, Eq, DebugWithDb, SemanticObject, salsa::Update)]
#[debug_db(dyn Database)]
pub struct Member<'db> {
    pub id: MemberId<'db>,
    pub ty: semantic::TypeId<'db>,
    #[dont_rewrite]
    pub visibility: Visibility,
}

/// Implementation of [StructSemantic::priv_struct_definition_data].
fn priv_struct_definition_data<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<StructDefinitionData<'db>> {
    let module_file_id = struct_id.module_file_id(db);
    let crate_id = module_file_id.0.owning_crate(db);
    let mut diagnostics = SemanticDiagnostics::default();
    // TODO(spapini): when code changes in a file, all the AST items change (as they contain a path
    // to the green root that changes. Once ASTs are rooted on items, use a selector that picks only
    // the item instead of all the module data.
    // TODO(spapini): Add generic args when they are supported on structs.
    let struct_ast = db.module_struct_by_id(struct_id)?;

    // Generic params.
    let generic_params_data = db.struct_generic_params_data(struct_id)?;
    let inference_id = InferenceId::LookupItemDefinition(LookupItemId::ModuleItem(
        ModuleItemId::Struct(struct_id),
    ));
    let mut resolver = Resolver::with_data(
        db,
        (*generic_params_data.resolver_data).clone_with_inference_id(db, inference_id),
    );
    diagnostics.extend(generic_params_data.diagnostics);

    // Members.
    let mut members = OrderedHashMap::default();
    for member in struct_ast.members(db).elements(db) {
        let feature_restore =
            resolver.extend_feature_config_from_item(db, crate_id, &mut diagnostics, &member);
        let id = MemberLongId(module_file_id, member.stable_ptr(db)).intern(db);
        let ty = resolve_type(db, &mut diagnostics, &mut resolver, &member.type_clause(db).ty(db));
        let visibility = Visibility::from_ast(db, &mut diagnostics, &member.visibility(db));
        let member_name = member.name(db).text(db).into();
        if let Some(_other_member) = members.insert(member_name, Member { id, ty, visibility }) {
            diagnostics
                .report(member.stable_ptr(db), StructMemberRedefinition { struct_id, member_name });
        }
        resolver.restore_feature_config(feature_restore);
    }

    // Check fully resolved.
    let inference = &mut resolver.inference();
    inference.finalize(&mut diagnostics, struct_ast.stable_ptr(db).untyped());

    for (_, member) in members.iter_mut() {
        member.ty = inference.rewrite(member.ty).no_err();
    }

    let resolver_data = Arc::new(resolver.data);
    Ok(StructDefinitionData {
        diagnostics: diagnostics.build(),
        members: members.into(),
        resolver_data,
    })
}

/// Query implementation of [StructSemantic::priv_struct_definition_data].
#[salsa::tracked]
fn priv_struct_definition_data_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<StructDefinitionData<'db>> {
    priv_struct_definition_data(db, struct_id)
}

/// Implementation of [StructSemantic::struct_definition_diagnostics].
fn struct_definition_diagnostics<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    let Ok(data) = db.priv_struct_definition_data(struct_id) else {
        return Default::default();
    };

    let crate_id = data.resolver_data.module_file_id.0.owning_crate(db);

    // If the struct is a phantom type, no need to check if its members are fully valid types, as
    // they won't be used.
    if db
        .declared_phantom_type_attributes(crate_id)
        .iter()
        .any(|attr| struct_id.has_attr(db, attr).unwrap_or_default())
    {
        return data.diagnostics;
    }
    let mut diagnostics = SemanticDiagnostics::from(data.diagnostics);
    for (_, member) in data.members.iter() {
        let stable_ptr = member.id.stable_ptr(db);
        add_type_based_diagnostics(db, &mut diagnostics, member.ty, stable_ptr);
        if member.ty.is_phantom(db) {
            diagnostics.report(stable_ptr, NonPhantomTypeContainingPhantomType);
        }
    }
    diagnostics.build()
}

/// Query implementation of [StructSemantic::struct_definition_diagnostics].
#[salsa::tracked]
fn struct_definition_diagnostics_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
    struct_definition_diagnostics(db, struct_id)
}

/// Implementation of [StructSemantic::struct_members].
fn struct_members<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, Member<'db>>>> {
    Ok(db.priv_struct_definition_data(struct_id)?.members)
}

/// Query implementation of [StructSemantic::struct_members].
#[salsa::tracked]
fn struct_members_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, Member<'db>>>> {
    struct_members(db, struct_id)
}

/// Implementation of [StructSemantic::struct_definition_resolver_data].
fn struct_definition_resolver_data<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    Ok(db.priv_struct_definition_data(struct_id)?.resolver_data)
}

/// Query implementation of [StructSemantic::struct_definition_resolver_data].
#[salsa::tracked]
fn struct_definition_resolver_data_tracked<'db>(
    db: &'db dyn Database,
    struct_id: StructId<'db>,
) -> Maybe<Arc<ResolverData<'db>>> {
    struct_definition_resolver_data(db, struct_id)
}

/// Implementation of [StructSemantic::concrete_struct_members].
fn concrete_struct_members<'db>(
    db: &'db dyn Database,
    concrete_struct_id: ConcreteStructId<'db>,
) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>> {
    // TODO(spapini): Uphold the invariant that constructed ConcreteEnumId instances
    //   always have the correct number of generic arguments.
    let generic_params = db.struct_generic_params(concrete_struct_id.struct_id(db))?;
    let generic_args = concrete_struct_id.long(db).generic_args.clone();
    let substitution = GenericSubstitution::new(&generic_params, &generic_args);

    let generic_members = db.struct_members(concrete_struct_id.struct_id(db))?;
    Ok(Arc::new(
        generic_members
            .iter()
            .map(|(name, member)| {
                let ty = substitution.substitute(db, member.ty)?;
                Ok((*name, semantic::Member { ty, ..member.clone() }))
            })
            .collect::<Maybe<_>>()?,
    ))
}

/// Query implementation of [StructSemantic::concrete_struct_members].
#[salsa::tracked]
fn concrete_struct_members_tracked<'db>(
    db: &'db dyn Database,
    concrete_struct_id: ConcreteStructId<'db>,
) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>> {
    concrete_struct_members(db, concrete_struct_id)
}

/// Trait for struct-related semantic queries.
pub trait StructSemantic<'db>: Database {
    /// Private query to compute data about a struct declaration.
    fn priv_struct_declaration_data(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<StructDeclarationData<'db>> {
        priv_struct_declaration_data_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the declaration diagnostics of a struct.
    fn struct_declaration_diagnostics(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        struct_declaration_diagnostics_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the attributes of a struct.
    fn struct_attributes(&'db self, struct_id: StructId<'db>) -> Maybe<Vec<Attribute<'db>>> {
        struct_attributes_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the generic parameters of a struct.
    fn struct_generic_params(&'db self, struct_id: StructId<'db>) -> Maybe<Vec<GenericParam<'db>>> {
        struct_generic_params_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the generic parameters data of a struct.
    fn struct_generic_params_data(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<GenericParamsData<'db>> {
        struct_generic_params_data_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the resolution resolved_items of a struct declaration.
    fn struct_declaration_resolver_data(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        struct_declaration_resolver_data_tracked(self.as_dyn_database(), structure_id)
    }

    /// Private query to compute data about a struct definition.
    fn priv_struct_definition_data(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<StructDefinitionData<'db>> {
        priv_struct_definition_data_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the semantic diagnostics of a struct definition.
    fn struct_definition_diagnostics(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Diagnostics<'db, SemanticDiagnostic<'db>> {
        struct_definition_diagnostics_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the members of a struct.
    fn struct_members(
        &'db self,
        struct_id: StructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>> {
        struct_members_tracked(self.as_dyn_database(), struct_id)
    }

    /// Returns the resolution resolved_items of a struct definition.
    fn struct_definition_resolver_data(
        &'db self,
        structure_id: StructId<'db>,
    ) -> Maybe<Arc<ResolverData<'db>>> {
        struct_definition_resolver_data_tracked(self.as_dyn_database(), structure_id)
    }

    /// Returns the concrete members of a struct.
    fn concrete_struct_members(
        &'db self,
        concrete_struct_id: ConcreteStructId<'db>,
    ) -> Maybe<Arc<OrderedHashMap<StrRef<'db>, semantic::Member<'db>>>> {
        concrete_struct_members_tracked(self.as_dyn_database(), concrete_struct_id)
    }
}

impl<'db, T: Database + ?Sized> StructSemantic<'db> for T {}
