use std::collections::HashSet;

use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId,
    LanguageElementId, LookupItemId, MemberId, ModuleItemId, ModuleTypeAliasId,
    NamedLanguageElementId, StructId, TopLevelLanguageElementId, TraitConstantId, TraitFunctionId,
    TraitId, TraitItemId, TraitTypeId,
};
use cairo_lang_filesystem::ids::SmolStrId;
use cairo_lang_semantic::items::constant::ConstantSemantic;
use cairo_lang_semantic::items::enm::EnumSemantic;
use cairo_lang_semantic::items::extern_function::ExternFunctionSemantic;
use cairo_lang_semantic::items::extern_type::ExternTypeSemantic;
use cairo_lang_semantic::items::free_function::FreeFunctionSemantic;
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_semantic::items::module::{ModuleItemInfo, ModuleSemantic};
use cairo_lang_semantic::items::module_type_alias::ModuleTypeAliasSemantic;
use cairo_lang_semantic::items::structure::StructSemantic;
use cairo_lang_semantic::items::trt::TraitSemantic;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::lookup_item::HasResolverData;
use cairo_lang_semantic::{Expr, GenericArgumentId, GenericParam, Parameter, TypeId};
use cairo_lang_syntax::attribute::structured::Attribute;
use itertools::Itertools;
use salsa::Database;

use crate::documentable_item::DocumentableItemId;
use crate::documentable_item::DocumentableItemId::Member;
use crate::signature_errors::SignatureError;

/// A helper struct gathering documentable item's signature data.
pub(crate) struct DocumentableItemSignatureData<'db> {
    pub(crate) item_id: DocumentableItemId<'db>,
    pub(crate) name: SmolStrId<'db>,
    pub(crate) visibility: Visibility,
    pub(crate) generic_args: Option<Vec<GenericArgumentId<'db>>>,
    pub(crate) generic_params: Option<Vec<GenericParam<'db>>>,
    pub(crate) variants: Option<Vec<(SmolStrId<'db>, TypeId<'db>)>>,
    pub(crate) members: Option<Vec<(SmolStrId<'db>, TypeId<'db>, Visibility)>>,
    pub(crate) return_type: Option<TypeId<'db>>,
    pub(crate) attributes: Option<Vec<Attribute<'db>>>,
    pub(crate) params: Option<Vec<Parameter<'db>>>,
    pub(crate) resolver_generic_params: Option<Vec<GenericParamId<'db>>>,
    pub(crate) return_value_expr: Option<Expr<'db>>,
    pub(crate) full_path: String,
}

/// A utility function, retrieves [`ModuleItemInfo`] for [`ModuleItemId`].
fn get_module_item_info<'db>(
    db: &'db dyn Database,
    module_item_id: ModuleItemId<'db>,
) -> Result<ModuleItemInfo<'db>, SignatureError> {
    let parent_module = module_item_id.parent_module(db);
    let item_name = module_item_id.name(db);
    if let Some(module_item_info) = db
        .module_item_info_by_name(parent_module, item_name)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db)))?
    {
        Ok(module_item_info)
    } else {
        Err(SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db)))
    }
}

/// Retrieves data for enum signature formatting. Returns [`SignatureError`] if any relevant data
/// could not be retrieved from db.
pub(crate) fn get_enum_signature_data<'db>(
    db: &'db dyn Database,
    item_id: EnumId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::Enum(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let enum_variants = db
        .enum_variants(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db)))?;

    let mut variants = Vec::new();
    for (name, variant_id) in enum_variants.iter() {
        let variant_semantic = db.variant_semantic(item_id, *variant_id).map_err(|_| {
            SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db))
        })?;
        variants.push((*name, variant_semantic.ty));
    }
    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: None,
        variants: Some(variants),
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for struct signature formatting.
pub(crate) fn get_struct_signature_data<'db>(
    db: &'db dyn Database,
    item_id: StructId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::Struct(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let struct_attributes = db
        .struct_attributes(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let members = db
        .struct_members(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .iter()
        .map(|(name, member)| (*name, member.ty, member.visibility))
        .collect();

    let generic_params = db
        .struct_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: Some(members),
        return_type: None,
        attributes: Some(struct_attributes.to_vec()),
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for member signature formatting.
pub(crate) fn get_member_signature_data<'db>(
    db: &'db dyn Database,
    item_id: MemberId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let name = item_id.name(db);
    let struct_id = item_id.struct_id(db);

    if let Some(member) = db
        .struct_members(struct_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .get(&name)
    {
        Ok(DocumentableItemSignatureData {
            item_id: Member(item_id),
            name,
            visibility: member.visibility,
            generic_args: None,
            generic_params: None,
            variants: None,
            members: None,
            return_type: Some(member.ty),
            attributes: None,
            params: None,
            resolver_generic_params: None,
            return_value_expr: None,
            full_path: item_id.full_path(db),
        })
    } else {
        Err(SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))
    }
}

/// Retrieves data for free function signature formatting.
pub(crate) fn get_free_function_signature_data<'db>(
    db: &'db dyn Database,
    item_id: FreeFunctionId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::FreeFunction(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let generic_params = db
        .free_function_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let signature = db
        .free_function_signature(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let resolver_data = db
        .free_function_declaration_resolver_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params.clone()),
        resolver_generic_params: Some(resolver_data.generic_params.clone()),
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for trait function signature formatting.
pub(crate) fn get_trait_function_signature_data<'db>(
    db: &'db dyn Database,
    item_id: TraitFunctionId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let signature = db
        .trait_function_signature(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let generic_params = db
        .trait_function_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let resolver_data = db
        .trait_function_resolver_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    // trait function resolver data contains both: its own and the traits generic params,
    // to get a signature relevant subset, the trait ones need to be filtered out
    let trait_id = item_id.trait_id(db);
    let trait_resolver_data = trait_id
        .resolver_data(db)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;
    let trait_params_set: HashSet<_> = trait_resolver_data.generic_params.iter().collect();

    let function_generic_params: Vec<GenericParamId<'_>> = resolver_data
        .generic_params
        .iter()
        .filter(|param| !trait_params_set.contains(param))
        .cloned()
        .collect_vec();

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(Function(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params.clone()),
        resolver_generic_params: Some(function_generic_params),
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for impl function signature formatting.
pub(crate) fn get_impl_function_signature_data<'db>(
    db: &'db dyn Database,
    item_id: ImplFunctionId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let signature = db
        .impl_function_signature(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let generic_params = db
        .impl_function_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params.clone()),
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for constant signature formatting.
pub(crate) fn get_constant_signature_data<'db>(
    db: &'db dyn Database,
    item_id: ConstantId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::Constant(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let constant = db
        .constant_semantic_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Constant(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(constant.ty()),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: Some(constant.arenas.exprs[constant.value].clone()),
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for impl constant signature formatting.
pub(crate) fn get_impl_constant_signature_data<'db>(
    db: &'db dyn Database,
    item_id: ImplConstantDefId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let return_type = db
        .impl_constant_def_value(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .ty(db)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Constant(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(return_type),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for trait signature formatting.
pub(crate) fn get_trait_signature_data<'db>(
    db: &'db dyn Database,
    item_id: TraitId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::Trait(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let generic_params = db
        .trait_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for trait const signature formatting.
pub(crate) fn get_trait_const_signature_data<'db>(
    db: &'db dyn Database,
    item_id: TraitConstantId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let attributes = db
        .trait_constant_attributes(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let return_type = db
        .trait_constant_type(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Constant(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(return_type),
        attributes: Some(attributes),
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for implementation signature formatting.
pub(crate) fn get_impl_def_signature_data<'db>(
    db: &'db dyn Database,
    item_id: ImplDefId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::Impl(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let resolver_data = db
        .impl_def_resolver_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let intern = db
        .impl_def_concrete_trait(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .long(db);

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: Some(intern.generic_args.clone()),
        generic_params: None,
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: Some(resolver_data.generic_params.clone()),
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for alias signature formatting.
pub(crate) fn get_impl_alias_signature_data<'db>(
    db: &'db dyn Database,
    item_id: ImplAliasId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::ImplAlias(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ImplAlias(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for type alias signature formatting.
pub(crate) fn get_module_type_alias_full_signature<'db>(
    db: &'db dyn Database,
    item_id: ModuleTypeAliasId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::TypeAlias(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let generic_params = db
        .module_type_alias_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let resolved_type = db
        .module_type_alias_resolved_type(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::TypeAlias(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(resolved_type),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for trait type signature formatting.
pub(crate) fn get_trait_type_full_signature<'db>(
    db: &'db dyn Database,
    item_id: TraitTypeId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let generic_params = db
        .trait_type_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for type signature formatting.
pub(crate) fn get_impl_type_def_full_signature<'db>(
    db: &'db dyn Database,
    item_id: ImplTypeDefId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let resolved_type = db
        .impl_type_def_resolved_type(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let generic_params = db
        .impl_type_def_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Type(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(resolved_type),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for extern type signature formatting.
pub(crate) fn get_extern_type_full_signature<'db>(
    db: &'db dyn Database,
    item_id: ExternTypeId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::ExternType(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let generic_params = db
        .extern_type_declaration_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternType(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for extern function signature formatting.
pub(crate) fn get_extern_function_full_signature<'db>(
    db: &'db dyn Database,
    item_id: ExternFunctionId<'db>,
) -> Result<DocumentableItemSignatureData<'db>, SignatureError> {
    let module_item_id = ModuleItemId::ExternFunction(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let generic_params = db
        .extern_function_declaration_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let signature = db
        .extern_function_signature(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternFunction(
            item_id,
        ))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params.to_vec()),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params.clone()),
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}
