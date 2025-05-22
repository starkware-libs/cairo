use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId,
    LanguageElementId, LookupItemId, MemberId, ModuleItemId, ModuleTypeAliasId,
    NamedLanguageElementId, StructId, TopLevelLanguageElementId, TraitConstantId, TraitFunctionId,
    TraitId, TraitItemId, TraitTypeId,
};
use cairo_lang_semantic::items::module::ModuleItemInfo;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::{Expr, GenericArgumentId, GenericParam, Parameter, TypeId};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_utils::LookupIntern;
use smol_str::SmolStr;

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::documentable_item::DocumentableItemId::Member;
use crate::signature_errors::SignatureError;

/// A helper struct gathering documentable item's signature data.
pub(crate) struct DocumentableItemSignatureData {
    pub(crate) item_id: DocumentableItemId,
    pub(crate) name: SmolStr,
    pub(crate) visibility: Visibility,
    pub(crate) generic_args: Option<Vec<GenericArgumentId>>,
    pub(crate) generic_params: Option<Vec<GenericParam>>,
    pub(crate) variants: Option<Vec<(SmolStr, TypeId)>>,
    pub(crate) members: Option<Vec<(SmolStr, TypeId, Visibility)>>,
    pub(crate) return_type: Option<TypeId>,
    pub(crate) attributes: Option<Vec<Attribute>>,
    pub(crate) params: Option<Vec<Parameter>>,
    pub(crate) resolver_generic_params: Option<Vec<GenericParamId>>,
    pub(crate) return_value_expr: Option<Expr>,
    pub(crate) full_path: String,
}

/// A utility function, retrieves [`ModuleItemInfo`] for [`ModuleItemId`].
fn get_module_item_info(
    db: &dyn DocGroup,
    module_item_id: ModuleItemId,
) -> Result<ModuleItemInfo, SignatureError> {
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
pub(crate) fn get_enum_signature_data(
    db: &dyn DocGroup,
    item_id: EnumId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
    let module_item_id = ModuleItemId::Enum(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let enum_variants = db
        .enum_variants(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db)))?;

    let mut variants: Vec<(SmolStr, TypeId)> = Vec::new();
    for (name, variant_id) in enum_variants {
        let variant_semantic = db.variant_semantic(item_id, variant_id).map_err(|_| {
            SignatureError::FailedRetrievingSemanticData(module_item_id.full_path(db))
        })?;
        variants.push((name, variant_semantic.ty));
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
pub(crate) fn get_struct_signature_data(
    db: &dyn DocGroup,
    item_id: StructId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
    let module_item_id = ModuleItemId::Struct(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let struct_attributes = db
        .struct_attributes(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let members = db
        .struct_members(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .iter()
        .map(|(name, member)| (name.clone(), member.ty, member.visibility))
        .collect();

    let generic_params = db
        .struct_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(generic_params),
        variants: None,
        members: Some(members),
        return_type: None,
        attributes: Some(struct_attributes),
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for member signature formatting.
pub(crate) fn get_member_signature_data(
    db: &dyn DocGroup,
    item_id: MemberId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_free_function_signature_data(
    db: &dyn DocGroup,
    item_id: FreeFunctionId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params),
        resolver_generic_params: Some(resolver_data.generic_params.clone()),
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for trait function signature formatting.
pub(crate) fn get_trait_function_signature_data(
    db: &dyn DocGroup,
    item_id: TraitFunctionId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
    let signature = db
        .trait_function_signature(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let generic_params = db
        .trait_function_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let resolver_data = db
        .trait_function_resolver_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(Function(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params),
        resolver_generic_params: Some(resolver_data.generic_params.clone()),
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for impl function signature formatting.
pub(crate) fn get_impl_function_signature_data(
    db: &dyn DocGroup,
    item_id: ImplFunctionId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params),
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}

/// Retrieves data for constant signature formatting.
pub(crate) fn get_constant_signature_data(
    db: &dyn DocGroup,
    item_id: ConstantId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_impl_constant_signature_data(
    db: &dyn DocGroup,
    item_id: ImplConstantDefId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_trait_signature_data(
    db: &dyn DocGroup,
    item_id: TraitId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
        generic_params: Some(generic_params),
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
pub(crate) fn get_trait_const_signature_data(
    db: &dyn DocGroup,
    item_id: TraitConstantId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_impl_def_signature_data(
    db: &dyn DocGroup,
    item_id: ImplDefId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
    let module_item_id = ModuleItemId::Impl(item_id);
    let module_item_info = get_module_item_info(db, module_item_id)?;

    let resolver_data = db
        .impl_def_resolver_data(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    let intern = db
        .impl_def_concrete_trait(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?
        .lookup_intern(db);

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(item_id))),
        name: item_id.name(db),
        visibility: module_item_info.visibility,
        generic_args: Some(intern.generic_args),
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
pub(crate) fn get_impl_alias_signature_data(
    db: &dyn DocGroup,
    item_id: ImplAliasId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_module_type_alias_full_signature(
    db: &dyn DocGroup,
    item_id: ModuleTypeAliasId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_trait_type_full_signature(
    db: &dyn DocGroup,
    item_id: TraitTypeId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
    let generic_params = db
        .trait_type_generic_params(item_id)
        .map_err(|_| SignatureError::FailedRetrievingSemanticData(item_id.full_path(db)))?;

    Ok(DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(item_id))),
        name: item_id.name(db),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: Some(generic_params),
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
pub(crate) fn get_impl_type_def_full_signature(
    db: &dyn DocGroup,
    item_id: ImplTypeDefId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
pub(crate) fn get_extern_type_full_signature(
    db: &dyn DocGroup,
    item_id: ExternTypeId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
        generic_params: Some(generic_params),
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
pub(crate) fn get_extern_function_full_signature(
    db: &dyn DocGroup,
    item_id: ExternFunctionId,
) -> Result<DocumentableItemSignatureData, SignatureError> {
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
        generic_params: Some(generic_params),
        variants: None,
        members: None,
        return_type: Some(signature.return_type),
        attributes: None,
        params: Some(signature.params),
        resolver_generic_params: None,
        return_value_expr: None,
        full_path: item_id.full_path(db),
    })
}
