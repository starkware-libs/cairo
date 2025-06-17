use std::fmt;
use std::fmt::Write;
use std::option::Option;

use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericImplItemId,
    GenericItemId, GenericKind, GenericModuleItemId, GenericParamId, GenericTraitItemId,
    ImplAliasId, ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId,
    LanguageElementId, LookupItemId, MemberId, ModuleId, ModuleItemId, ModuleTypeAliasId,
    NamedLanguageElementId, StructId, TopLevelLanguageElementId, TraitConstantId, TraitFunctionId,
    TraitId, TraitItemId, TraitTypeId, VariantId,
};
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::generics::GenericArgumentId;
use cairo_lang_semantic::items::modifiers::get_relevant_modifier;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::types::TypeId;
use cairo_lang_semantic::{ConcreteTypeId, Expr, GenericParam, TypeLongId};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, green};
use cairo_lang_utils::LookupIntern;
use itertools::Itertools;

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::location_links::{LocationLink, format_signature};
use crate::signature_data::{
    DocumentableItemSignatureData, get_constant_signature_data, get_enum_signature_data,
    get_extern_function_full_signature, get_extern_type_full_signature,
    get_free_function_signature_data, get_impl_alias_signature_data,
    get_impl_constant_signature_data, get_impl_def_signature_data,
    get_impl_function_signature_data, get_impl_type_def_full_signature, get_member_signature_data,
    get_module_type_alias_full_signature, get_struct_signature_data,
    get_trait_const_signature_data, get_trait_function_signature_data, get_trait_signature_data,
    get_trait_type_full_signature,
};
use crate::signature_errors::SignatureError;

/// Used for indenting children items of complex data type signature e.g. struct members.
const INDENT: &str = "    ";
/// Returned when item's signature could not be determined.
const MISSING: &str = "<missing>";

/// Gets the signature of an item (i.e., item without its body).
pub fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableItemId) -> Option<String> {
    get_item_signature_with_links(db, item_id).0
}

/// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
/// signature slices on documentable items.
pub fn get_item_signature_with_links(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> (Option<String>, Vec<LocationLink>) {
    let mut f = HirFormatter::new(db);
    match item_id {
        DocumentableItemId::LookupItem(item_id) => match item_id {
            LookupItemId::ModuleItem(item_id) => match item_id {
                ModuleItemId::Struct(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::Enum(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::FreeFunction(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::TypeAlias(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::ImplAlias(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::Trait(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::Impl(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::ExternType(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::ExternFunction(item_id) => item_id.get_signature_with_links(&mut f),
                ModuleItemId::Submodule(_) => (None, vec![]),
                ModuleItemId::Use(_) => (None, vec![]),
                ModuleItemId::MacroDeclaration(_) => (None, vec![]),
            },
            LookupItemId::TraitItem(item_id) => match item_id {
                TraitItemId::Function(item_id) => item_id.get_signature_with_links(&mut f),
                TraitItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
                TraitItemId::Type(item_id) => item_id.get_signature_with_links(&mut f),
                TraitItemId::Impl(_) => (None, vec![]),
            },
            LookupItemId::ImplItem(item_id) => match item_id {
                ImplItemId::Function(item_id) => item_id.get_signature_with_links(&mut f),
                ImplItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
                ImplItemId::Type(item_id) => item_id.get_signature_with_links(&mut f),
                ImplItemId::Impl(_) => (None, vec![]),
            },
        },
        DocumentableItemId::Member(item_id) => item_id.get_signature_with_links(&mut f),
        DocumentableItemId::Variant(item_id) => item_id.get_signature_with_links(&mut f),
        DocumentableItemId::Crate(_) => (None, vec![]),
    }
}

pub trait HirDisplay {
    /// Formats signature.
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError>;

    /// Gets the signature of an item (i.e., item without its body).
    fn get_signature(&self, f: &mut HirFormatter<'_>) -> Option<String> {
        match self.hir_fmt(f) {
            Ok(_) => Some(f.buf.clone()),
            Err(_) => None,
        }
    }

    /// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
    /// signature slices on documentable items.
    fn get_signature_with_links(
        &self,
        f: &mut HirFormatter<'_>,
    ) -> (Option<String>, Vec<LocationLink>) {
        let signature = self.get_signature(f);
        (signature, f.location_links.clone())
    }
}

/// Documentable items signature formatter.
pub struct HirFormatter<'a> {
    /// The database handle.
    db: &'a dyn DocGroup,
    /// A buffer to intercept writes with.
    buf: String,
    /// Linkable signature items.
    location_links: Vec<LocationLink>,
}

impl fmt::Write for HirFormatter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}

/// [`HirFormatter`] implementation.
impl<'a> HirFormatter<'a> {
    /// Creates new instance of [`HirFormatter`].
    pub fn new(db: &'a dyn DocGroup) -> Self {
        Self { db, buf: String::new(), location_links: Vec::new() }
    }

    /// Adds a [`LocationLink`] to [`HirFormatter`] instance.
    fn add_location_link(&mut self, start: usize, end: usize, item_id: DocumentableItemId) {
        self.location_links.push(LocationLink { start, end, item_id })
    }

    /// Adds type's [`LocationLink`] to [`HirFormatter`] instance, formats and writes relevant
    /// signature slice.
    fn write_type(
        &mut self,
        prefix: Option<&str>,
        element_type: TypeId,
        postfix: Option<&str>,
        full_path: &String,
    ) -> fmt::Result {
        self.write_str(prefix.unwrap_or_default())?;
        let formatted_element_type = element_type.format(self.db);

        if let TypeLongId::Tuple(vec_types) = element_type.lookup_intern(self.db) {
            self.write_str("(")?;
            let mut count = vec_types.len();
            for t in vec_types {
                self.write_type(None, t, if count == 1 { None } else { Some(", ") }, full_path)?;
                count -= 1;
            }
            self.write_str(")")?;
        } else if is_the_same_root(full_path, &formatted_element_type) {
            let documentable_id = resolve_type(self.db, element_type);
            match documentable_id {
                Some(documentable_id) => {
                    let start_offset = self.buf.len();
                    self.write_str(&extract_and_format(&formatted_element_type))?;
                    let end_offset = self.buf.len();
                    self.add_location_link(start_offset, end_offset, documentable_id);
                }
                None => {
                    self.write_str(&extract_and_format(&formatted_element_type))?;
                }
            }
        } else {
            self.write_str(&extract_and_format(&formatted_element_type))?;
        }
        self.write_str(postfix.unwrap_or_default())
    }

    /// Adds [`LocationLink`] to [`HirFormatter`] instance, writes `name` argument into signature
    /// buf.
    fn write_link(
        &mut self,
        name: String,
        documentable_id: Option<DocumentableItemId>,
    ) -> fmt::Result {
        match documentable_id {
            Some(documentable_id) => {
                let start_offset = self.buf.len();
                self.write_str(&name)?;
                let end_offset = self.buf.len();
                self.add_location_link(start_offset, end_offset, documentable_id);
                Ok(())
            }
            None => self.write_str(&extract_and_format(&name)),
        }
    }

    /// Applies extra formatting to item signature.
    /// Avoid using for types whose signatures are invalid cairo code
    /// (such as struct members or enum variants).
    fn format(&mut self) {
        let (formatted_signature, moved_location_links) = format_signature(
            std::mem::take(&mut self.buf),
            std::mem::take(&mut self.location_links),
        );
        self.buf = formatted_signature;
        self.location_links = moved_location_links;
    }
}

impl HirDisplay for VariantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let name = self.name(f.db);
        let variant_semantic =
            f.db.variant_semantic(self.enum_id(f.db), *self)
                .map_err(|_| SignatureError::FailedRetrievingSemanticData(self.full_path(f.db)))?;
        if !variant_semantic.ty.is_unit(f.db) {
            f.write_type(
                Some(&format!("{name}: ")),
                variant_semantic.ty,
                None,
                &self.full_path(f.db),
            )
        } else {
            f.write_str(name.as_str())
        }
        .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))
    }
}

impl HirDisplay for EnumId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let enum_full_signature = get_enum_signature_data(f.db, *self)?;
        write!(
            f,
            "{}enum {} {{",
            get_syntactic_visibility(&enum_full_signature.visibility),
            enum_full_signature.name,
        )
        .map_err(|_| {
            SignatureError::FailedWritingSignature(enum_full_signature.full_path.clone())
        })?;
        let variants = enum_full_signature.variants;
        if let Some(variants) = variants {
            let is_variants_empty = variants.is_empty();
            for (name, variant_type) in variants {
                if !variant_type.is_unit(f.db) {
                    f.write_type(
                        Some(&format!("\n{INDENT}{name}: ",)),
                        variant_type,
                        Some(","),
                        &enum_full_signature.full_path,
                    )
                } else {
                    write!(f, "\n{INDENT}{name},")
                }
                .map_err(|_| {
                    SignatureError::FailedWritingSignature(enum_full_signature.full_path.clone())
                })?;
            }
            f.write_str(if is_variants_empty { "}" } else { "\n}" })
        } else {
            f.write_str("}")
        }
        .map_err(|_| {
            SignatureError::FailedWritingSignature(enum_full_signature.full_path.clone())
        })?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for MemberId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let member_full_signature = get_member_signature_data(f.db, *self)?;
        if let Some(return_type) = member_full_signature.return_type {
            if return_type.is_unit(f.db) {
                write!(
                    f,
                    "{}{}",
                    get_syntactic_visibility(&member_full_signature.visibility),
                    member_full_signature.name
                )
            } else {
                f.write_type(
                    Some(&format!(
                        "{}{}: ",
                        get_syntactic_visibility(&member_full_signature.visibility),
                        member_full_signature.name,
                    )),
                    return_type,
                    None,
                    &member_full_signature.full_path,
                )
            }
            .map_err(|_| SignatureError::FailedWritingType(member_full_signature.full_path.clone()))
        } else {
            Err(SignatureError::FailedRetrievingSemanticData(self.full_path(f.db)))
        }
    }
}

impl HirDisplay for StructId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let struct_full_signature = get_struct_signature_data(f.db, *self)?;
        if let Some(attributes) = struct_full_signature.attributes {
            write_struct_attributes_syntax(attributes, f).map_err(|_| {
                SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
            })?;
        }
        write!(
            f,
            "{}struct {}",
            get_syntactic_visibility(&struct_full_signature.visibility),
            struct_full_signature.name,
        )
        .map_err(|_| {
            SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
        })?;
        if let Some(generic_params) = struct_full_signature.generic_params {
            write_generic_params(generic_params, f).map_err(|_| {
                SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
            })?;
        }
        f.write_str(" {").map_err(|_| {
            SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
        })?;

        if let Some(members) = struct_full_signature.members {
            let is_members_empty = members.is_empty();
            for member in members {
                let (name, member_type, visibility) = member;
                f.write_type(
                    Some(
                        &format!("\n{INDENT}{}{}: ", get_syntactic_visibility(&visibility), name,),
                    ),
                    member_type,
                    Some(","),
                    &struct_full_signature.full_path,
                )
                .map_err(|_| {
                    SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
                })?;
            }
            f.write_str(if is_members_empty { "}" } else { "\n}" }).map_err(|_| {
                SignatureError::FailedWritingSignature(struct_full_signature.full_path.clone())
            })?;
        };
        f.format();
        Ok(())
    }
}

impl HirDisplay for FreeFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let free_function_full_signature = get_free_function_signature_data(f.db, *self)?;
        write_function_signature(f, free_function_full_signature, "".to_string())
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ConstantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let constant_full_signature = get_constant_signature_data(f.db, *self)?;
        write!(
            f,
            "{}const {}: ",
            get_syntactic_visibility(&constant_full_signature.visibility),
            constant_full_signature.name,
        )
        .map_err(|_| {
            SignatureError::FailedWritingSignature(constant_full_signature.full_path.clone())
        })?;
        if let Some(return_type) = constant_full_signature.return_type {
            f.write_type(None, return_type, Some(" = "), &constant_full_signature.full_path)
                .map_err(|_| {
                    SignatureError::FailedWritingSignature(
                        constant_full_signature.full_path.clone(),
                    )
                })?;
        }
        if let Some(return_value_expr) = constant_full_signature.return_value_expr {
            match return_value_expr {
                Expr::Literal(v) => write!(f, "{};", v.value,).map_err(|_| {
                    SignatureError::FailedWritingSignature(
                        constant_full_signature.full_path.clone(),
                    )
                }),
                Expr::FunctionCall(_) => {
                    let const_value_id = f.db.constant_const_value(*self).map_err(|_| {
                        SignatureError::FailedRetrievingSemanticData(
                            constant_full_signature.full_path.clone(),
                        )
                    })?;
                    let constant_value = f.db.lookup_intern_const_value(const_value_id);
                    if let ConstValue::Int(value, _) = constant_value {
                        write_syntactic_evaluation(f, constant_full_signature.item_id).map_err(
                            |_| {
                                SignatureError::FailedWritingSignature(
                                    constant_full_signature.full_path.clone(),
                                )
                            },
                        )?;
                        write!(f, " // = {value}")
                    } else {
                        write_syntactic_evaluation(f, constant_full_signature.item_id)
                    }
                    .map_err(|_| {
                        SignatureError::FailedWritingSignature(
                            constant_full_signature.full_path.clone(),
                        )
                    })
                }
                _ => write_syntactic_evaluation(f, constant_full_signature.item_id).map_err(|_| {
                    SignatureError::FailedWritingSignature(
                        constant_full_signature.full_path.clone(),
                    )
                }),
            }
        } else {
            Err(SignatureError::FailedRetrievingSemanticData(self.full_path(f.db)))
        }
    }
}

impl HirDisplay for ImplConstantDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let constant_full_signature = get_impl_constant_signature_data(f.db, *self)?;
        if let Some(return_type) = constant_full_signature.return_type {
            f.write_type(
                Some(&format!("const {}: ", constant_full_signature.name,)),
                return_type,
                Some(" = "),
                &constant_full_signature.full_path,
            )
            .map_err(|_| {
                SignatureError::FailedWritingSignature(constant_full_signature.full_path.clone())
            })?;
        }
        write_syntactic_evaluation(f, constant_full_signature.item_id).map_err(|_| {
            SignatureError::FailedWritingSignature(constant_full_signature.full_path)
        })?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for TraitFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let free_function_full_signature = get_trait_function_signature_data(f.db, *self)?;
        write_function_signature(f, free_function_full_signature, "".to_string())
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ImplFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let impl_function_full_signature = get_impl_function_signature_data(f.db, *self)?;
        write_function_signature(f, impl_function_full_signature, "".to_string())
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for TraitId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let trait_full_signature = get_trait_signature_data(f.db, *self)?;
        write!(
            f,
            "{}trait {}",
            get_syntactic_visibility(&trait_full_signature.visibility),
            trait_full_signature.name,
        )
        .map_err(|_| {
            SignatureError::FailedWritingSignature(trait_full_signature.full_path.clone())
        })?;
        if let Some(generic_params) = trait_full_signature.generic_params {
            write_generic_params(generic_params, f).map_err(|_| {
                SignatureError::FailedWritingSignature(trait_full_signature.full_path)
            })?
        };
        f.format();
        Ok(())
    }
}

impl HirDisplay for TraitConstantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let trait_const_full_signature = get_trait_const_signature_data(f.db, *self)?;
        if let Some(return_type) = trait_const_full_signature.return_type {
            write!(
                f,
                "const {}: {};",
                trait_const_full_signature.name,
                extract_and_format(&return_type.format(f.db)),
            )
            .map_err(|_| {
                SignatureError::FailedWritingSignature(trait_const_full_signature.full_path)
            })?;
        } else {
            Err(SignatureError::FailedRetrievingSemanticData(self.full_path(f.db)))?;
        }
        f.format();
        Ok(())
    }
}

impl HirDisplay for ImplDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let impl_def_full_signature = get_impl_def_signature_data(f.db, *self)?;
        let trait_id = f.db.impl_def_trait(*self).map_err(|_| {
            SignatureError::FailedRetrievingSemanticData(impl_def_full_signature.full_path.clone())
        })?;
        if let Some(resolver_generic_params) = impl_def_full_signature.resolver_generic_params {
            let resolver_generic_params =
                format_resolver_generic_params(f.db, resolver_generic_params);
            write!(
                f,
                "{}impl {}{} of {}",
                get_syntactic_visibility(&impl_def_full_signature.visibility),
                impl_def_full_signature.name,
                resolver_generic_params,
                trait_id.name(f.db),
            )
            .map_err(|_| {
                SignatureError::FailedWritingSignature(impl_def_full_signature.full_path.clone())
            })?;
        }
        if let Some(generic_args) = impl_def_full_signature.generic_args {
            write_generic_args(generic_args, f).map_err(|_| {
                SignatureError::FailedWritingSignature(impl_def_full_signature.full_path.clone())
            })?;
        }
        f.write_str(";").map_err(|_| {
            SignatureError::FailedWritingSignature(impl_def_full_signature.full_path)
        })?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ImplAliasId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let impl_alias_full_signature = get_impl_alias_signature_data(f.db, *self)?;
        write!(
            f,
            "{}impl {} = ",
            get_syntactic_visibility(&impl_alias_full_signature.visibility),
            self.name(f.db),
        )
        .map_err(|_| {
            SignatureError::FailedWritingSignature(impl_alias_full_signature.full_path.clone())
        })?;
        write_syntactic_evaluation(f, impl_alias_full_signature.item_id).map_err(|_| {
            SignatureError::FailedWritingSignature(impl_alias_full_signature.full_path)
        })?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ModuleTypeAliasId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let module_type_alias_full_signature = get_module_type_alias_full_signature(f.db, *self)?;
        write_type_signature(f, module_type_alias_full_signature, false)
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for TraitTypeId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let trait_type_full_signature = get_trait_type_full_signature(f.db, *self)?;
        write_type_signature(f, trait_type_full_signature, false)
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ImplTypeDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let impl_type_def_full_signature = get_impl_type_def_full_signature(f.db, *self)?;
        write_type_signature(f, impl_type_def_full_signature, false)
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ExternTypeId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let extern_type_full_signature = get_extern_type_full_signature(f.db, *self)?;
        write_type_signature(f, extern_type_full_signature, true)
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        f.format();
        Ok(())
    }
}

impl HirDisplay for ExternFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), SignatureError> {
        let extern_function_full_signature = get_extern_function_full_signature(f.db, *self)?;
        let signature = match f.db.extern_function_signature(*self) {
            Ok(signature) => signature,
            _ => {
                return Err(SignatureError::FailedRetrievingSemanticData(
                    extern_function_full_signature.full_path,
                ));
            }
        };
        write_function_signature(f, extern_function_full_signature, "extern ".to_string())
            .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        if !signature.implicits.is_empty() {
            f.write_str(" implicits(")
                .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
            let mut count = signature.implicits.len();
            for type_id in &signature.implicits {
                write!(
                    f,
                    "{}{}",
                    extract_and_format(&type_id.format(f.db)),
                    if count == 1 { ")".to_string() } else { ", ".to_string() }
                )
                .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
                count -= 1;
            }
        }
        if !signature.panicable {
            f.write_str(" nopanic")
                .map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))?;
        };
        f.write_str(";").map_err(|_| SignatureError::FailedWritingSignature(self.full_path(f.db)))
    }
}

/// Formats the text of the [`Visibility`] to a relevant string slice.
pub fn get_syntactic_visibility(semantic_visibility: &Visibility) -> &str {
    match semantic_visibility {
        Visibility::Public => "pub ",
        Visibility::PublicInCrate => "pub(crate) ",
        Visibility::Private => "",
    }
}

/// Checks if given paths come from the same root.
fn is_the_same_root(path1: &str, path2: &str) -> bool {
    fn extract_root(input: &str) -> &str {
        if let Some(index) = input.find("::") { &input[..index] } else { input }
    }
    extract_root(path1) == extract_root(path2)
}

/// Formats complex types full paths. For example "Result<Error::NotFound, System::Error>" input
/// results in "Result<NotFound, Error>" output.
fn extract_and_format(input: &str) -> String {
    let delimiters = [',', '<', '>', '(', ')', '[', ']'];
    let mut output = String::new();
    let mut slice_start = 0;
    let mut in_slice = false;

    for (i, c) in input.char_indices() {
        if delimiters.contains(&c) {
            if in_slice {
                let slice = &input[slice_start..i];
                output.push_str(&format_final_part(slice));
                in_slice = false;
            }
            output.push(c);
            slice_start = i + 1;
        } else {
            in_slice = true;
        }
    }
    if in_slice {
        let slice = &input[slice_start..];
        output.push_str(&format_final_part(slice));
    }
    output
}

/// Formats single type path. For example "core::felt252" input results in "felt252" output.
fn format_final_part(slice: &str) -> String {
    let parts: Vec<&str> = slice.split("::").collect();
    let ensure_whitespace =
        if let Some(first) = parts.first() { first.starts_with(" ") } else { false };
    let result = {
        match parts[..] {
            [.., before_last, ""] => before_last.to_string(),
            [.., last] => last.to_string(),
            _ => slice.to_string(),
        }
    };
    if ensure_whitespace && !result.starts_with(' ') { format!(" {result}") } else { result }
}

/// Takes a list of [`GenericParamId`]s and formats it into a String representation used for
/// signature documentation.
fn format_resolver_generic_params(db: &dyn DocGroup, params: Vec<GenericParamId>) -> String {
    if !params.is_empty() {
        format!(
            "<{}>",
            params
                .iter()
                .map(|param| {
                    if matches!(param.kind(db), GenericKind::Impl) {
                        let param_formatted = param.format(db);
                        if param_formatted.starts_with("+") {
                            param_formatted
                        } else {
                            match db.generic_param_semantic(*param) {
                                Ok(generic_param) => match generic_param {
                                    GenericParam::Impl(generic_param_impl) => {
                                        match generic_param_impl.concrete_trait {
                                            Ok(concrete_trait) => {
                                                format!(
                                                    "impl {param_formatted}: {}<{}>",
                                                    concrete_trait.name(db),
                                                    concrete_trait
                                                        .generic_args(db)
                                                        .iter()
                                                        .map(|arg| arg.format(db))
                                                        .collect::<Vec<_>>()
                                                        .join(", "),
                                                )
                                            }
                                            Err(_) => param_formatted,
                                        }
                                    }
                                    _ => param_formatted,
                                },
                                Err(_) => param_formatted,
                            }
                        }
                    } else {
                        param.format(db)
                    }
                })
                .join(", ")
        )
    } else {
        "".to_string()
    }
}

/// A utility function used for formatting documentable functions data. Use with
/// [`DocumentableItemSignatureData`] argument created for [`FreeFunctionId`], [`TraitFunctionId`],
/// [`ImplFunctionId`] or [`ExternFunctionId`]. As those are the items for which a
/// [`cairo_lang_semantic::items::functions::Signature`] can be retrieved.
fn write_function_signature(
    f: &mut HirFormatter<'_>,
    documentable_signature: DocumentableItemSignatureData,
    syntactic_kind: String,
) -> Result<(), fmt::Error> {
    let resolver_generic_params = match documentable_signature.resolver_generic_params {
        Some(params) => format_resolver_generic_params(f.db, params),
        None => "".to_string(),
    };

    write!(
        f,
        "{}{}fn {}{}",
        get_syntactic_visibility(&documentable_signature.visibility),
        syntactic_kind,
        documentable_signature.name,
        resolver_generic_params,
    )?;
    if let Some(generic_args) = documentable_signature.generic_args {
        write_generic_args(generic_args, f)?;
    }
    f.write_str("(")?;
    if let Some(params) = documentable_signature.params {
        let mut count = params.len();
        let mut postfix = String::from(", ");
        for param in params {
            if count == 1 {
                postfix = "".to_string();
            }
            let syntax_node = param.id.stable_location(f.db).syntax_node(f.db);
            let modifier = get_relevant_modifier(&param.mutability);
            let modifier_postfix = if modifier.is_empty() { "" } else { " " };
            if param.ty.is_fully_concrete(f.db) {
                f.write_type(
                    Some(&format!("{modifier}{modifier_postfix}{}: ", param.name)),
                    param.ty,
                    Some(&postfix),
                    &documentable_signature.full_path,
                )?;
            } else {
                let type_definition = get_type_clause(syntax_node, f.db).unwrap_or_default();
                write!(f, "{modifier}{modifier_postfix}{}{type_definition}{postfix}", param.name,)?;
            }
            count -= 1;
        }
    }
    f.write_str(")")?;

    if let Some(return_type) = documentable_signature.return_type {
        if !return_type.is_unit(f.db) {
            f.write_type(Some(" -> "), return_type, None, &documentable_signature.full_path)?;
        }
    }
    Ok(())
}

/// Retrieves [`SyntaxKind::TypeClause`] text from [`SyntaxNode`].
fn get_type_clause(syntax_node: SyntaxNode, db: &dyn DocGroup) -> Option<String> {
    for child in syntax_node.get_children(db).iter() {
        if child.kind(db) == SyntaxKind::TypeClause {
            return Some(child.get_text_without_all_comment_trivia(db));
        }
    }
    Some(String::from(MISSING))
}

/// Formats and writes [`GenericParam`]s data into [`HirFormatter`]'s buff.
fn write_generic_params(
    generic_params: Vec<GenericParam>,
    f: &mut HirFormatter<'_>,
) -> Result<(), fmt::Error> {
    if !generic_params.is_empty() {
        let mut count = generic_params.len();
        f.write_str("<")?;
        for param in generic_params {
            match param {
                GenericParam::Type(param_type) => {
                    let name = extract_and_format(&param_type.id.format(f.db));
                    write!(f, "{}{}", name, if count == 1 { "" } else { ", " })?;
                }
                GenericParam::Const(param_const) => {
                    let name = extract_and_format(&param_const.id.format(f.db));
                    write!(f, "const {}{}", name, if count == 1 { "" } else { ", " })?;
                }
                GenericParam::Impl(param_impl) => {
                    let name = extract_and_format(&param_impl.id.format(f.db));
                    match param_impl.concrete_trait {
                        Ok(concrete_trait) => {
                            let documentable_id =
                                DocumentableItemId::from(LookupItemId::ModuleItem(
                                    ModuleItemId::Trait(concrete_trait.trait_id(f.db)),
                                ));
                            if name.starts_with("+") {
                                f.write_link(name, Some(documentable_id))?;
                            } else {
                                write!(f, "impl {name}: ")?;
                                let concrete_trait_name = concrete_trait.name(f.db);
                                let concrete_trait_generic_args_formatted = concrete_trait
                                    .generic_args(f.db)
                                    .iter()
                                    .map(|arg| extract_and_format(&arg.format(f.db)))
                                    .collect::<Vec<_>>()
                                    .join(", ");
                                f.write_link(
                                    concrete_trait_name.to_string(),
                                    Some(documentable_id),
                                )?;
                                if !concrete_trait_generic_args_formatted.is_empty() {
                                    write!(f, "<{concrete_trait_generic_args_formatted}>")?;
                                }
                            }
                        }
                        Err(_) => {
                            write!(f, "{}{}", name, if count == 1 { "" } else { ", " })?;
                        }
                    }
                }
                GenericParam::NegImpl(_) => f.write_str(MISSING)?,
            };
            count -= 1;
        }
        f.write_str(">")
    } else {
        Ok(())
    }
}

/// Formats syntax of generic arguments and writes it into [`HirFormatter`].
fn write_generic_args(
    generic_args: Vec<GenericArgumentId>,
    f: &mut HirFormatter<'_>,
) -> Result<(), fmt::Error> {
    let mut count = generic_args.len();
    if !generic_args.is_empty() {
        f.write_str("<")?;
    }
    for arg in &generic_args {
        let documentable_id = resolve_generic_arg(*arg, f.db);
        let _ = f.write_link(extract_and_format(&arg.format(f.db)), documentable_id);
        let _ = f.write_str(if count == 1 { ">" } else { ", " });
        count -= 1;
    }
    Ok(())
}

/// Formats syntax of struct attributes and writes it into [`HirFormatter`].
fn write_struct_attributes_syntax(
    attributes: Vec<Attribute>,
    f: &mut HirFormatter<'_>,
) -> Result<(), fmt::Error> {
    for attribute in attributes {
        let syntax_node = attribute.stable_ptr.lookup(f.db).as_syntax_node();
        for child in syntax_node.get_children(f.db).iter() {
            let to_text = child.get_text_without_all_comment_trivia(f.db);
            let cleaned_text = to_text.replace("\n", "");
            f.write_str(&cleaned_text)?;
        }
        f.write_str("\n")?;
    }
    Ok(())
}

/// Formats syntax of documentable item and writes it into [`HirFormatter`].
fn write_syntactic_evaluation(
    f: &mut HirFormatter<'_>,
    item_id: DocumentableItemId,
) -> Result<(), fmt::Error> {
    if let Some(stable_location) = item_id.stable_location(f.db) {
        let syntax_node = stable_location.syntax_node(f.db);
        if matches!(&syntax_node.green_node(f.db).details, green::GreenNodeDetails::Node { .. }) {
            let mut is_after_evaluation_value = false;
            for child in syntax_node.get_children(f.db).iter() {
                let kind = child.kind(f.db);
                if !matches!(kind, SyntaxKind::Trivia) {
                    if matches!(kind, SyntaxKind::TerminalSemicolon) {
                        f.buf.write_str(";")?;
                        return Ok(());
                    }
                    if is_after_evaluation_value {
                        f.buf.write_str(&SyntaxNode::get_text_without_all_comment_trivia(
                            child, f.db,
                        ))?;
                    };
                    if matches!(kind, SyntaxKind::TerminalEq) {
                        is_after_evaluation_value = true;
                    }
                }
            }
        };
        Ok(())
    } else {
        Err(fmt::Error)
    }
}

/// A utility function used for formatting documentable types data. Use with
/// [`DocumentableItemSignatureData`] argument created for [`ModuleTypeAliasId`], [`TraitTypeId`],
/// [`ImplTypeDefId`] or [`ExternTypeId`]. Because of the same signature structure.
fn write_type_signature(
    f: &mut HirFormatter<'_>,
    documentable_signature: DocumentableItemSignatureData,
    is_extern_type: bool,
) -> Result<(), fmt::Error> {
    write!(
        f,
        "{}{}type {}",
        get_syntactic_visibility(&documentable_signature.visibility),
        if is_extern_type { "extern " } else { "" },
        documentable_signature.name
    )?;
    if let Some(generic_params) = documentable_signature.generic_params {
        write_generic_params(generic_params, f)?;
    }
    if let Some(return_type) = documentable_signature.return_type {
        write!(f, " = ")?;
        f.write_type(None, return_type, None, &documentable_signature.full_path)?;
    };
    write!(f, ";")?;
    Ok(())
}

/// Returns relevant [`DocumentableItemId`] for [`GenericItemId`] if one can be retrieved.
fn resolve_generic_item(
    generic_item_id: GenericItemId,
    db: &dyn DocGroup,
) -> Option<DocumentableItemId> {
    match generic_item_id {
        GenericItemId::ModuleItem(module_item_id) => {
            Some(resolve_generic_module_item(module_item_id))
        }
        GenericItemId::TraitItem(generic_trait_item_id) => match generic_trait_item_id {
            GenericTraitItemId::Type(trait_type_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Trait(trait_type_id.trait_id(db))),
            )),
        },
        GenericItemId::ImplItem(generic_impl_item_id) => match generic_impl_item_id {
            GenericImplItemId::Type(impl_type_def_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Impl(impl_type_def_id.impl_def_id(db))),
            )),
        },
    }
}

/// Returns relevant [`DocumentableItemId`] for [`GenericModuleItemId`].
fn resolve_generic_module_item(generic_module_item_id: GenericModuleItemId) -> DocumentableItemId {
    match generic_module_item_id {
        GenericModuleItemId::FreeFunc(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(id)))
        }
        GenericModuleItemId::ExternFunc(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternFunction(id)))
        }
        GenericModuleItemId::TraitFunc(id) => {
            DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Function(id)))
        }
        GenericModuleItemId::ImplFunc(id) => {
            DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(id)))
        }
        GenericModuleItemId::Trait(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(id)))
        }
        GenericModuleItemId::Impl(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(id)))
        }
        GenericModuleItemId::Struct(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(id)))
        }
        GenericModuleItemId::Enum(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(id)))
        }
        GenericModuleItemId::ExternType(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternType(id)))
        }
        GenericModuleItemId::TypeAlias(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::TypeAlias(id)))
        }
        GenericModuleItemId::ImplAlias(id) => {
            DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ImplAlias(id)))
        }
    }
}

/// Returns relevant [`DocumentableItemId`] for [`GenericArgumentId`] if one can be retrieved.
fn resolve_generic_arg(
    generic_arg_id: GenericArgumentId,
    db: &dyn DocGroup,
) -> Option<DocumentableItemId> {
    match generic_arg_id {
        GenericArgumentId::Type(type_id) => resolve_type(db, type_id),
        GenericArgumentId::Constant(constant_value_id) => match constant_value_id.ty(db) {
            Ok(type_id) => resolve_type(db, type_id),
            Err(_) => None,
        },
        GenericArgumentId::Impl(impl_id) => match impl_id.concrete_trait(db) {
            Ok(concrete_trait) => {
                let trait_id = concrete_trait.trait_id(db);
                Some(DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(
                    trait_id,
                ))))
            }
            Err(_) => None,
        },
        GenericArgumentId::NegImpl => None,
    }
}

/// Returns relevant [`DocumentableItemId`] for [`TypeId`] if one can be retrieved.
fn resolve_type(db: &dyn DocGroup, type_id: TypeId) -> Option<DocumentableItemId> {
    let intern = type_id.lookup_intern(db);
    match intern {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(struct_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id.struct_id(db))),
            )),
            ConcreteTypeId::Enum(enum_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id.enum_id(db))),
            )),
            ConcreteTypeId::Extern(extern_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::ExternType(extern_id.extern_type_id(db))),
            )),
        },
        TypeLongId::Tuple(_) => None,
        TypeLongId::Snapshot(type_id) => resolve_type(db, type_id),
        TypeLongId::GenericParameter(generic_param_id) => {
            let item = generic_param_id.generic_item(db);
            resolve_generic_item(item, db)
        }
        TypeLongId::Var(type_var) => match type_var.inference_id {
            InferenceId::LookupItemDeclaration(lookup_item_id)
            | InferenceId::LookupItemGenerics(lookup_item_id)
            | InferenceId::LookupItemDefinition(lookup_item_id) => {
                Some(DocumentableItemId::from(lookup_item_id))
            }
            InferenceId::ImplDefTrait(impl_def_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Impl(impl_def_id)),
            )),
            InferenceId::ImplAliasImplDef(impl_alias_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::ImplAlias(impl_alias_id)),
            )),
            InferenceId::GenericParam(generic_param_id) => {
                let item = generic_param_id.generic_item(db);
                resolve_generic_item(item, db)
            }
            InferenceId::GenericImplParamTrait(generic_param_id) => {
                let item = generic_param_id.generic_item(db);
                resolve_generic_item(item, db)
            }
            InferenceId::GlobalUseStar(global_use_id) => {
                match db.priv_global_use_imported_module(global_use_id) {
                    Ok(module_id) => match module_id {
                        ModuleId::CrateRoot(crate_id) => Some(DocumentableItemId::from(crate_id)),
                        ModuleId::Submodule(submodule_id) => Some(DocumentableItemId::from(
                            LookupItemId::ModuleItem(ModuleItemId::Submodule(submodule_id)),
                        )),
                    },
                    Err(_) => None,
                }
            }
            InferenceId::Canonical => None,
            InferenceId::NoContext => None,
        },
        TypeLongId::Coupon(function_id) => {
            let concrete_function = function_id.get_concrete(db);
            match concrete_function.generic_function {
                GenericFunctionId::Free(function_id) => Some(DocumentableItemId::from(
                    LookupItemId::ModuleItem(ModuleItemId::FreeFunction(function_id)),
                )),
                GenericFunctionId::Extern(function_id) => Some(DocumentableItemId::from(
                    LookupItemId::ModuleItem(ModuleItemId::ExternFunction(function_id)),
                )),
                GenericFunctionId::Impl(function_id) => Some(DocumentableItemId::from(
                    LookupItemId::TraitItem(Function(function_id.function)),
                )),
            }
        }
        TypeLongId::FixedSizeArray { type_id: _, size: _ } => resolve_type(db, type_id),
        TypeLongId::ImplType(impl_type_id) => match impl_type_id.impl_id().concrete_trait(db) {
            Ok(concrete_trait_id) => Some(DocumentableItemId::from(LookupItemId::ModuleItem(
                ModuleItemId::Trait(concrete_trait_id.trait_id(db)),
            ))),
            Err(_) => None,
        },
        TypeLongId::Closure(closure_type_id) => resolve_type(db, closure_type_id.ret_ty),
        TypeLongId::Missing(_) => None,
    }
}
