use std::fmt;
use std::fmt::Write;
use std::option::Option;

use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, ImplAliasId,
    ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId, LanguageElementId,
    LookupItemId, MacroDeclarationId, MemberId, ModuleItemId, ModuleTypeAliasId,
    NamedLanguageElementId, StructId, TopLevelLanguageElementId, TraitConstantId, TraitFunctionId,
    TraitId, TraitItemId, TraitTypeId, VariantId,
};
use cairo_lang_filesystem::ids::Tracked;
use cairo_lang_semantic::items::constant::{ConstValue, ConstantSemantic};
use cairo_lang_semantic::items::enm::EnumSemantic;
use cairo_lang_semantic::items::extern_function::ExternFunctionSemantic;
use cairo_lang_semantic::items::generics::GenericArgumentId;
use cairo_lang_semantic::items::imp::ImplSemantic;
use cairo_lang_semantic::items::macro_declaration::MacroDeclarationSemantic;
use cairo_lang_semantic::items::modifiers::get_relevant_modifier;
use cairo_lang_semantic::types::TypeId;
use cairo_lang_semantic::{Expr, TypeLongId};
use cairo_lang_syntax::node::ast::WrappedMacro;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use itertools::Itertools;
use salsa::Database;

use crate::documentable_item::DocumentableItemId;
use crate::helpers::{
    extract_and_format, format_resolver_generic_params, get_generic_params,
    get_struct_attributes_syntax, get_syntactic_evaluation, get_syntactic_visibility, resolve_type,
};
use crate::location_links::{LocationLink, format_signature};
use crate::signature_data::{DocumentableItemSignatureData, SignatureDataRetriever};
use crate::signature_errors::SignatureError;

/// Used for indenting children items of complex data type signature, e.g., struct members.
const INDENT: &str = "    ";
/// Returned when item's signature could not be determined.
pub(crate) const MISSING: &str = "<missing>";

/// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
/// signature slices on documentable items.
#[salsa::tracked]
pub fn get_item_signature_with_links<'db>(
    db: &'db dyn Database,
    _tracked: Tracked,
    item_id: DocumentableItemId<'db>,
) -> (Option<String>, Vec<LocationLink<'db>>) {
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
                ModuleItemId::MacroDeclaration(item_id) => item_id.get_signature_with_links(&mut f),
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

pub trait HirDisplay<'db> {
    /// Formats signature.
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError>;

    /// Gets the signature of an item (i.e., the item without its body).
    fn get_signature(&self, f: &mut HirFormatter<'db>) -> Option<String> {
        match self.hir_fmt(f) {
            Ok(_) => Some(f.buf.clone()),
            Err(_) => None,
        }
    }

    /// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
    /// signature slices on documentable items.
    fn get_signature_with_links(
        &self,
        f: &mut HirFormatter<'db>,
    ) -> (Option<String>, Vec<LocationLink<'db>>) {
        let signature = self.get_signature(f);
        (signature, f.location_links.clone())
    }
}

/// Documentable items signature formatter.
pub struct HirFormatter<'db> {
    /// The database handle.
    db: &'db dyn Database,
    /// A buffer to intercept writes with.
    buf: String,
    /// Linkable signature items.
    location_links: Vec<LocationLink<'db>>,
}

impl<'db> fmt::Write for HirFormatter<'db> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        Ok(())
    }
}

/// [`HirFormatter`] implementation.
impl<'db> HirFormatter<'db> {
    /// Creates new instance of [`HirFormatter`].
    pub fn new(db: &'db dyn Database) -> Self {
        Self { db, buf: String::new(), location_links: Vec::new() }
    }

    /// Adds a [`LocationLink`] to [`HirFormatter`] instance.
    fn add_location_link(&mut self, start: usize, end: usize, item_id: DocumentableItemId<'db>) {
        self.location_links.push(LocationLink { start, end, item_id })
    }

    /// Wraps `HirFormatter::write_str` call to satisfy the `SignatureError` return type.
    fn hir_write(&mut self, s: &str) -> Result<(), SignatureError> {
        self.write_str(s)?;
        Ok(())
    }

    /// Adds type's [`LocationLink`] to [`HirFormatter`] instance, formats and writes relevant
    /// signature slice.
    fn write_type(
        &mut self,
        prefix: Option<&str>,
        element_type: TypeId<'db>,
        postfix: Option<&str>,
        full_path: &String,
    ) -> Result<(), SignatureError> {
        self.write_str(prefix.unwrap_or_default())?;
        let formatted_element_type = element_type.format(self.db);

        if let TypeLongId::Tuple(vec_types) = element_type.long(self.db) {
            self.write_str("(")?;
            let mut count = vec_types.len();
            for t in vec_types {
                self.write_type(None, *t, if count == 1 { None } else { Some(", ") }, full_path)?;
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
        self.hir_write(postfix.unwrap_or_default())
    }

    /// Adds [`LocationLink`] to [`HirFormatter`] instance, writes `name` argument into signature
    /// buf.
    fn write_link(
        &mut self,
        name: String,
        documentable_id: Option<DocumentableItemId<'db>>,
    ) -> Result<(), fmt::Error> {
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
    /// Avoid using for types whose signatures are invalid Cairo code
    /// (such as struct members or enum variants).
    fn format(&mut self) {
        let (formatted_signature, moved_location_links) = format_signature(
            self.db,
            std::mem::take(&mut self.buf),
            std::mem::take(&mut self.location_links),
        );
        self.buf = formatted_signature;
        self.location_links = moved_location_links;
    }

    /// Writes a chunk of text with [`LocationLink`]s into [`HirFormatter`] instance.
    fn write_chunk(
        &mut self,
        text: &str,
        location_links: Vec<LocationLink<'db>>,
    ) -> Result<(), SignatureError> {
        let offset = self.buf.len();
        self.buf.push_str(text);
        for location_link in location_links {
            self.add_location_link(
                offset + location_link.start,
                offset + location_link.end,
                location_link.item_id,
            );
        }
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for VariantId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let name = self.name(f.db);
        let variant_semantic = f.db.variant_semantic(self.enum_id(f.db), *self)?;
        if !variant_semantic.ty.is_unit(f.db) {
            f.write_type(
                Some(&format!("{}: ", name.long(f.db))),
                variant_semantic.ty,
                None,
                &self.full_path(f.db),
            )
        } else {
            f.hir_write(name.long(f.db))
        }
    }
}

impl<'db> HirDisplay<'db> for EnumId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let enum_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write!(
            f,
            "{}enum {} {{",
            get_syntactic_visibility(&enum_full_signature.visibility),
            enum_full_signature.name.long(f.db),
        )?;
        let variants = enum_full_signature.variants;
        if let Some(variants) = variants {
            let is_variants_empty = variants.is_empty();
            for (name, variant_type) in variants {
                if !variant_type.is_unit(f.db) {
                    f.write_type(
                        Some(&format!("\n{INDENT}{}: ", name.long(f.db))),
                        variant_type,
                        Some(","),
                        &enum_full_signature.full_path,
                    )
                } else {
                    f.hir_write(&format!("\n{INDENT}{},", name.long(f.db)))
                }?;
            }
            f.hir_write(if is_variants_empty { "}" } else { "\n}" })
        } else {
            f.hir_write("}")
        }?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for MemberId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let member_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        if let Some(return_type) = member_full_signature.return_type {
            if return_type.is_unit(f.db) {
                f.hir_write(&format!(
                    "{}{} = ",
                    get_syntactic_visibility(&member_full_signature.visibility),
                    member_full_signature.name.long(f.db),
                ))
            } else {
                f.write_type(
                    Some(&format!(
                        "{}{}: ",
                        get_syntactic_visibility(&member_full_signature.visibility),
                        member_full_signature.name.long(f.db),
                    )),
                    return_type,
                    None,
                    &member_full_signature.full_path,
                )
            }
        } else {
            Err(SignatureError::FailedWritingSignature(member_full_signature.full_path.clone()))
        }
    }
}

impl<'db> HirDisplay<'db> for StructId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let struct_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        if let Some(attributes) = struct_full_signature.attributes {
            let stx = get_struct_attributes_syntax(attributes, f.db)?;
            f.hir_write(&stx)?;
        }
        write!(
            f,
            "{}struct {}",
            get_syntactic_visibility(&struct_full_signature.visibility),
            struct_full_signature.name.long(f.db),
        )?;
        if let Some(generic_params) = struct_full_signature.generic_params {
            let (stx, lls) = get_generic_params(generic_params, f.db)?;
            f.write_chunk(&stx, lls)?;
        }

        f.hir_write(" {")?;

        if let Some(members) = struct_full_signature.members {
            let is_members_empty = members.is_empty();
            for member in members {
                let (name, member_type, visibility) = member;
                f.write_type(
                    Some(&format!(
                        "\n{INDENT}{}{}: ",
                        get_syntactic_visibility(&visibility),
                        name.long(f.db),
                    )),
                    member_type,
                    Some(","),
                    &struct_full_signature.full_path,
                )?;
            }
            f.hir_write(if is_members_empty { "}" } else { "\n}" })?;
        };
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for FreeFunctionId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let free_function_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_function_signature(f, free_function_full_signature, "".to_string())?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ConstantId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let constant_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        if let Some(return_value_expr) = constant_full_signature.return_value_expr {
            write!(
                f,
                "{}const {}: ",
                get_syntactic_visibility(&constant_full_signature.visibility),
                constant_full_signature.name.long(f.db),
            )?;
            if let Some(return_type) = constant_full_signature.return_type {
                f.write_type(None, return_type, Some(" = "), &constant_full_signature.full_path)?;
            }
            match return_value_expr {
                Expr::Literal(v) => write!(f, "{};", v.value),
                Expr::FunctionCall(_) => {
                    let const_value_id = f.db.constant_const_value(*self)?;
                    let constant_value = const_value_id.long(f.db);
                    if let ConstValue::Int(value, _) = constant_value {
                        let stx = get_syntactic_evaluation(constant_full_signature.item_id, f.db)?;
                        write!(f, "{stx} // = {value}")
                    } else {
                        let stx = get_syntactic_evaluation(constant_full_signature.item_id, f.db)?;
                        write!(f, "{stx};")
                    }
                }
                _ => write!(
                    f,
                    "{}",
                    get_syntactic_evaluation(constant_full_signature.item_id, f.db)?
                ),
            }
        } else {
            write!(f, "{}", get_syntactic_evaluation(constant_full_signature.item_id, f.db)?)
        }?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ImplConstantDefId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let constant_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        if let Some(return_type) = constant_full_signature.return_type {
            f.write_type(
                Some(&format!("const {}: ", constant_full_signature.name.long(f.db))),
                return_type,
                Some(" = "),
                &constant_full_signature.full_path,
            )?;
        }
        let stx = get_syntactic_evaluation(constant_full_signature.item_id, f.db)?;
        f.hir_write(&stx)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for TraitFunctionId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let trait_function_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_function_signature(f, trait_function_full_signature, "".to_string())?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ImplFunctionId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let impl_function_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_function_signature(f, impl_function_full_signature, "".to_string())?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for TraitId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let trait_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write!(
            f,
            "{}trait {}",
            get_syntactic_visibility(&trait_full_signature.visibility),
            trait_full_signature.name.long(f.db),
        )?;
        if let Some(generic_params) = trait_full_signature.generic_params {
            let (stx, lls) = get_generic_params(generic_params, f.db)?;
            f.write_chunk(&stx, lls)?;
        };
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for TraitConstantId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let trait_const_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        let return_type = trait_const_full_signature.return_type.ok_or(
            SignatureError::FailedRetrievingSemanticData(trait_const_full_signature.full_path),
        )?;
        write!(
            f,
            "const {}: {};",
            trait_const_full_signature.name.long(f.db),
            extract_and_format(&return_type.format(f.db)),
        )?;

        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ImplDefId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let impl_def_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        let trait_id = f.db.impl_def_trait(*self)?;

        if let Some(resolver_generic_params) = impl_def_full_signature.resolver_generic_params {
            let resolver_generic_params =
                format_resolver_generic_params(f.db, resolver_generic_params);
            write!(
                f,
                "{}impl {}{} of {}",
                get_syntactic_visibility(&impl_def_full_signature.visibility),
                impl_def_full_signature.name.long(f.db),
                resolver_generic_params,
                trait_id.name(f.db).long(f.db),
            )?;
        }
        if let Some(generic_args) = impl_def_full_signature.generic_args {
            write_generic_args(generic_args, f)?;
        }
        f.hir_write(";")?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ImplAliasId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let impl_alias_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write!(
            f,
            "{}impl {} = ",
            get_syntactic_visibility(&impl_alias_full_signature.visibility),
            self.name(f.db).long(f.db),
        )?;
        let stx = get_syntactic_evaluation(impl_alias_full_signature.item_id, f.db)?;
        write!(f, "{}", stx)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ModuleTypeAliasId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let module_type_alias_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_type_signature(f, module_type_alias_full_signature, false)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for TraitTypeId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let trait_type_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_type_signature(f, trait_type_full_signature, false)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ImplTypeDefId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let impl_type_def_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_type_signature(f, impl_type_def_full_signature, false)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ExternTypeId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let extern_type_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        write_type_signature(f, extern_type_full_signature, true)?;
        f.format();
        Ok(())
    }
}

impl<'db> HirDisplay<'db> for ExternFunctionId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let extern_function_full_signature = Self::retrieve_signature_data(f.db, *self)?;
        let signature = f.db.extern_function_signature(*self)?;
        write_function_signature(f, extern_function_full_signature, "extern ".to_string())?;
        if !signature.implicits.is_empty() {
            f.write_str(" implicits(")?;
            let mut count = signature.implicits.len();
            for type_id in &signature.implicits {
                write!(
                    f,
                    "{}{}",
                    extract_and_format(&type_id.format(f.db)),
                    if count == 1 { ")".to_string() } else { ", ".to_string() }
                )?;
                count -= 1;
            }
        }
        if !signature.panicable {
            f.write_str(" nopanic")?;
        };
        f.hir_write(";")
    }
}

impl<'db> HirDisplay<'db> for MacroDeclarationId<'db> {
    fn hir_fmt(&self, f: &mut HirFormatter<'db>) -> Result<(), SignatureError> {
        let module_item_id = ModuleItemId::MacroDeclaration(*self);
        f.write_str(&format!("macro {} {{", module_item_id.name(f.db).long(f.db)))?;
        let macro_rules_data = f.db.macro_declaration_rules(*self)?;

        for rule_data in macro_rules_data {
            let (left_bracket, elements, right_bracket) = match rule_data.pattern {
                WrappedMacro::Braced(m) => ("{", m.elements(f.db), "}"),
                WrappedMacro::Bracketed(m) => ("[", m.elements(f.db), "]"),
                WrappedMacro::Parenthesized(m) => ("(", m.elements(f.db), ")"),
            };
            let macro_match = elements
                .elements_vec(f.db)
                .iter()
                .map(|element| element.as_syntax_node().get_text(f.db))
                .join("")
                .split_whitespace()
                .join(" ");
            f.write_str(
                format!("\n    {}{}{} => {{ ... }};", left_bracket, macro_match, right_bracket)
                    .as_str(),
            )?;
        }
        f.hir_write("\n}")
    }
}

/// Checks if the given paths come from the same root.
fn is_the_same_root(path1: &str, path2: &str) -> bool {
    fn extract_root(input: &str) -> &str {
        if let Some(index) = input.find("::") { &input[..index] } else { input }
    }
    extract_root(path1) == extract_root(path2)
}

/// A utility function for formatting documentable function data. Use with
/// [`DocumentableItemSignatureData`] argument created for [`FreeFunctionId`], [`TraitFunctionId`],
/// [`ImplFunctionId`] or [`ExternFunctionId`]. As those are the items for which a
/// [`cairo_lang_semantic::items::functions::Signature`] can be retrieved.
fn write_function_signature<'db>(
    f: &mut HirFormatter<'db>,
    documentable_signature: DocumentableItemSignatureData<'db>,
    syntactic_kind: String,
) -> Result<(), SignatureError> {
    let resolver_generic_params = match documentable_signature.resolver_generic_params {
        Some(params) => format_resolver_generic_params(f.db, params),
        None => "".to_string(),
    };

    write!(
        f,
        "{}{}fn {}{}",
        get_syntactic_visibility(&documentable_signature.visibility),
        syntactic_kind,
        documentable_signature.name.long(f.db),
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
                    Some(&format!("{modifier}{modifier_postfix}{}: ", param.name.long(f.db))),
                    param.ty,
                    Some(&postfix),
                    &documentable_signature.full_path,
                )?;
            } else {
                let type_definition = get_type_clause(syntax_node, f.db).unwrap_or_default();
                write!(
                    f,
                    "{modifier}{modifier_postfix}{}{type_definition}{postfix}",
                    param.name.long(f.db)
                )?;
            }
            count -= 1;
        }
    }
    f.write_str(")")?;

    if let Some(return_type) = documentable_signature.return_type
        && !return_type.is_unit(f.db)
    {
        f.write_type(Some(" -> "), return_type, None, &documentable_signature.full_path)?;
    }
    Ok(())
}

/// Retrieves the [`SyntaxKind::TypeClause`] text from a [`SyntaxNode`].
fn get_type_clause<'db>(syntax_node: SyntaxNode<'db>, db: &'db dyn Database) -> Option<String> {
    for child in syntax_node.get_children(db).iter() {
        if child.kind(db) == SyntaxKind::TypeClause {
            return Some(child.get_text_without_all_comment_trivia(db));
        }
    }
    Some(String::from(MISSING))
}

/// Formats the syntax of generic arguments and writes it into [`HirFormatter`].
fn write_generic_args<'db>(
    generic_args: Vec<GenericArgumentId<'db>>,
    f: &mut HirFormatter<'db>,
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

/// A utility function used for formatting documentable types data. Use with
/// [`DocumentableItemSignatureData`] argument created for [`ModuleTypeAliasId`], [`TraitTypeId`],
/// [`ImplTypeDefId`] or [`ExternTypeId`]. Because of the same signature structure.
fn write_type_signature<'db>(
    f: &mut HirFormatter<'db>,
    documentable_signature: DocumentableItemSignatureData<'db>,
    is_extern_type: bool,
) -> Result<(), fmt::Error> {
    write!(
        f,
        "{}{}type {}",
        get_syntactic_visibility(&documentable_signature.visibility),
        if is_extern_type { "extern " } else { "" },
        documentable_signature.name.long(f.db)
    )?;
    if let Some(generic_params) = documentable_signature.generic_params {
        let (stx, lls) = get_generic_params(generic_params, f.db)?;
        f.write_chunk(&stx, lls)?;
    }
    if let Some(return_type) = documentable_signature.return_type {
        write!(f, " = ")?;
        f.write_type(None, return_type, None, &documentable_signature.full_path)?;
    };
    write!(f, ";")?;
    Ok(())
}

/// Returns relevant [`DocumentableItemId`] for [`GenericArgumentId`] if one can be retrieved.
fn resolve_generic_arg<'db>(
    generic_arg_id: GenericArgumentId<'db>,
    db: &'db dyn Database,
) -> Option<DocumentableItemId<'db>> {
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
        GenericArgumentId::NegImpl(_) => None,
    }
}
