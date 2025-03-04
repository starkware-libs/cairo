use std::fmt;
use std::fmt::Write;
use std::option::Option;

use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericImplItemId,
    GenericItemId, GenericModuleItemId, GenericParamId, GenericTraitItemId, ImplAliasId,
    ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId, LanguageElementId,
    LookupItemId, MemberId, ModuleId, ModuleItemId, ModuleTypeAliasId, NamedLanguageElementId,
    StructId, TraitConstantId, TraitFunctionId, TraitId, TraitItemId, TraitTypeId, VariantId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::generics::GenericArgumentId;
use cairo_lang_semantic::items::modifiers::get_relevant_modifier;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::types::TypeId;
use cairo_lang_semantic::{ConcreteTypeId, Expr, GenericParam, Parameter, TypeLongId};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, green};
use cairo_lang_utils::{LookupIntern, Upcast};
use itertools::Itertools;
use smol_str::SmolStr;

use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::documentable_item::DocumentableItemId::Member;

/// Used for indenting children items of complex data type signature e.g. struct members.
const INDENT: &str = "    ";
/// Returned when item's signature could not be determined.
const MISSING: &str = "<missing>";

/// Gets the signature of an item (i.e., item without its body).
pub fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableItemId) -> String {
    get_item_signature_with_links(db, item_id).0
}

/// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
/// signature slices on documentable items.
pub fn get_item_signature_with_links(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> (String, Vec<LocationLink>) {
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
                _ => panic!("get_item_signature not implemented for item_id: {:?}", item_id),
            },
            LookupItemId::TraitItem(item_id) => match item_id {
                TraitItemId::Function(item_id) => item_id.get_signature_with_links(&mut f),
                TraitItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
                TraitItemId::Type(item_id) => item_id.get_signature_with_links(&mut f),
                _ => {
                    panic!("get_item_signature not implemented for item_id: {:?}", item_id)
                }
            },
            LookupItemId::ImplItem(item_id) => match item_id {
                ImplItemId::Function(item_id) => item_id.get_signature_with_links(&mut f),
                ImplItemId::Constant(item_id) => item_id.get_signature_with_links(&mut f),
                ImplItemId::Type(item_id) => item_id.get_signature_with_links(&mut f),
                _ => {
                    panic!("get_item_signature not implemented for item_id: {:?}", item_id)
                }
            },
        },
        DocumentableItemId::Member(item_id) => item_id.get_signature_with_links(&mut f),
        DocumentableItemId::Variant(item_id) => item_id.get_signature_with_links(&mut f),
        DocumentableItemId::Crate(_) => {
            panic!("get_item_signature not implemented for item_id: {:?}", item_id)
        }
    }
}

/// A helper struct gathering documentable item's signature data.
pub struct DocumentableItemSignatureData {
    item_id: DocumentableItemId,
    name: SmolStr,
    visibility: Visibility,
    generic_args: Option<Vec<GenericArgumentId>>,
    generic_params: Option<Vec<GenericParam>>,
    variants: Option<Vec<(SmolStr, TypeId)>>,
    members: Option<Vec<(SmolStr, TypeId, Visibility)>>,
    return_type: Option<TypeId>,
    attributes: Option<Vec<Attribute>>,
    params: Option<Vec<Parameter>>,
    resolver_generic_params: Option<Vec<GenericParamId>>,
    return_value_expr: Option<Expr>,
}

pub trait HirDisplay {
    /// Formats signature.
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error>;

    /// Gets the signature of an item (i.e., item without its body).
    fn get_signature(&self, f: &mut HirFormatter<'_>) -> String {
        self.hir_fmt(f).unwrap();
        f.buf.clone()
    }

    /// Gets the signature of an item and a list of [`LocationLink`]s to enable mapping
    /// signature slices on documentable items.
    fn get_signature_with_links(&self, f: &mut HirFormatter<'_>) -> (String, Vec<LocationLink>) {
        (self.get_signature(f), f.location_links.clone())
    }
}

/// A helper struct to map parts of item signature on respective documentable items.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocationLink {
    /// Link's start offset in documentable item's signature.
    pub start: usize,
    /// Link's end offset in documentable item's signature.
    pub end: usize,
    /// Linked item identifier.
    pub item_id: DocumentableItemId,
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
    ) -> fmt::Result {
        self.write_str(prefix.unwrap_or_default())?;

        if let TypeLongId::Tuple(vec_types) = element_type.lookup_intern(self.db) {
            self.write_str("(")?;
            let mut count = vec_types.len();
            for t in vec_types {
                self.write_type(None, t, if count == 1 { None } else { Some(", ") })?;
                count -= 1;
            }
            self.write_str(")")?;
        } else {
            let documentable_id = resolve_type(self.db, element_type);
            match documentable_id {
                Some(documentable_id) => {
                    let start_offset = self.buf.len();
                    self.write_str(&extract_and_format(&element_type.format(self.db.upcast())))?;
                    let end_offset = self.buf.len();
                    self.add_location_link(start_offset, end_offset, documentable_id);
                }
                None => {
                    self.write_str(&extract_and_format(&element_type.format(self.db.upcast())))?;
                }
            }
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
}

impl HirDisplay for VariantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let name = self.name(f.db.upcast());
        let variant_semantic = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .variant_semantic(self.enum_id(f.db.upcast()), *self)
            .unwrap();
        if !variant_semantic.ty.is_unit(f.db.upcast()) {
            f.write_type(Some(&format!("{name}: ")), variant_semantic.ty, None)
        } else {
            f.write_str(name.as_str())
        }
    }
}

impl HirDisplay for EnumId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let enum_full_signature = get_enum_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "{}enum {} {{",
            get_syntactic_visibility(&enum_full_signature.visibility),
            enum_full_signature.name,
        ))?;
        let variants = enum_full_signature.variants;
        match variants {
            Some(variants) => {
                variants.iter().for_each(|(name, variant_type)| {
                    if !variant_type.is_unit(f.db.upcast()) {
                        f.write_type(
                            Some(&format!("\n{INDENT}{name}: ",)),
                            *variant_type,
                            Some(","),
                        )
                        .unwrap();
                    } else {
                        f.write_str(&format!("\n{INDENT}{name},",)).unwrap()
                    }
                });
                f.write_str("\n}")
            }
            None => f.write_str("}"),
        }
    }
}

impl HirDisplay for MemberId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let member_full_signature = get_member_signature_data(f.db.upcast(), *self);

        if member_full_signature.return_type.unwrap().is_unit(f.db.upcast()) {
            f.write_str(&format!(
                "{}{}",
                get_syntactic_visibility(&member_full_signature.visibility),
                member_full_signature.name
            ))
        } else {
            f.write_type(
                Some(&format!(
                    "{}{}: ",
                    get_syntactic_visibility(&member_full_signature.visibility),
                    member_full_signature.name,
                )),
                member_full_signature.return_type.unwrap(),
                None,
            )
        }
    }
}

impl HirDisplay for StructId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let struct_full_signature = get_struct_signature_data(f.db.upcast(), *self);

        if let Some(attributes) = struct_full_signature.attributes {
            write_struct_attributes_syntax(attributes, f)?;
        }
        f.write_str(&format!(
            "{}struct {}",
            get_syntactic_visibility(&struct_full_signature.visibility),
            struct_full_signature.name,
        ))?;

        write_generic_params(struct_full_signature.generic_params.unwrap(), f)?;
        f.write_str(" {")?;

        if let Some(members) = struct_full_signature.members {
            members.iter().for_each(|(name, member_type, visibility)| {
                f.write_type(
                    Some(&format!("\n{INDENT}{}{}: ", get_syntactic_visibility(visibility), name,)),
                    *member_type,
                    Some(","),
                )
                .unwrap()
            });
            f.write_str(if members.is_empty() { "}" } else { "\n}" })?;
        };
        Ok(())
    }
}

impl HirDisplay for FreeFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let free_function_full_signature = get_free_function_signature_data(f.db.upcast(), *self);
        write_function_signature(f, free_function_full_signature, "".to_string())
    }
}

impl HirDisplay for ConstantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let constant_full_signature = get_constant_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "{}const {}: ",
            get_syntactic_visibility(&constant_full_signature.visibility),
            constant_full_signature.name,
        ))?;

        f.write_type(None, constant_full_signature.return_type.unwrap(), Some(" = "))?;

        match constant_full_signature.return_value_expr.unwrap() {
            Expr::Literal(v) => f.write_str(&format!("{};", v.value,)),
            Expr::FunctionCall(_) => {
                let constant_value = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                    .lookup_intern_const_value(
                        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                            .constant_const_value(*self)
                            .unwrap(),
                    );
                if let ConstValue::Int(value, _) = constant_value {
                    write_syntactic_evaluation(f, constant_full_signature.item_id)?;
                    f.write_str(&format!(" // = {}", value))
                } else {
                    write_syntactic_evaluation(f, constant_full_signature.item_id)
                }
            }
            _ => write_syntactic_evaluation(f, constant_full_signature.item_id),
        }
    }
}

impl HirDisplay for ImplConstantDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let constant_full_signature = get_impl_constant_signature_data(f.db.upcast(), *self);
        f.write_type(
            Some(&format!("const {}: ", constant_full_signature.name,)),
            constant_full_signature.return_type.unwrap(),
            Some(" = "),
        )?;
        write_syntactic_evaluation(f, constant_full_signature.item_id)
    }
}

impl HirDisplay for TraitFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let trait_function_full_signature = get_trait_function_signature_data(f.db, *self);
        write_function_signature(f, trait_function_full_signature, "".to_string())
    }
}

impl HirDisplay for ImplFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let impl_function_full_signature = get_impl_function_signature_data(f.db, *self);
        write_function_signature(f, impl_function_full_signature, "".to_string())
    }
}

impl HirDisplay for TraitId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let trait_full_signature = get_trait_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "{}trait {}",
            get_syntactic_visibility(&trait_full_signature.visibility),
            trait_full_signature.name,
        ))?;
        write_generic_params(trait_full_signature.generic_params.unwrap(), f)
    }
}

impl HirDisplay for TraitConstantId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let trait_const_full_signature = get_trait_const_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "const {}: {};",
            trait_const_full_signature.name,
            extract_and_format(
                &trait_const_full_signature.return_type.unwrap().format(f.db.upcast())
            ),
        ))
    }
}

impl HirDisplay for ImplDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let impl_def_full_signature = get_impl_def_signature_data(f.db.upcast(), *self);
        let trait_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .impl_def_trait(*self)
            .unwrap();
        let resolver_generic_params = format_resolver_generic_params(
            f.db,
            impl_def_full_signature.resolver_generic_params.unwrap(),
        );
        f.write_str(&format!(
            "{}impl {}{} of {}",
            get_syntactic_visibility(&impl_def_full_signature.visibility),
            impl_def_full_signature.name,
            resolver_generic_params,
            trait_id.name(f.db.upcast()),
        ))?;
        write_generic_args(impl_def_full_signature.generic_args.unwrap(), f)?;
        f.write_str(";")
    }
}

impl HirDisplay for ImplAliasId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let impl_alias_full_signature = get_impl_alias_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "{}impl {} = ",
            get_syntactic_visibility(&impl_alias_full_signature.visibility),
            self.name(f.db.upcast()),
        ))?;
        write_syntactic_evaluation(f, impl_alias_full_signature.item_id)
    }
}

impl HirDisplay for ModuleTypeAliasId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let module_type_alias_full_signature = get_module_type_alias_full_signature(f.db, *self);
        f.write_str(&format!(
            "{}impl {} = ",
            get_syntactic_visibility(&module_type_alias_full_signature.visibility),
            self.name(f.db.upcast()),
        ))?;
        write_syntactic_evaluation(f, module_type_alias_full_signature.item_id)
    }
}

impl HirDisplay for TraitTypeId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let trait_type_full_signature = get_trait_type_full_signature(f.db, *self);
        f.write_str(&format!("type {};", trait_type_full_signature.name,))
    }
}

impl HirDisplay for ImplTypeDefId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let impl_type_def_full_signature = get_impl_type_def_full_signature(f.db, *self);
        f.write_str(&format!(
            "type {} = {};",
            impl_type_def_full_signature.name,
            extract_and_format(
                &impl_type_def_full_signature.return_type.unwrap().format(f.db.upcast())
            ),
        ))
    }
}

impl HirDisplay for ExternTypeId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let extern_type_full_signature = get_extern_type_full_signature(f.db, *self);
        f.write_str(&format!(
            "{}extern type {}",
            get_syntactic_visibility(&extern_type_full_signature.visibility),
            self.name(f.db.upcast()),
        ))?;
        let generic_params = extern_type_full_signature.generic_params.unwrap();
        if !generic_params.is_empty() {
            f.write_str("<")?;
            generic_params.iter().for_each(|param| {
                f.write_str(
                    param.id().name(f.db.upcast()).unwrap_or(SmolStr::from(MISSING)).as_str(),
                )
                .unwrap()
            });
            f.write_str(">")?;
        }
        f.write_str(";")
    }
}

impl HirDisplay for ExternFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter<'_>) -> Result<(), fmt::Error> {
        let extern_function_full_signature = get_extern_function_full_signature(f.db, *self);
        let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .extern_function_signature(*self)
            .unwrap();

        write_function_signature(f, extern_function_full_signature, "extern ".to_string())?;

        if !signature.implicits.is_empty() {
            f.write_str(" implicits(")?;

            let mut count = signature.implicits.len();
            signature.implicits.iter().for_each(|type_id| {
                f.write_str(&format!(
                    "{}{}",
                    extract_and_format(&type_id.format(f.db.upcast())),
                    if count == 1 { ")".to_string() } else { ", ".to_string() }
                ))
                .unwrap();
                count -= 1;
            })
        }
        if !signature.panicable {
            f.write_str(" nopanic")?;
        };
        f.write_str(";")
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

/// Performs formatting for types full paths. For example "core::felt252" input results in "felt252"
/// output.
pub fn extract_and_format(input: &str) -> String {
    fn inner(input: &str) -> String {
        input
            .split(',')
            .map(|part| {
                let mut parts = part.split("::").filter(|s| !s.is_empty()).collect::<Vec<_>>();

                if parts.len() >= 2 && parts.last().unwrap().contains("<") {
                    let last = parts.pop().unwrap();
                    let generic_parts = last
                        .split::<&[_]>(&['<', '>', ':'])
                        .filter(|s| !s.is_empty())
                        .collect::<Vec<_>>();
                    if generic_parts.len() >= 2 {
                        let l = generic_parts.len();
                        parts.push(generic_parts[l - 2]);
                        format!("{}<{}>", parts.join("::"), generic_parts[l - 1])
                    } else {
                        last.to_owned()
                    }
                } else {
                    parts.pop().unwrap_or(part).trim().to_owned()
                }
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    let mut result = String::new();
    let mut temp = String::new();
    let mut nest_level = 0;

    for c in input.chars() {
        match c {
            '(' | '[' | '<' => {
                if nest_level == 0 && !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                nest_level += 1;
                result.push(c);
            }
            ')' | ']' | '>' => {
                if nest_level == 1 && !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                nest_level -= 1;
                result.push(c);
            }
            ',' if nest_level > 0 => {
                if !temp.is_empty() {
                    result.push_str(&inner(&temp));
                    temp.clear();
                }
                result.push(c);
                result.push(' ');
            }
            ',' => temp.push(c),
            _ => temp.push(c),
        }
    }

    if !temp.is_empty() {
        result.push_str(&inner(&temp));
    }
    result
}

/// Takes a list of [`GenericParamId`]s and formats it into a String representation used for
/// signature documentation.
fn format_resolver_generic_params(db: &dyn DocGroup, params: Vec<GenericParamId>) -> String {
    if !params.is_empty() {
        format!("<{}>", params.iter().map(|param| { param.format(db.upcast()) }).join(", "))
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

    f.write_str(&format!(
        "{}{}fn {}{}",
        get_syntactic_visibility(&documentable_signature.visibility),
        syntactic_kind,
        documentable_signature.name,
        resolver_generic_params,
    ))?;
    if let Some(generic_args) = documentable_signature.generic_args {
        write_generic_args(generic_args, f)?;
    }
    f.write_str("(")?;
    if let Some(params) = documentable_signature.params {
        let mut count = params.len();
        let mut postfix = String::from(", ");
        params.iter().for_each(|param| {
            if count == 1 {
                postfix = "".to_string();
            }
            let syntax_node = param.id.stable_location(f.db.upcast()).syntax_node(f.db.upcast());
            let modifier = get_relevant_modifier(&param.mutability);
            let modifier_postfix = if modifier.is_empty() { "" } else { " " };
            if param.ty.is_fully_concrete(f.db.upcast()) {
                f.write_type(
                    Some(&format!("{modifier}{modifier_postfix}{}: ", param.name)),
                    param.ty,
                    Some(&postfix),
                )
                .unwrap();
            } else {
                let type_definition = get_type_clause(syntax_node, f.db).unwrap();
                f.write_str(&format!(
                    "{modifier}{modifier_postfix}{}{type_definition}{postfix}",
                    param.name,
                ))
                .unwrap();
            }
            count -= 1;
        });
    }
    f.write_str(")")?;

    if let Some(return_type) = documentable_signature.return_type {
        if !return_type.is_unit(f.db.upcast()) {
            f.write_type(Some(" -> "), return_type, None)?;
        }
    }
    Ok(())
}

/// Retrieves [`SyntaxKind::TypeClause`] text from [`SyntaxNode`].
fn get_type_clause(syntax_node: SyntaxNode, db: &dyn DocGroup) -> Option<String> {
    let children = db.get_children(syntax_node);
    for child in children.iter() {
        if child.kind(db.upcast()) == SyntaxKind::TypeClause {
            return Some(child.clone().get_text_without_all_comment_trivia(db.upcast()));
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
        generic_params.iter().for_each(|param| {
            let generic_item = param.id().generic_item(f.db.upcast());
            let documentable_id = resolve_generic_item(generic_item, f.db.upcast());
            match param {
                GenericParam::Type(param_type) => {
                    let name = extract_and_format(&param_type.id.format(f.db.upcast()));
                    f.write_link(name, documentable_id).unwrap();
                    f.write_str(if count == 1 { "" } else { ", " }).unwrap()
                }
                GenericParam::Const(param_const) => {
                    let name = extract_and_format(&param_const.id.format(f.db.upcast()));
                    f.write_str("const").unwrap();
                    f.write_link(name, documentable_id).unwrap();
                    f.write_str(if count == 1 { "" } else { ", " }).unwrap()
                }
                GenericParam::Impl(param_impl) => {
                    let name = extract_and_format(&param_impl.id.format(f.db.upcast()));
                    f.write_link(name, documentable_id).unwrap();
                    f.write_str(if count == 1 { "" } else { ", " }).unwrap()
                }
                GenericParam::NegImpl(_) => f.write_str(MISSING).unwrap(),
            };
            count -= 1;
        });

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
    generic_args.iter().for_each(|arg| {
        let documentable_id = resolve_generic_arg(*arg, f.db);
        f.write_link(extract_and_format(&arg.format(f.db.upcast())), documentable_id).unwrap();
        f.write_str(if count == 1 { ">" } else { ", " }).unwrap();
        count -= 1;
    });
    Ok(())
}

/// Formats syntax of struct attributes and writes it into [`HirFormatter`].
fn write_struct_attributes_syntax(
    attributes: Vec<Attribute>,
    f: &mut HirFormatter<'_>,
) -> Result<(), fmt::Error> {
    attributes.iter().for_each(|a| {
        let syntax_node = a.stable_ptr.lookup(f.db.upcast()).as_syntax_node();
        let children =
            <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db).get_children(syntax_node);
        for child in children.iter() {
            let to_text = child.clone().get_text_without_all_comment_trivia(f.db.upcast());
            let cleaned_text = to_text.replace("\n", "");
            f.write_str(&cleaned_text).unwrap();
        }
        f.write_str("\n").unwrap();
    });
    Ok(())
}

/// Formats syntax of documentable item and writes it into [`HirFormatter`].
fn write_syntactic_evaluation(
    f: &mut HirFormatter<'_>,
    item_id: DocumentableItemId,
) -> Result<(), fmt::Error> {
    let syntax_node = item_id.stable_location(f.db.upcast()).unwrap().syntax_node(f.db.upcast());

    if matches!(
        &syntax_node.green_node(f.db.upcast()).details,
        green::GreenNodeDetails::Node { .. }
    ) {
        let mut is_after_evaluation_value = false;
        for child in f.db.get_children(syntax_node.clone()).iter() {
            let kind = child.kind(f.db.upcast());
            if !matches!(kind, SyntaxKind::Trivia) {
                if matches!(kind, SyntaxKind::TerminalSemicolon) {
                    f.buf.write_str(";")?;
                    return Ok(());
                }
                if is_after_evaluation_value {
                    f.buf.write_str(&SyntaxNode::get_text_without_all_comment_trivia(
                        child,
                        f.db.upcast(),
                    ))?;
                };
                if matches!(kind, SyntaxKind::TerminalEq) {
                    is_after_evaluation_value = true;
                }
            }
        }
    };

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
                LookupItemId::ModuleItem(ModuleItemId::Trait(trait_type_id.trait_id(db.upcast()))),
            )),
        },
        GenericItemId::ImplItem(generic_impl_item_id) => match generic_impl_item_id {
            GenericImplItemId::Type(impl_type_def_id) => {
                Some(DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(
                    impl_type_def_id.impl_def_id(db.upcast()),
                ))))
            }
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
        GenericArgumentId::Constant(constant_value_id) => match constant_value_id.ty(db.upcast()) {
            Ok(type_id) => resolve_type(db, type_id),
            Err(_) => None,
        },
        GenericArgumentId::Impl(impl_id) => {
            let trait_id = impl_id.concrete_trait(db.upcast()).unwrap().trait_id(db.upcast());
            Some(DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(trait_id))))
        }
        GenericArgumentId::NegImpl => None,
    }
}

/// Returns relevant [`DocumentableItemId`] for [`TypeId`] if one can be retrieved.
fn resolve_type(db: &dyn DocGroup, type_id: TypeId) -> Option<DocumentableItemId> {
    let intern = type_id.lookup_intern(db);
    match intern {
        TypeLongId::Concrete(concrete_type_id) => match concrete_type_id {
            ConcreteTypeId::Struct(struct_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Struct(struct_id.struct_id(db.upcast()))),
            )),
            ConcreteTypeId::Enum(enum_id) => Some(DocumentableItemId::from(
                LookupItemId::ModuleItem(ModuleItemId::Enum(enum_id.enum_id(db.upcast()))),
            )),
            ConcreteTypeId::Extern(extern_id) => {
                Some(DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternType(
                    extern_id.extern_type_id(db.upcast()),
                ))))
            }
        },
        TypeLongId::Tuple(_) => {
            panic!("resolve_type not implemented for TypeLongId::Tuple")
        }
        TypeLongId::Snapshot(type_id) => resolve_type(db, type_id),
        TypeLongId::GenericParameter(generic_param_id) => {
            let item = generic_param_id.generic_item(db.upcast());
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
                let item = generic_param_id.generic_item(db.upcast());
                resolve_generic_item(item, db)
            }
            InferenceId::GenericImplParamTrait(generic_param_id) => {
                let item = generic_param_id.generic_item(db.upcast());
                resolve_generic_item(item, db)
            }
            InferenceId::GlobalUseStar(global_use_id) => {
                match <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
                    .priv_global_use_imported_module(global_use_id)
                {
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
            let concrete_function = function_id.get_concrete(db.upcast());
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
        TypeLongId::ImplType(impl_type_id) => {
            match impl_type_id.impl_id().concrete_trait(db.upcast()) {
                Ok(concrete_trait_id) => Some(DocumentableItemId::from(LookupItemId::ModuleItem(
                    ModuleItemId::Trait(concrete_trait_id.trait_id(db.upcast())),
                ))),
                Err(_) => None,
            }
        }
        TypeLongId::Closure(closure_type_id) => resolve_type(db, closure_type_id.ret_ty),
        TypeLongId::Missing(_) => None,
    }
}

/// Retrieves data for enum signature formatting.
fn get_enum_signature_data(db: &dyn DocGroup, item_id: EnumId) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::Enum(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let mut variants: Vec<(SmolStr, TypeId)> = Vec::new();
    <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .enum_variants(item_id)
        .unwrap()
        .iter()
        .for_each(|(name, variant_id)| {
            let variant_semantic = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
                .variant_semantic(item_id, *variant_id)
                .unwrap();
            variants.push((name.clone(), variant_semantic.ty))
        });

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Enum(item_id))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for struct signature formatting.
fn get_struct_signature_data(
    db: &dyn DocGroup,
    item_id: StructId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::Struct(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let struct_attributes =
        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db).struct_attributes(item_id).unwrap();

    let members = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .struct_members(item_id)
        .unwrap()
        .iter()
        .map(|(name, member)| (name.clone(), member.ty, member.visibility))
        .collect();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Struct(item_id))),
        name: item_id.name(db.upcast()),
        visibility: module_item_info.visibility,
        generic_args: None,
        generic_params: Some(
            <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
                .struct_generic_params(item_id)
                .unwrap(),
        ),
        variants: None,
        members: Some(members),
        return_type: None,
        attributes: Some(struct_attributes),
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
    }
}

/// Retrieves data for member signature formatting.
fn get_member_signature_data(
    db: &dyn DocGroup,
    item_id: MemberId,
) -> DocumentableItemSignatureData {
    let name = item_id.name(db.upcast());
    let struct_id = item_id.struct_id(db.upcast());
    let semantic_members =
        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db).struct_members(struct_id).unwrap();
    let member = semantic_members.get(&name).unwrap();

    DocumentableItemSignatureData {
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
    }
}

/// Retrieves data for free function signature formatting.
fn get_free_function_signature_data(
    db: &dyn DocGroup,
    item_id: FreeFunctionId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::FreeFunction(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .free_function_generic_params(item_id)
        .unwrap();

    let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .free_function_signature(item_id)
        .unwrap();

    let resolver_data = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .free_function_declaration_resolver_data(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::FreeFunction(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for trait function signature formatting.
fn get_trait_function_signature_data(
    db: &dyn DocGroup,
    item_id: TraitFunctionId,
) -> DocumentableItemSignatureData {
    let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .trait_function_signature(item_id)
        .unwrap();
    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .trait_function_generic_params(item_id)
        .unwrap();

    let resolver_data = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .trait_function_resolver_data(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(Function(item_id))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for impl function signature formatting.
fn get_impl_function_signature_data(
    db: &dyn DocGroup,
    item_id: ImplFunctionId,
) -> DocumentableItemSignatureData {
    let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_function_signature(item_id)
        .unwrap();
    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_function_generic_params(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Function(item_id))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for constant signature formatting.
fn get_constant_signature_data(
    db: &dyn DocGroup,
    item_id: ConstantId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::Constant(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let constant = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .constant_semantic_data(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Constant(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for impl constant signature formatting.
fn get_impl_constant_signature_data(
    db: &dyn DocGroup,
    item_id: ImplConstantDefId,
) -> DocumentableItemSignatureData {
    let def_value_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_constant_def_value(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Constant(item_id))),
        name: item_id.name(db.upcast()),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(def_value_id.ty(db.upcast()).unwrap()),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
    }
}

/// Retrieves data for trait signature formatting.
fn get_trait_signature_data(db: &dyn DocGroup, item_id: TraitId) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::Trait(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .trait_generic_params(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Trait(item_id))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for trait const signature formatting.
fn get_trait_const_signature_data(
    db: &dyn DocGroup,
    item_id: TraitConstantId,
) -> DocumentableItemSignatureData {
    let attributes =
        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db).trait_constant_attributes(item_id);
    let return_type =
        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db).trait_constant_type(item_id);
    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Constant(item_id))),
        name: item_id.name(db.upcast()),
        visibility: Visibility::Private,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(return_type.unwrap()),
        attributes: Some(attributes.unwrap()),
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
    }
}

/// Retrieves data for implementation signature formatting.
fn get_impl_def_signature_data(
    db: &dyn DocGroup,
    item_id: ImplDefId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::Impl(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let resolver_data = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_def_resolver_data(item_id)
        .unwrap();
    let concrete_trait_id = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_def_concrete_trait(item_id)
        .unwrap();
    let intern =
        concrete_trait_id.lookup_intern(<dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db));

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::Impl(item_id))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for alias signature formatting.
fn get_impl_alias_signature_data(
    db: &dyn DocGroup,
    item_id: ImplAliasId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::ImplAlias(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ImplAlias(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for type alias signature formatting.
fn get_module_type_alias_full_signature(
    db: &dyn DocGroup,
    item_id: ModuleTypeAliasId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::TypeAlias(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::TypeAlias(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for trait type signature formatting.
fn get_trait_type_full_signature(
    db: &dyn DocGroup,
    item_id: TraitTypeId,
) -> DocumentableItemSignatureData {
    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::TraitItem(TraitItemId::Type(item_id))),
        name: item_id.name(db.upcast()),
        visibility: Visibility::Public,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: None,
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
    }
}

/// Retrieves data for type signature formatting.
fn get_impl_type_def_full_signature(
    db: &dyn DocGroup,
    item_id: ImplTypeDefId,
) -> DocumentableItemSignatureData {
    let resolved_type = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .impl_type_def_resolved_type(item_id)
        .unwrap();
    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ImplItem(ImplItemId::Type(item_id))),
        name: item_id.name(db.upcast()),
        visibility: Visibility::Public,
        generic_args: None,
        generic_params: None,
        variants: None,
        members: None,
        return_type: Some(resolved_type),
        attributes: None,
        params: None,
        resolver_generic_params: None,
        return_value_expr: None,
    }
}

/// Retrieves data for extern type signature formatting.
fn get_extern_type_full_signature(
    db: &dyn DocGroup,
    item_id: ExternTypeId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::ExternType(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();
    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .extern_type_declaration_generic_params(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternType(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}

/// Retrieves data for extern function signature formatting.
fn get_extern_function_full_signature(
    db: &dyn DocGroup,
    item_id: ExternFunctionId,
) -> DocumentableItemSignatureData {
    let module_item_id = ModuleItemId::ExternFunction(item_id);
    let parent_module = module_item_id.parent_module(db.upcast());
    let item_name = module_item_id.name(db.upcast());
    let module_item_info = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .module_item_info_by_name(parent_module, item_name.clone())
        .unwrap()
        .unwrap();

    let generic_params = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .extern_function_declaration_generic_params(item_id)
        .unwrap();

    let signature = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(db)
        .extern_function_signature(item_id)
        .unwrap();

    DocumentableItemSignatureData {
        item_id: DocumentableItemId::from(LookupItemId::ModuleItem(ModuleItemId::ExternFunction(
            item_id,
        ))),
        name: item_id.name(db.upcast()),
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
    }
}
