use crate::db::DocGroup;
use crate::documentable_item::DocumentableItemId;
use crate::documentable_item::DocumentableItemId::Member;
use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    ConstantId, EnumId, ExternFunctionId, ExternTypeId, FreeFunctionId, GenericParamId,
    ImplAliasId, ImplConstantDefId, ImplDefId, ImplFunctionId, ImplItemId, ImplTypeDefId,
    LanguageElementId, LookupItemId, MemberId, ModuleItemId, ModuleTypeAliasId,
    NamedLanguageElementId, StructId, TraitConstantId, TraitFunctionId, TraitId, TraitItemId,
    TraitTypeId, VariantId,
};
use cairo_lang_semantic::db::SemanticGroup;
use cairo_lang_semantic::items::constant::ConstValue;
use cairo_lang_semantic::items::generics::GenericArgumentId;
use cairo_lang_semantic::items::modifiers::get_relevant_modifier;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::types::TypeId;
use cairo_lang_semantic::{Expr, GenericParam, Parameter};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, green};
use cairo_lang_utils::{LookupIntern, Upcast};
use itertools::Itertools;
use smol_str::SmolStr;
use std::fmt;
use std::fmt::Write;
use std::option::Option;

/// Used for indenting children items of complex data type signature e.g. struct members.
const INDENT: &str = "    ";
/// Returned when item's signature could not be determined.
const MISSING: &str = "<missing>";

pub fn get_item_signature(db: &dyn DocGroup, item_id: DocumentableItemId) -> String {
    let (signature, _) = get_item_signature_with_elements(db, item_id);
    signature
}

pub fn get_item_signature_with_elements(
    db: &dyn DocGroup,
    item_id: DocumentableItemId,
) -> (String, Vec<SignatureElement>) {
    let mut f = HirFormatter::new(db);
    match item_id {
        DocumentableItemId::LookupItem(item_id) => match item_id {
            LookupItemId::ModuleItem(item_id) => match item_id {
                ModuleItemId::Struct(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::Enum(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::Constant(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::FreeFunction(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::TypeAlias(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::ImplAlias(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::Trait(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::Impl(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::ExternType(item_id) => item_id.get_signature_with_elements(&mut f),
                ModuleItemId::ExternFunction(item_id) => {
                    item_id.get_signature_with_elements(&mut f)
                }
                _ => panic!("get_item_signature not implemented for item_id: {:?}", item_id),
            },
            LookupItemId::TraitItem(item_id) => match item_id {
                TraitItemId::Function(item_id) => item_id.get_signature_with_elements(&mut f),
                TraitItemId::Constant(item_id) => item_id.get_signature_with_elements(&mut f),
                TraitItemId::Type(item_id) => item_id.get_signature_with_elements(&mut f),
                _ => {
                    panic!("get_item_signature not implemented for item_id: {:?}", item_id)
                }
            },
            LookupItemId::ImplItem(item_id) => match item_id {
                ImplItemId::Function(item_id) => item_id.get_signature_with_elements(&mut f),
                ImplItemId::Constant(item_id) => item_id.get_signature_with_elements(&mut f),
                ImplItemId::Type(item_id) => item_id.get_signature_with_elements(&mut f),
                _ => {
                    panic!("get_item_signature not implemented for item_id: {:?}", item_id)
                }
            },
        },
        DocumentableItemId::Member(item_id) => item_id.get_signature_with_elements(&mut f),
        DocumentableItemId::Variant(item_id) => item_id.get_signature_with_elements(&mut f),
        DocumentableItemId::Crate(_) => {
            panic!("get_item_signature not implemented for item_id: {:?}", item_id)
        }
    }
}

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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error>;

    fn get_signature(&self, f: &mut HirFormatter) -> String {
        self.hir_fmt(f).unwrap();
        f.buf.clone()
    }

    fn get_signature_with_elements(&self, f: &mut HirFormatter) -> (String, Vec<SignatureElement>) {
        (self.get_signature(f), f.elements.clone())
    }
}

/// A helper struct to reconstruct elements full paths mapped on formatted signature string.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SignatureElement {
    /// A slice of documentable signature.
    pub signature: String,
    /// Maps an item full path on relevant DocumentableItem signature slice.
    pub full_path: Option<String>,
}

/// High-Level Intermediate Representation semantic data Formatter used for item's signature creation.
pub struct HirFormatter<'a> {
    /// The database handle.
    db: &'a dyn DocGroup,
    /// A buffer to intercept writes with.
    buf: String,
    elements: Vec<SignatureElement>, //todo: test, pub expose
}

impl fmt::Write for HirFormatter<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buf.push_str(s);
        self.elements.push(SignatureElement { signature: s.to_string(), full_path: None });
        Ok(())
    }
}

fn write_type(
    prefix: Option<String>,
    element_type: TypeId,
    postfix: Option<String>,
    f: &mut HirFormatter,
) -> fmt::Result {
    if element_type.is_fully_concrete(f.db.upcast()) {
        write_item_with_path(prefix, element_type.format(f.db.upcast()), postfix, f)
    } else {
        f.write_str(&format!(
            "{}{}{}",
            prefix.unwrap_or("".to_string()),
            extract_and_format(&element_type.format(f.db.upcast())),
            postfix.unwrap_or("".to_string()),
        ))
    }
}

fn write_item_with_path(
    prefix: Option<String>,
    full_path: String,
    postfix: Option<String>,
    f: &mut HirFormatter,
) -> fmt::Result {
    let type_signature = extract_and_format(&full_path);

    if let Some(prefix) = prefix {
        f.buf.push_str(&prefix);
        f.elements.push(SignatureElement { signature: prefix, full_path: None });
    }
    f.buf.push_str(&type_signature);
    f.elements.push(SignatureElement { signature: type_signature, full_path: Some(full_path) });

    if let Some(postfix) = postfix {
        f.buf.push_str(&postfix);
        f.elements.push(SignatureElement { signature: postfix, full_path: None });
    };
    Ok(())
}

impl<'a> HirFormatter<'a> {
    pub fn new(db: &'a dyn DocGroup) -> Self {
        Self { db, buf: String::new(), elements: Vec::new() }
    }
}

impl HirDisplay for VariantId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let name = self.name(f.db.upcast());
        let variant_semantic = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
            .variant_semantic(self.enum_id(f.db.upcast()), *self)
            .unwrap();
        if !variant_semantic.ty.is_unit(f.db.upcast()) {
            write_type(Some(format!("{name}: ",)), variant_semantic.ty, None, f)
        } else {
            f.write_str(name.as_str())
        }
    }
}

impl HirDisplay for EnumId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
                        write_type(
                            Some(format!("\n{INDENT}{name}: ",)),
                            *variant_type,
                            Some(",".to_string()),
                            f,
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let member_full_signature = get_member_signature_data(f.db.upcast(), *self);

        if member_full_signature.return_type.unwrap().is_unit(f.db.upcast()) {
            f.write_str(&format!(
                "{}{}",
                get_syntactic_visibility(&member_full_signature.visibility),
                member_full_signature.name
            ))
        } else {
            write_type(
                Some(format!(
                    "{}{}: ",
                    get_syntactic_visibility(&member_full_signature.visibility),
                    member_full_signature.name,
                )),
                member_full_signature.return_type.unwrap(),
                None,
                f,
            )
        }
    }
}

impl HirDisplay for StructId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
                write_type(
                    Some(format!("\n{INDENT}{}{}: ", get_syntactic_visibility(visibility), name,)),
                    *member_type,
                    Some(",".to_string()),
                    f,
                )
                .unwrap()
            });
            f.write_str(if members.is_empty() { "}" } else { "\n}" })?;
        };
        Ok(())
    }
}

impl HirDisplay for FreeFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let free_function_full_signature = get_free_function_signature_data(f.db.upcast(), *self);
        write_function_signature(f, free_function_full_signature, "".to_string())
    }
}

impl HirDisplay for ConstantId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let constant_full_signature = get_constant_signature_data(f.db.upcast(), *self);
        f.write_str(&format!(
            "{}const {}: ",
            get_syntactic_visibility(&constant_full_signature.visibility),
            constant_full_signature.name,
        ))?;

        write_type(None, constant_full_signature.return_type.unwrap(), Some(" = ".to_string()), f)?;

        let return_value_expression = match constant_full_signature.return_value_expr.unwrap() {
            Expr::Literal(v) => {
                format!("{};", v.value,)
            }
            Expr::FunctionCall(_) => {
                let constant_value = <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                    .lookup_intern_const_value(
                        <dyn DocGroup as Upcast<dyn SemanticGroup>>::upcast(f.db)
                            .constant_const_value(*self)
                            .unwrap(),
                    );

                if let ConstValue::Int(value, _) = constant_value {
                    format!(
                        "{} // = {}",
                        get_syntactic_evaluation(f.db, constant_full_signature.item_id),
                        value
                    )
                } else {
                    get_syntactic_evaluation(f.db, constant_full_signature.item_id)
                }
            }
            _ => get_syntactic_evaluation(f.db, constant_full_signature.item_id),
        };
        f.write_str(&return_value_expression)
    }
}

impl HirDisplay for ImplConstantDefId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let constant_full_signature = get_impl_constant_signature_data(f.db.upcast(), *self);
        write_type(
            Some(format!("const {}: ", constant_full_signature.name,)),
            constant_full_signature.return_type.unwrap(),
            Some(" = ".to_string()),
            f,
        )?;
        write_syntactic_evaluation(f, constant_full_signature.item_id)
    }
}

impl HirDisplay for TraitFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let trait_function_full_signature = get_trait_function_signature_data(f.db, *self);
        write_function_signature(f, trait_function_full_signature, "".to_string())
    }
}

impl HirDisplay for ImplFunctionId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let impl_function_full_signature = get_impl_function_signature_data(f.db, *self);
        write_function_signature(f, impl_function_full_signature, "".to_string())
    }
}

impl HirDisplay for TraitId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
        let trait_type_full_signature = get_trait_type_full_signature(f.db, *self);
        f.write_str(&format!("type {};", trait_type_full_signature.name,))
    }
}

impl HirDisplay for ImplTypeDefId {
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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
    fn hir_fmt(&self, f: &mut HirFormatter) -> Result<(), fmt::Error> {
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

pub fn get_syntactic_visibility(semantic_visibility: &Visibility) -> &str {
    match semantic_visibility {
        Visibility::Public => "pub ",
        Visibility::PublicInCrate => "pub(crate) ",
        Visibility::Private => "",
    }
}

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

fn format_resolver_generic_params(db: &dyn DocGroup, params: Vec<GenericParamId>) -> String {
    if !params.is_empty() {
        format!("<{}>", params.iter().map(|param| { param.format(db.upcast()) }).join(", "))
    } else {
        "".to_string()
    }
}

fn write_function_signature(
    f: &mut HirFormatter,
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
                write_type(
                    Some(format!("{modifier}{modifier_postfix}{}: ", param.name)),
                    param.ty,
                    Some(postfix.to_string()),
                    f,
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
            write_type(Some(" -> ".to_string()), return_type, None, f)?;
        }
    }
    Ok(())
}

fn get_type_clause(syntax_node: SyntaxNode, db: &dyn DocGroup) -> Option<String> {
    let children = db.get_children(syntax_node);
    for child in children.iter() {
        if child.kind(db.upcast()) == SyntaxKind::TypeClause {
            return Some(child.clone().get_text_without_all_comment_trivia(db.upcast()));
        }
    }
    Some(String::from(MISSING))
}

fn write_generic_params(
    generic_params: Vec<GenericParam>,
    f: &mut HirFormatter,
) -> Result<(), fmt::Error> {
    if !generic_params.is_empty() {
        let mut count = generic_params.len();
        f.write_str("<")?;
        generic_params.iter().for_each(|param| {
            match param {
                GenericParam::Type(param_type) => write_item_with_path(
                    None,
                    param_type.id.format(f.db.upcast()),
                    if count == 1 { None } else { Some(String::from(", ")) },
                    f,
                )
                .unwrap(),
                GenericParam::Const(param_const) => {
                    write_item_with_path(
                        Some(format!("const{}", param_const.id.format(f.db.upcast()))),
                        param_const.ty.format(f.db.upcast()),
                        if count == 1 { None } else { Some(String::from(", ")) },
                        f,
                    )
                    .unwrap();
                }
                GenericParam::Impl(param_impl) => {
                    param_impl.id.format(f.db.upcast()).to_string();
                    write_item_with_path(
                        None,
                        param_impl.id.format(f.db.upcast()),
                        if count == 1 { None } else { Some(String::from(", ")) },
                        f,
                    )
                    .unwrap();
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

fn write_generic_args(
    generic_args: Vec<GenericArgumentId>,
    f: &mut HirFormatter,
) -> Result<(), fmt::Error> {
    let mut count = generic_args.len();
    generic_args.iter().for_each(|arg| {
        write_item_with_path(
            if count == generic_args.len() { Some("<".to_string()) } else { None },
            arg.format(f.db.upcast()),
            Some(if count == 1 { ">".to_string() } else { ", ".to_string() }),
            f,
        )
        .unwrap();
        count -= 1;
    });
    Ok(())
}

fn write_struct_attributes_syntax(
    attributes: Vec<Attribute>,
    f: &mut HirFormatter,
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

fn write_syntactic_evaluation(
    f: &mut HirFormatter,
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

fn get_syntactic_evaluation(db: &dyn DocGroup, item_id: DocumentableItemId) -> String {
    let syntax_node = item_id.stable_location(db.upcast()).unwrap().syntax_node(db.upcast());
    let mut buf: String = String::new();
    if matches!(&syntax_node.green_node(db.upcast()).details, green::GreenNodeDetails::Node { .. })
    {
        let mut is_after_evaluation_value = false;
        for child in db.get_children(syntax_node.clone()).iter() {
            let kind = child.kind(db.upcast());
            if !matches!(kind, SyntaxKind::Trivia) {
                if matches!(kind, SyntaxKind::TerminalSemicolon) {
                    write!(buf, ":",).unwrap();
                }
                if is_after_evaluation_value {
                    buf.write_str(&SyntaxNode::get_text_without_all_comment_trivia(
                        child,
                        db.upcast(),
                    ))
                    .unwrap();
                };
                if matches!(kind, SyntaxKind::TerminalEq) {
                    is_after_evaluation_value = true;
                }
            }
        }
    };

    buf
}

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
