use std::fmt;

use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    GenericImplItemId, GenericItemId, GenericKind, GenericModuleItemId, GenericParamId,
    GenericTraitItemId, ImplItemId, LookupItemId, ModuleId, ModuleItemId, TraitItemId,
};
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::generics::GenericParamSemantic;
use cairo_lang_semantic::items::us::UseSemantic;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::{ConcreteTypeId, GenericParam, TypeId, TypeLongId};
use cairo_lang_syntax::attribute::structured::Attribute;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedStablePtr, TypedSyntaxNode, green};
use itertools::Itertools;
use salsa::Database;

use crate::documentable_item::DocumentableItemId;
use crate::location_links::LocationLink;

/// Formats and returns [`GenericParam`]s data.
pub fn get_generic_params<'db>(
    generic_params: Vec<GenericParam<'db>>,
    db: &'db dyn Database,
) -> Result<(String, Vec<LocationLink<'db>>), fmt::Error> {
    let mut buff = String::new();
    let mut location_links: Vec<LocationLink<'_>> = Vec::new();

    if !generic_params.is_empty() {
        let mut count = generic_params.len();
        buff.push('<');

        for param in generic_params {
            match param {
                GenericParam::Type(param_type) => {
                    let name = extract_and_format(param_type.id.format(db).long(db));
                    buff.push_str(&format!("{}{}", name, if count == 1 { "" } else { ", " }));
                }
                GenericParam::Const(param_const) => {
                    let name = extract_and_format(param_const.id.format(db).long(db));
                    buff.push_str(&format!("const {}{}", name, if count == 1 { "" } else { ", " }));
                }
                GenericParam::Impl(param_impl) => {
                    let name = extract_and_format(param_impl.id.format(db).long(db));
                    match param_impl.concrete_trait {
                        Ok(concrete_trait) => {
                            let documentable_id =
                                DocumentableItemId::from(LookupItemId::ModuleItem(
                                    ModuleItemId::Trait(concrete_trait.trait_id(db)),
                                ));
                            if name.starts_with("+") {
                                location_links.push(LocationLink::new(
                                    buff.len(),
                                    buff.len() + name.len(),
                                    documentable_id,
                                    0,
                                ));
                                buff.push_str(&name);
                            } else {
                                buff.push_str(&format!("impl {name}: "));

                                let concrete_trait_name = concrete_trait.name(db);
                                let concrete_trait_generic_args_formatted = concrete_trait
                                    .generic_args(db)
                                    .iter()
                                    .map(|arg| extract_and_format(&arg.format(db)))
                                    .collect::<Vec<_>>()
                                    .join(", ");

                                location_links.push(LocationLink::new(
                                    buff.len(),
                                    buff.len() + concrete_trait_name.to_string(db).len(),
                                    documentable_id,
                                    0,
                                ));
                                buff.push_str(&concrete_trait_name.to_string(db));

                                if !concrete_trait_generic_args_formatted.is_empty() {
                                    let f = format!("<{concrete_trait_generic_args_formatted}>");
                                    buff.push_str(&f);
                                }
                            }
                        }
                        Err(_) => {
                            buff.push_str(&format!(
                                "{}{}",
                                name,
                                if count == 1 { "" } else { ", " }
                            ));
                        }
                    }
                }
                GenericParam::NegImpl(_) => buff.push_str(crate::documentable_formatter::MISSING),
            };
            count -= 1;
        }
        buff.push('>');
    }
    Ok((buff, location_links))
}

/// Formats and returns syntax of a documentable item.
pub fn get_syntactic_evaluation<'db>(
    item_id: DocumentableItemId<'db>,
    db: &'db dyn Database,
) -> Result<String, fmt::Error> {
    let mut buff = String::new();

    if let Some(stable_location) = item_id.stable_location(db) {
        let syntax_node = stable_location.syntax_node(db);
        if matches!(&syntax_node.green_node(db).details, green::GreenNodeDetails::Node { .. }) {
            let mut is_after_evaluation_value = false;
            for child in syntax_node.get_children(db).iter() {
                let kind = child.kind(db);
                if !matches!(kind, SyntaxKind::Trivia) {
                    if matches!(kind, SyntaxKind::TerminalSemicolon) {
                        buff.push(';');
                        return Ok(buff);
                    }
                    if is_after_evaluation_value {
                        buff.push_str(&SyntaxNode::get_text_without_all_comment_trivia(child, db));
                    };
                    if matches!(kind, SyntaxKind::TerminalEq) {
                        is_after_evaluation_value = true;
                    }
                }
            }
        };
        Ok(buff)
    } else {
        Err(fmt::Error)
    }
}

/// Formats the text of [`Visibility`] to a relevant string slice.
pub fn get_syntactic_visibility(semantic_visibility: &Visibility) -> &str {
    match semantic_visibility {
        Visibility::Public => "pub ",
        Visibility::PublicInCrate => "pub(crate) ",
        Visibility::Private => "",
    }
}

/// Formats the full paths of complex types. For example, input "Result<Error::NotFound,
/// System::Error>" results in output "Result<NotFound, Error>".
pub(crate) fn extract_and_format(input: &str) -> String {
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

/// Formats a single type path. For example, input "core::felt252" results in output "felt252".
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

/// Takes a list of [`GenericParamId`]s and formats it into a string representation used for
/// signature documentation.
pub fn format_resolver_generic_params<'db>(
    db: &'db dyn Database,
    params: Vec<GenericParamId<'db>>,
) -> String {
    if !params.is_empty() {
        format!(
            "<{}>",
            params
                .iter()
                .map(|param| {
                    if matches!(param.kind(db), GenericKind::Impl) {
                        let param_formatted = param.format(db);
                        if param_formatted.long(db).starts_with("+") {
                            param_formatted.long(db).to_string()
                        } else {
                            match db.generic_param_semantic(*param) {
                                Ok(generic_param) => match generic_param {
                                    GenericParam::Impl(generic_param_impl) => {
                                        match generic_param_impl.concrete_trait {
                                            Ok(concrete_trait) => {
                                                format!(
                                                    "impl {}: {}<{}>",
                                                    param_formatted.long(db),
                                                    concrete_trait.name(db).long(db),
                                                    concrete_trait
                                                        .generic_args(db)
                                                        .iter()
                                                        .map(|arg| arg.format(db))
                                                        .collect::<Vec<_>>()
                                                        .join(", "),
                                                )
                                            }
                                            Err(_) => param_formatted.long(db).to_string(),
                                        }
                                    }
                                    _ => param_formatted.long(db).to_string(),
                                },
                                Err(_) => param_formatted.long(db).to_string(),
                            }
                        }
                    } else {
                        param.format(db).long(db).to_string()
                    }
                })
                .join(", ")
        )
    } else {
        "".to_string()
    }
}

/// Formats and returns syntax of struct attributes.
pub fn get_struct_attributes_syntax<'db>(
    attributes: Vec<Attribute<'db>>,
    db: &'db dyn Database,
) -> Result<String, fmt::Error> {
    let mut buff = String::new();
    for attribute in attributes {
        let syntax_node = attribute.stable_ptr.lookup(db).as_syntax_node();
        for child in syntax_node.get_children(db).iter() {
            let to_text = child.get_text_without_all_comment_trivia(db);
            let cleaned_text = to_text.replace("\r\n", "").replace("\n", "");
            buff.push_str(&cleaned_text);
        }
        buff.push('\n');
    }
    Ok(buff)
}

/// Returns a relevant [`DocumentableItemId`] for [`TypeId`] if one can be retrieved.
pub fn resolve_type<'db>(
    db: &'db dyn Database,
    type_id: TypeId<'db>,
) -> Option<DocumentableItemId<'db>> {
    let intern = type_id.long(db);
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
        TypeLongId::Snapshot(type_id) => resolve_type(db, *type_id),
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
                        ModuleId::MacroCall { .. } => None,
                    },
                    Err(_) => None,
                }
            }
            InferenceId::MacroCall(_) => None,
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
        TypeLongId::FixedSizeArray { type_id, size: _ } => resolve_type(db, *type_id),
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

/// Returns a relevant [`DocumentableItemId`] for [`GenericItemId`] if one can be retrieved.
fn resolve_generic_item<'db>(
    generic_item_id: GenericItemId<'db>,
    db: &'db dyn Database,
) -> Option<DocumentableItemId<'db>> {
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
pub fn resolve_generic_module_item(
    generic_module_item_id: GenericModuleItemId<'_>,
) -> DocumentableItemId<'_> {
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
