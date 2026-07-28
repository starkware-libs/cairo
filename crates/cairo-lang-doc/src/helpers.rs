use std::fmt;

use cairo_lang_defs::ids::TraitItemId::Function;
use cairo_lang_defs::ids::{
    GenericImplItemId, GenericItemId, GenericKind, GenericModuleItemId, GenericParamId,
    GenericTraitItemId, ImplItemId, LookupItemId, ModuleId, ModuleItemId, NamedLanguageElementId,
    TraitItemId,
};
use cairo_lang_semantic::expr::inference::InferenceId;
use cairo_lang_semantic::items::constant::{ConstValue, ConstValueId};
use cairo_lang_semantic::items::functions::GenericFunctionId;
use cairo_lang_semantic::items::generics::{
    GenericArgumentId, GenericParamImpl, GenericParamSemantic,
};
use cairo_lang_semantic::items::imp::{ImplId, ImplLongId};
use cairo_lang_semantic::items::trt::ConcreteTraitId;
use cairo_lang_semantic::items::us::UseSemantic;
use cairo_lang_semantic::items::visibility::Visibility;
use cairo_lang_semantic::types::ImplTypeId;
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
        buff.push('<');
        for (i, param) in generic_params.iter().enumerate() {
            if i > 0 {
                buff.push_str(", ");
            }
            match param {
                GenericParam::Type(param_type) => {
                    buff.push_str(&extract_and_format(param_type.id.format(db).long(db)));
                }
                GenericParam::Const(param_const) => {
                    buff.push_str("const ");
                    buff.push_str(&extract_and_format(param_const.id.format(db).long(db)));
                }
                GenericParam::Impl(param_impl) => match param_impl.concrete_trait {
                    Ok(concrete_trait) => {
                        let documentable_id = DocumentableItemId::from(LookupItemId::ModuleItem(
                            ModuleItemId::Trait(concrete_trait.trait_id(db)),
                        ));
                        if let Some(name) = param_impl.id.name(db) {
                            buff.push_str(&format!("impl {}: ", name.long(db)));

                            let concrete_trait_name = concrete_trait.name(db).long(db);
                            let concrete_trait_generic_args = concrete_trait
                                .generic_args(db)
                                .iter()
                                .map(|arg| format_generic_arg(db, *arg))
                                .join(", ");

                            location_links.push(LocationLink::new(
                                buff.len(),
                                buff.len() + concrete_trait_name.len(),
                                documentable_id,
                                0,
                            ));
                            buff.push_str(concrete_trait_name);

                            if !concrete_trait_generic_args.is_empty() {
                                buff.push_str(&format!("<{concrete_trait_generic_args}>"));
                            }
                        } else {
                            let bound =
                                format_anonymous_impl_param(db, "+", param_impl, concrete_trait);
                            location_links.push(LocationLink::new(
                                buff.len(),
                                buff.len() + bound.len(),
                                documentable_id,
                                0,
                            ));
                            buff.push_str(&bound);
                        }
                    }
                    Err(_) => buff.push_str(param_impl.id.format(db).long(db)),
                },
                GenericParam::NegImpl(param_neg_impl) => match param_neg_impl.concrete_trait {
                    Ok(concrete_trait) => {
                        let bound =
                            format_anonymous_impl_param(db, "-", param_neg_impl, concrete_trait);
                        location_links.push(LocationLink::new(
                            buff.len(),
                            buff.len() + bound.len(),
                            DocumentableItemId::from(LookupItemId::ModuleItem(
                                ModuleItemId::Trait(concrete_trait.trait_id(db)),
                            )),
                            0,
                        ));
                        buff.push_str(&bound);
                    }
                    Err(_) => buff.push_str(param_neg_impl.id.format(db).long(db)),
                },
            }
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
fn extract_and_format(input: &str) -> String {
    let delimiters = [',', '<', '>', '(', ')', '[', ']', '@'];
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

/// Formats an anonymous impl param, as in `+Shape<S::Item>`, `-Drop<T>` or
/// `+FnOnce<F, (T,)>[Output: U]`.
///
/// An anonymous param is spelled by the bound it imposes, so it keeps the `+` or `-` the source
/// writes it with. This is what separates it from an impl *argument*, which is spelled `_`.
///
/// The bound is built from the param's semantic data rather than by shortening its syntactic text.
/// The text spells the trait as written, which may be a full path, and shortening it takes the `::`
/// of an associated type in the trait's arguments for a path separator and drops the qualifier.
fn format_anonymous_impl_param<'db>(
    db: &'db dyn Database,
    sign: &str,
    param_impl: &GenericParamImpl<'db>,
    concrete_trait: ConcreteTraitId<'db>,
) -> String {
    let bound = format!("{sign}{}", format_concrete_trait(db, concrete_trait, false));
    if param_impl.type_constraints.is_empty() {
        return bound;
    }
    let constraints = param_impl
        .type_constraints
        .iter()
        .map(|(trait_type, type_id)| {
            format!("{}: {}", trait_type.name(db).long(db), format_type(db, *type_id))
        })
        .join(", ");
    format!("{bound}[{constraints}]")
}

/// Formats a [`TypeId`] for signature documentation.
///
/// Full paths are shortened to the item's name (for example `core::felt252` results in `felt252`),
/// except for associated types, which keep the impl qualifier they are accessed through (for
/// example `Self::Item`), as an associated type name on its own does not identify a type.
pub(crate) fn format_type<'db>(db: &'db dyn Database, type_id: TypeId<'db>) -> String {
    match type_id.long(db) {
        TypeLongId::ImplType(impl_type_id) => format_impl_type(db, impl_type_id),
        TypeLongId::Snapshot(inner_type_id) => format!("@{}", format_type(db, *inner_type_id)),
        TypeLongId::Tuple(inner_type_ids) => {
            let inner = inner_type_ids.iter().map(|ty| format_type(db, *ty)).join(", ");
            // A single element tuple requires a trailing comma, to tell it apart from a
            // parenthesized type.
            if inner_type_ids.len() == 1 { format!("({inner},)") } else { format!("({inner})") }
        }
        TypeLongId::FixedSizeArray { type_id, size } => {
            format!("[{}; {}]", format_type(db, *type_id), format_const_value(db, *size))
        }
        TypeLongId::Concrete(concrete_type_id) => {
            let name = concrete_type_id.generic_type(db).name(db).long(db);
            let generic_args = concrete_type_id.generic_args(db);
            if generic_args.is_empty() {
                name.to_string()
            } else {
                let generic_args =
                    generic_args.iter().map(|arg| format_generic_arg(db, *arg)).join(", ");
                format!("{name}<{generic_args}>")
            }
        }
        // None of these can contain a nested type, so shortening the flattened text is enough.
        TypeLongId::GenericParameter(_)
        | TypeLongId::Var(_)
        | TypeLongId::NumericLiteral(_)
        | TypeLongId::Coupon(_)
        | TypeLongId::Closure(_)
        | TypeLongId::Missing(_) => extract_and_format(&type_id.format(db)),
    }
}

/// Formats a [`GenericArgumentId`] for signature documentation.
pub(crate) fn format_generic_arg<'db>(
    db: &'db dyn Database,
    generic_arg: GenericArgumentId<'db>,
) -> String {
    match generic_arg {
        GenericArgumentId::Type(type_id) => format_type(db, type_id),
        GenericArgumentId::Constant(value) => format_const_value(db, value),
        // An impl argument is written when it has a name - the source can pass one explicitly, as
        // in `bar::<MyImpl>()` - and `_` when it is inferred and has none to write. An impl *param*
        // is a bound instead, and keeps its `+` or `-`.
        GenericArgumentId::Impl(impl_id) => {
            format_impl_name(db, impl_id).unwrap_or_else(|| "_".to_string())
        }
        // A negative impl is never written in argument position.
        GenericArgumentId::NegImpl(_) => "_".to_string(),
    }
}

/// Formats a [`ConcreteTraitId`] for signature documentation, as its name followed by its generic
/// arguments - for example `AssocTrait<T>`.
///
/// `as_path_qualifier` must be set when the result is followed by `::`, as in
/// `AssocTrait::<T>::Item`. A path segment carrying generic arguments has to be spelled with a
/// turbofish there, since the parser ends a path at a segment followed by `<` with no `::` - so
/// `AssocTrait<T>::Item` does not parse at all.
///
/// [`get_generic_params`] deliberately does not use this: it records a location link spanning the
/// trait name alone, so it needs the name and the arguments separately.
fn format_concrete_trait<'db>(
    db: &'db dyn Database,
    concrete_trait: ConcreteTraitId<'db>,
    as_path_qualifier: bool,
) -> String {
    let name = concrete_trait.name(db).long(db);
    let generic_args = concrete_trait.generic_args(db);
    if generic_args.is_empty() {
        return name.to_string();
    }
    let generic_args = generic_args.iter().map(|arg| format_generic_arg(db, *arg)).format(", ");
    let turbofish = if as_path_qualifier { "::" } else { "" };
    format!("{name}{turbofish}<{generic_args}>")
}

/// Formats the name an [`ImplId`] is written with - for example `Self`, `CircleShape` or
/// `S::Inner`. Returns [`None`] for an impl the source cannot name: an anonymous impl param, an
/// inference variable, and a generated impl.
///
/// The name comes from the impl's semantic data rather than from shortening [`ImplLongId::name`],
/// which is a full path for some variants and a debug representation for others. Shortening the
/// whole `<impl>::<item>` string is not an option either - it would take the `::` for a path
/// separator and drop the qualifier, which is the very bug this avoids.
fn format_impl_name<'db>(db: &'db dyn Database, impl_id: ImplId<'db>) -> Option<String> {
    match impl_id.long(db) {
        // The trait's own impl is written `Self`.
        ImplLongId::SelfImpl(_) => Some("Self".to_string()),
        // A concrete impl, or a named impl param, is written with its own name, which
        // [`ImplLongId::name`] gives as the bare item name.
        ImplLongId::Concrete(_) => Some(impl_id.long(db).name(db)),
        ImplLongId::GenericParameter(param) if param.name(db).is_some() => {
            Some(impl_id.long(db).name(db))
        }
        // An impl item of another impl is written through the impl holding it, so it is nameable
        // only as long as that one is.
        ImplLongId::ImplImpl(impl_impl) => {
            let outer = format_impl_name(db, impl_impl.impl_id())?;
            Some(format!("{outer}::{}", impl_impl.trait_impl_id().name(db).long(db)))
        }
        ImplLongId::GenericParameter(_) | ImplLongId::ImplVar(_) | ImplLongId::GeneratedImpl(_) => {
            None
        }
    }
}

/// Formats an [`ImplId`] as the qualifier an associated item is accessed through.
///
/// An impl with no name of its own still has to be qualified with something, or the associated
/// item's name would not identify it. Its trait is used instead, as in `AssocTrait::<T>::Item`,
/// which is how the source reaches the item too.
fn format_impl<'db>(db: &'db dyn Database, impl_id: ImplId<'db>) -> String {
    format_impl_name(db, impl_id).unwrap_or_else(|| {
        impl_id
            .concrete_trait(db)
            .map(|concrete_trait| format_concrete_trait(db, concrete_trait, true))
            .unwrap_or_else(|_| crate::documentable_formatter::MISSING.to_string())
    })
}

/// Formats an associated type, qualified by the impl it is accessed through. For example, input
/// `core::iter::traits::iterator::Iterator::<T>::Item` results in output `Self::Item`.
fn format_impl_type<'db>(db: &'db dyn Database, impl_type_id: &ImplTypeId<'db>) -> String {
    format!("{}::{}", format_impl(db, impl_type_id.impl_id()), impl_type_id.ty().name(db).long(db))
}

/// Formats a [`ConstValueId`] for signature documentation, as reached through a fixed size array's
/// length.
///
/// An associated const keeps the impl qualifier it is accessed through, for the same reason an
/// associated type does - `SIZE` on its own does not identify a const. A length is a `usize`, so
/// the remaining spellings a value can take there are a literal, a const generic param, an
/// inference variable and a missing value, all of which are already short.
fn format_const_value<'db>(db: &'db dyn Database, value: ConstValueId<'db>) -> String {
    match value.long(db) {
        ConstValue::ImplConstant(impl_constant) => format!(
            "{}::{}",
            format_impl(db, impl_constant.impl_id()),
            impl_constant.trait_constant_id().name(db).long(db)
        ),
        _ => value.format(db),
    }
}

/// Formats a single type path. For example, input "core::felt252" results in output "felt252".
fn format_final_part(slice: &str) -> String {
    let mut parts = slice.rsplit("::");
    let result = if let Some(last) = parts.next().map(str::trim)
        && !last.is_empty()
    {
        last
    } else if let Some(before_last) = parts.next() {
        before_last.trim()
    } else {
        return slice.to_string();
    };
    if slice.starts_with(" ") { format!(" {result}") } else { result.to_string() }
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
                    // Only an impl param has a bound to spell; a type or const param is its name.
                    let semantic = match param.kind(db) {
                        GenericKind::Impl | GenericKind::NegImpl => {
                            db.generic_param_semantic(*param).ok()
                        }
                        GenericKind::Type | GenericKind::Const => None,
                    };
                    match semantic {
                        Some(GenericParam::Impl(param_impl)) => {
                            match (param_impl.concrete_trait, param.name(db)) {
                                (Ok(concrete_trait), Some(name)) => format!(
                                    "impl {}: {}",
                                    name.long(db),
                                    format_concrete_trait(db, concrete_trait, false),
                                ),
                                (Ok(concrete_trait), None) => format_anonymous_impl_param(
                                    db,
                                    "+",
                                    &param_impl,
                                    concrete_trait,
                                ),
                                (Err(_), _) => param.format(db).long(db).to_string(),
                            }
                        }
                        Some(GenericParam::NegImpl(param_impl)) => {
                            match param_impl.concrete_trait {
                                Ok(concrete_trait) => format_anonymous_impl_param(
                                    db,
                                    "-",
                                    &param_impl,
                                    concrete_trait,
                                ),
                                Err(_) => param.format(db).long(db).to_string(),
                            }
                        }
                        _ => param.format(db).long(db).to_string(),
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

/// Returns the relevant [`DocumentableItemId`] for [`TypeId`] if one can be retrieved.
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
        TypeLongId::NumericLiteral(_) => None,
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

/// Returns the relevant [`DocumentableItemId`] for [`GenericItemId`] if one can be retrieved.
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
