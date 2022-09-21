#[cfg(test)]
#[path = "resolve_item_test.rs"]
mod test;

use defs::diagnostic_utils::StableLocation;
use defs::ids::{GenericFunctionId, GenericTypeId, ModuleId, ModuleItemId, VariantId};
use diagnostics::Diagnostics;
use filesystem::ids::CrateLongId;
use syntax::node::ast::{self};
use syntax::node::helpers::GetIdentifier;
use syntax::node::Terminal;

use crate::corelib::core_module;
use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::items::types::resolve_type;
use crate::{
    ConcreteFunction, ConcreteType, FunctionId, FunctionLongId, GenericArgumentId,
    SemanticDiagnostic, TypeId, TypeLongId,
};

pub enum ResolvedItem {
    Module(ModuleId),
    Function(FunctionId),
    Type(TypeId),
    Variant(TypeId, VariantId),
}
impl TryFrom<ResolvedItem> for FunctionId {
    type Error = SemanticDiagnosticKind;
    fn try_from(value: ResolvedItem) -> Result<Self, Self::Error> {
        if let ResolvedItem::Function(res) = value {
            Ok(res)
        } else {
            Err(SemanticDiagnosticKind::UnknownFunction)
        }
    }
}
impl TryFrom<ResolvedItem> for TypeId {
    type Error = SemanticDiagnosticKind;
    fn try_from(value: ResolvedItem) -> Result<Self, Self::Error> {
        if let ResolvedItem::Type(res) = value {
            Ok(res)
        } else {
            Err(SemanticDiagnosticKind::UnknownType)
        }
    }
}

pub fn resolve_item(
    db: &dyn SemanticGroup,
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    current_module_id: ModuleId,
    path: &ast::ExprPath,
) -> Option<ResolvedItem> {
    let syntax_db = db.upcast();
    let elements_vec = path.elements(syntax_db);
    let mut elements = elements_vec.iter().peekable();

    let base_module_id = determine_base_module(db, &mut elements, current_module_id);
    let mut item = ResolvedItem::Module(base_module_id);

    // Follow modules.
    for segment in elements {
        let ident = segment.identifier(syntax_db);
        let stable_location = StableLocation::from_ast(current_module_id, segment);
        let generic_args = if let ast::PathSegment::WithGenericArgs(generic_segment) = segment {
            generic_segment
                .generic_args(syntax_db)
                .generic_args(syntax_db)
                .elements(syntax_db)
                .iter()
                .map(|generic_arg_syntax| {
                    let ty = resolve_type(diagnostics, db, current_module_id, generic_arg_syntax);
                    GenericArgumentId::Type(ty)
                })
                .collect()
        } else {
            vec![]
        };
        match item {
            ResolvedItem::Module(module_id) => {
                let module_item =
                    if let Some(module_item) = db.module_item_by_name(module_id, ident) {
                        module_item
                    } else {
                        diagnostics.add(SemanticDiagnostic {
                            stable_location,
                            kind: SemanticDiagnosticKind::UnknownItem,
                        });
                        return None;
                    };
                item = match module_item {
                    ModuleItemId::Submodule(id) => {
                        check_no_generics(diagnostics, module_id, segment);
                        ResolvedItem::Module(ModuleId::Submodule(id))
                    }
                    ModuleItemId::Use(_) => todo!("Follow uses."),
                    ModuleItemId::FreeFunction(id) => ResolvedItem::Function(specialize_function(
                        db,
                        diagnostics,
                        stable_location,
                        GenericFunctionId::Free(id),
                        generic_args,
                    )),
                    ModuleItemId::ExternFunction(id) => {
                        ResolvedItem::Function(specialize_function(
                            db,
                            diagnostics,
                            stable_location,
                            GenericFunctionId::Extern(id),
                            generic_args,
                        ))
                    }
                    ModuleItemId::Struct(id) => ResolvedItem::Type(specialize_type(
                        db,
                        diagnostics,
                        stable_location,
                        GenericTypeId::Struct(id),
                        generic_args,
                    )),
                    ModuleItemId::Enum(id) => ResolvedItem::Type(specialize_type(
                        db,
                        diagnostics,
                        stable_location,
                        GenericTypeId::Enum(id),
                        generic_args,
                    )),
                    ModuleItemId::ExternType(id) => ResolvedItem::Type(specialize_type(
                        db,
                        diagnostics,
                        stable_location,
                        GenericTypeId::Extern(id),
                        generic_args,
                    )),
                };
                continue;
            }
            ResolvedItem::Type(ty) => {
                if let TypeLongId::Concrete(ConcreteType {
                    generic_type: GenericTypeId::Enum(_enum_id),
                    generic_args: _,
                }) = db.lookup_intern_type(ty)
                {
                    // TODO(spapini): Variant.
                    return None;
                }
            }
            ResolvedItem::Function(_) | ResolvedItem::Variant(_, _) => {}
        };
        diagnostics
            .add(SemanticDiagnostic { stable_location, kind: SemanticDiagnosticKind::UnknownItem });
        return None;
    }
    Some(item)
}

fn check_no_generics(
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    module_id: ModuleId,
    segment: &syntax::node::ast::PathSegment,
) {
    if let ast::PathSegment::WithGenericArgs(generics) = segment {
        diagnostics.add(SemanticDiagnostic {
            stable_location: StableLocation::from_ast(module_id, generics),
            kind: SemanticDiagnosticKind::UnexpectedGenerics,
        });
    }
}

/// Determines the base module for the path resolving.
fn determine_base_module(
    db: &dyn SemanticGroup,
    elements: &mut std::iter::Peekable<std::slice::Iter<'_, syntax::node::ast::PathSegment>>,
    current_module_id: ModuleId,
) -> ModuleId {
    let syntax_db = db.upcast();

    // If the first segment is not a simple segment (i.e. it had generics), use current module
    // as a base module.
    let simple_segment = match elements.peek() {
        Some(syntax::node::ast::PathSegment::Simple(segment)) => segment,
        _ => {
            return current_module_id;
        }
    };
    let ident = simple_segment.ident(syntax_db).text(syntax_db);

    // If an item with this name is found inside the current module, use the current module.
    if db.module_item_by_name(current_module_id, ident.clone()).is_some() {
        return current_module_id;
    }

    // If a crate with the same in is found, use its root module as the base module.
    let crate_id = db.intern_crate(CrateLongId(ident));
    if db.crates().iter().any(|c| *c == crate_id) {
        // Consume this segment.
        elements.next();
        return ModuleId::CrateRoot(crate_id);
    }

    // Last resort, use the `core` crate root module as the base module.
    core_module(db)
}

/// Tries to specializes a generic function.
fn specialize_function(
    db: &dyn SemanticGroup,
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    stable_location: StableLocation,
    generic_function: GenericFunctionId,
    mut generic_args: Vec<GenericArgumentId>,
) -> FunctionId {
    let signature = if let Some(signature) = db.generic_function_signature(generic_function) {
        signature
    } else {
        diagnostics.add(SemanticDiagnostic {
            stable_location,
            kind: SemanticDiagnosticKind::UnknownFunction,
        });
        return FunctionId::missing(db);
    };

    if generic_args.len() != signature.generic_params.len() {
        diagnostics.add(SemanticDiagnostic {
            stable_location,
            kind: SemanticDiagnosticKind::WrongNumberOfGenericArguments {
                expected: signature.generic_params.len(),
                actual: generic_args.len(),
            },
        });
        generic_args
            .resize(signature.generic_params.len(), GenericArgumentId::Type(TypeId::missing(db)));
    }

    db.intern_function(FunctionLongId::Concrete(ConcreteFunction {
        generic_function,
        generic_args,
        return_type: signature.return_type,
    }))
}

fn specialize_type(
    db: &dyn SemanticGroup,
    diagnostics: &mut Diagnostics<SemanticDiagnostic>,
    stable_location: StableLocation,
    generic_type: GenericTypeId,
    mut generic_args: Vec<GenericArgumentId>,
) -> TypeId {
    let generic_params = if let Some(generic_params) = db.generic_type_generic_params(generic_type)
    {
        generic_params
    } else {
        diagnostics
            .add(SemanticDiagnostic { stable_location, kind: SemanticDiagnosticKind::UnknownType });
        return TypeId::missing(db);
    };

    if generic_args.len() != generic_params.len() {
        diagnostics.add(SemanticDiagnostic {
            stable_location,
            kind: SemanticDiagnosticKind::WrongNumberOfGenericArguments {
                expected: generic_params.len(),
                actual: generic_args.len(),
            },
        });
        generic_args.resize(generic_params.len(), GenericArgumentId::Type(TypeId::missing(db)));
    }

    db.intern_type(TypeLongId::Concrete(ConcreteType { generic_type, generic_args }))
}
