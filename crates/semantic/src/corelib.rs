use defs::ids::{GenericFunctionId, GenericTypeId, ModuleId};
use filesystem::ids::CrateLongId;
use syntax::token::TokenKind;
use utils::OptionFrom;

use crate::db::SemanticGroup;
use crate::diagnostic::SemanticDiagnosticKind;
use crate::{semantic, TypeId};

pub fn core_module(db: &dyn SemanticGroup) -> ModuleId {
    let core_crate = db.intern_crate(CrateLongId("core".into()));
    ModuleId::CrateRoot(core_crate)
}

pub fn core_felt_ty(db: &dyn SemanticGroup) -> TypeId {
    let core_module = db.core_module();
    // This should not fail if the corelib is present.
    let generic_type = db
        .module_item_by_name(core_module, "felt".into())
        .and_then(GenericTypeId::option_from)
        .unwrap();
    db.intern_type(semantic::TypeLongId::Concrete(semantic::ConcreteType {
        generic_type,
        generic_args: vec![],
    }))
}

pub fn unit_ty(db: &dyn SemanticGroup) -> TypeId {
    db.intern_type(semantic::TypeLongId::Tuple(vec![]))
}

pub fn core_binary_operator(
    db: &dyn SemanticGroup,
    operator_kind: TokenKind,
) -> Result<GenericFunctionId, SemanticDiagnosticKind> {
    let core_module = db.core_module();
    let function_name = match operator_kind {
        TokenKind::Plus => "felt_add",
        TokenKind::Minus => "felt_sub",
        TokenKind::Mul => "felt_mul",
        TokenKind::Div => "felt_div",
        TokenKind::EqEq => "felt_eq",
        TokenKind::AndAnd => "bool_and",
        TokenKind::OrOr => "bool_or",
        TokenKind::Not => "bool_not",
        TokenKind::LE => "felt_le",
        _ => return Err(SemanticDiagnosticKind::UnknownBinaryOperator),
    };
    let generic_function = db
        .module_item_by_name(core_module, function_name.into())
        .and_then(GenericFunctionId::option_from)
        .expect("Operator function not found in core lib.");
    Ok(generic_function)
}
