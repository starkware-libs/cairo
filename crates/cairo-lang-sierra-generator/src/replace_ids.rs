use cairo_lang_debug::DebugWithDb;
use cairo_lang_sierra::program;
use cairo_lang_utils::extract_matches;

use crate::db::SierraGenGroup;
use crate::pre_sierra::{self, PushValue};

pub trait SierraIdReplacer {
    /// Returns a new program where all the ids are replaced.
    fn apply(
        &self,
        program: &cairo_lang_sierra::program::Program,
    ) -> cairo_lang_sierra::program::Program {
        let mut program = program.clone();
        for statement in &mut program.statements {
            if let cairo_lang_sierra::program::GenStatement::Invocation(p) = statement {
                p.libfunc_id = self.replace_libfunc_id(&p.libfunc_id);
            }
        }
        for type_declaration in &mut program.type_declarations {
            type_declaration.id = self.replace_type_id(&type_declaration.id);
            self.replace_generic_args(&mut type_declaration.long_id.generic_args);
        }
        for libfunc_declaration in &mut program.libfunc_declarations {
            libfunc_declaration.id = self.replace_libfunc_id(&libfunc_declaration.id);
            self.replace_generic_args(&mut libfunc_declaration.long_id.generic_args);
        }
        for function in &mut program.funcs {
            function.id = self.replace_function_id(&function.id);
            for param in &mut function.params {
                param.ty = self.replace_type_id(&param.ty);
            }
            for ty in &mut function.signature.ret_types {
                *ty = self.replace_type_id(ty);
            }
            for ty in &mut function.signature.param_types {
                *ty = self.replace_type_id(ty);
            }
        }
        program
    }

    // Replaces libfunc_ids
    fn replace_libfunc_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId;

    // Replace type_ids
    fn replace_type_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> cairo_lang_sierra::ids::ConcreteTypeId;

    // Replace user function ids.
    fn replace_function_id(
        &self,
        sierra_id: &cairo_lang_sierra::ids::FunctionId,
    ) -> cairo_lang_sierra::ids::FunctionId;

    fn replace_generic_args(&self, generic_args: &mut Vec<program::GenericArg>) {
        for arg in generic_args {
            match arg {
                program::GenericArg::Type(id) => {
                    *id = self.replace_type_id(id);
                }
                program::GenericArg::UserFunc(id) => {
                    *id = self.replace_function_id(id);
                }
                program::GenericArg::Libfunc(id) => {
                    *id = self.replace_libfunc_id(id);
                }
                program::GenericArg::Value(_) | program::GenericArg::UserType(_) => {}
            }
        }
    }
}

/// Replaces `cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId}` with a dummy
/// ids whose debug string is the string representing the expanded information about the id.
/// For Libfuncs and Types - that would be recursively opening their generic arguments, for
/// functions - that would be getting their original name. For example, while the original debug
/// string may be `[6]`, the resulting debug string may be:
///  - For libfuncs: `felt_const<2>` or `unbox<Box<Box<felt>>>`.
///  - For types: `felt` or `Box<Box<felt>>`.
///  - For user functions: `test::foo`.
pub struct DebugReplacer<'a> {
    pub db: &'a dyn SierraGenGroup,
}
impl SierraIdReplacer for DebugReplacer<'_> {
    fn replace_libfunc_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteLibfuncId,
    ) -> cairo_lang_sierra::ids::ConcreteLibfuncId {
        let mut long_id = self.db.lookup_intern_concrete_lib_func(id.clone());
        self.replace_generic_args(&mut long_id.generic_args);
        long_id.to_string().into()
    }

    fn replace_type_id(
        &self,
        id: &cairo_lang_sierra::ids::ConcreteTypeId,
    ) -> cairo_lang_sierra::ids::ConcreteTypeId {
        let mut long_id = self.db.lookup_intern_concrete_type(id.clone());
        self.replace_generic_args(&mut long_id.generic_args);
        if long_id.generic_id == "Enum".into() || long_id.generic_id == "Struct".into() {
            long_id.generic_id =
                extract_matches!(&long_id.generic_args[0], program::GenericArg::UserType)
                    .to_string()
                    .into();
            if long_id.generic_id == "Tuple".into() {
                long_id.generic_args = long_id.generic_args.into_iter().skip(1).collect();
                if long_id.generic_args.is_empty() {
                    long_id.generic_id = "Unit".into();
                }
            } else {
                long_id.generic_args.clear();
            }
        }
        long_id.to_string().into()
    }

    /// Helper for [replace_sierra_ids] and [replace_sierra_ids_in_program] replacing function ids.
    fn replace_function_id(
        &self,
        sierra_id: &cairo_lang_sierra::ids::FunctionId,
    ) -> cairo_lang_sierra::ids::FunctionId {
        let semantic_id = self.db.lookup_intern_sierra_function(sierra_id.clone());
        format!("{:?}", self.db.lookup_intern_function(semantic_id).debug(self.db.upcast())).into()
    }
}

pub fn replace_sierra_ids(
    db: &dyn SierraGenGroup,
    statement: &pre_sierra::Statement,
) -> pre_sierra::Statement {
    let replacer = DebugReplacer { db };
    match statement {
        pre_sierra::Statement::Sierra(cairo_lang_sierra::program::GenStatement::Invocation(p)) => {
            pre_sierra::Statement::Sierra(cairo_lang_sierra::program::GenStatement::Invocation(
                cairo_lang_sierra::program::GenInvocation {
                    libfunc_id: replacer.replace_libfunc_id(&p.libfunc_id),
                    ..p.clone()
                },
            ))
        }
        pre_sierra::Statement::PushValues(values) => pre_sierra::Statement::PushValues(
            values
                .iter()
                .map(|value| PushValue { ty: replacer.replace_type_id(&value.ty), ..value.clone() })
                .collect(),
        ),
        _ => statement.clone(),
    }
}

/// Replaces `cairo_lang_sierra::ids::{ConcreteLibfuncId, ConcreteTypeId, FunctionId}` with a dummy
/// ids whose debug string is the string representing the expanded information about the id.
/// For Libfuncs and Types - that would be recursively opening their generic arguments, for
/// functions - that would be getting their original name. For example, while the original debug
/// string may be `[6]`, the resulting debug string may be:
///  - For libfuncs: `felt_const<2>` or `unbox<Box<Box<felt>>>`.
///  - For types: `felt` or `Box<Box<felt>>`.
///  - For user functions: `test::foo`.
///
/// Similar to [replace_sierra_ids] except that it acts on [cairo_lang_sierra::program::Program].
pub fn replace_sierra_ids_in_program(
    db: &dyn SierraGenGroup,
    program: &cairo_lang_sierra::program::Program,
) -> cairo_lang_sierra::program::Program {
    DebugReplacer { db }.apply(program)
}
