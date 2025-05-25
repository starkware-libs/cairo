use std::fmt;

use cairo_lang_utils::unordered_hash_map::UnorderedHashMap;
use cairo_lang_utils::unordered_hash_set::UnorderedHashSet;
use cairo_lang_utils::write_comma_separated;

use crate::ids::{
    ConcreteLibfuncId, ConcreteTypeId, FunctionId, GenericLibfuncId, GenericTypeId, UserTypeId,
    VarId,
};
use crate::labeled_statement::replace_statement_id;
use crate::program::{
    ConcreteLibfuncLongId, ConcreteTypeLongId, GenBranchInfo, GenBranchTarget, GenFunction,
    GenInvocation, GenStatement, GenericArg, LibfuncDeclaration, Param, Program, StatementIdx,
    TypeDeclaration,
};

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for declaration in &self.type_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        for declaration in &self.libfunc_declarations {
            writeln!(f, "{declaration};")?;
        }
        writeln!(f)?;
        // The labels of function starts.
        let funcs_labels = UnorderedHashMap::<usize, String>::from_iter(
            self.funcs.iter().enumerate().map(|(i, f)| (f.entry_point.0, format!("F{i}"))),
        );
        // The offsets of branch targets.
        let mut block_offsets = UnorderedHashSet::<usize>::default();
        for s in &self.statements {
            replace_statement_id(s.clone(), |idx| {
                block_offsets.insert(idx.0);
            });
        }
        // All labels including inner function labels.
        let mut labels = funcs_labels.clone();
        // Starting as `NONE` for support of invalid Sierra code.
        let mut function_label = "NONE".to_string();
        // Assuming function code is contiguous - this is the index for same function labels.
        let mut inner_idx = 0;
        for i in 0..self.statements.len() {
            if let Some(label) = funcs_labels.get(&i) {
                function_label = label.clone();
                inner_idx = 0;
            } else if block_offsets.contains(&i) {
                labels.insert(i, format!("{function_label}_B{inner_idx}"));
                inner_idx += 1;
            }
        }

        for (i, statement) in self.statements.iter().enumerate() {
            if let Some(label) = labels.get(&i) {
                writeln!(f, "{label}:")?;
            }
            let with_labels = replace_statement_id(statement.clone(), |idx| labels[&idx.0].clone());
            writeln!(f, "{with_labels};")?;
        }
        writeln!(f)?;
        for func in &self.funcs {
            let with_label = GenFunction {
                id: func.id.clone(),
                signature: func.signature.clone(),
                params: func.params.clone(),
                entry_point: labels[&func.entry_point.0].clone(),
            };
            writeln!(f, "{with_label};",)?;
        }
        Ok(())
    }
}

impl fmt::Display for TypeDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let TypeDeclaration { id, long_id, declared_type_info } = self;
        write!(f, "type {id} = {long_id}")?;
        if let Some(info) = declared_type_info {
            write!(
                f,
                " [storable: {:?}, drop: {:?}, dup: {:?}, zero_sized: {:?}]",
                info.storable, info.droppable, info.duplicatable, info.zero_sized
            )?;
        }
        Ok(())
    }
}

impl fmt::Display for ConcreteTypeLongId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.generic_id)?;
        write_template_args(f, &self.generic_args)
    }
}

impl fmt::Display for LibfuncDeclaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "libfunc {} = {}", self.id, self.long_id)
    }
}

impl fmt::Display for ConcreteLibfuncLongId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.generic_id)?;
        write_template_args(f, &self.generic_args)
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenFunction<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}@{}(", self.id, self.entry_point)?;
        write_comma_separated(f, &self.params)?;
        write!(f, ") -> (")?;
        write_comma_separated(f, &self.signature.ret_types)?;
        write!(f, ")")
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.id, self.ty)
    }
}

macro_rules! display_generic_identity {
    ($type_name:tt) => {
        impl fmt::Display for $type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", self.0)
            }
        }
    };
}

display_generic_identity!(GenericLibfuncId);
display_generic_identity!(GenericTypeId);

macro_rules! display_identity {
    ($type_name:tt) => {
        impl fmt::Display for $type_name {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match &self.debug_name {
                    Some(name) => write!(f, "{name}"),
                    None => write!(f, "[{}]", self.id),
                }
            }
        }
    };
}

display_identity!(ConcreteLibfuncId);
display_identity!(FunctionId);
display_identity!(UserTypeId);
display_identity!(VarId);
display_identity!(ConcreteTypeId);

impl fmt::Display for GenericArg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenericArg::Type(id) => write!(f, "{id}"),
            GenericArg::UserType(id) => write!(f, "ut@{id}"),
            GenericArg::Value(v) => write!(f, "{v}"),
            GenericArg::UserFunc(id) => write!(f, "user@{id}"),
            GenericArg::Libfunc(id) => write!(f, "lib@{id}"),
        }
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenStatement<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenStatement::Invocation(invocation) => write!(f, "{invocation}"),
            GenStatement::Return(ids) => {
                write!(f, "return(")?;
                write_comma_separated(f, ids)?;
                write!(f, ")")
            }
        }
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenInvocation<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.libfunc_id)?;
        write_comma_separated(f, &self.args)?;
        if let [GenBranchInfo { target: GenBranchTarget::Fallthrough, results }] =
            &self.branches[..]
        {
            write!(f, ") -> (")?;
            write_comma_separated(f, results)?;
            write!(f, ")")
        } else {
            write!(f, ") {{ ")?;
            self.branches.iter().try_for_each(|branch_info| write!(f, "{branch_info} "))?;
            write!(f, "}}")
        }
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenBranchInfo<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.target)?;
        write_comma_separated(f, &self.results)?;
        write!(f, ")")
    }
}

impl<StatementId: fmt::Display> fmt::Display for GenBranchTarget<StatementId> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenBranchTarget::Fallthrough => write!(f, "fallthrough"),
            GenBranchTarget::Statement(id) => write!(f, "{id}"),
        }
    }
}

impl fmt::Display for StatementIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

fn write_template_args(f: &mut fmt::Formatter<'_>, args: &[GenericArg]) -> fmt::Result {
    if args.is_empty() {
        Ok(())
    } else {
        write!(f, "<")?;
        write_comma_separated(f, args)?;
        write!(f, ">")
    }
}
