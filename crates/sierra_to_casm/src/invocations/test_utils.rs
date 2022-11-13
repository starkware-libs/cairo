/// Creates a FeltOperator from a token operator.
#[macro_export]
macro_rules! cell_expr_operator {
    (+) => {
        sierra::extensions::felt::FeltOperator::Add
    };
    (-) => {
        sierra::extensions::felt::FeltOperator::Sub
    };
    (*) => {
        sierra::extensions::felt::FeltOperator::Mul
    };
    (/) => {
        sierra::extensions::felt::FeltOperator::Div
    };
}

/// Adds a cell expression to a ReferenceExpression cells vector.
#[macro_export]
macro_rules! ref_expr_extend {
    ($cells:ident) => {};
    ($cells:ident, [$a:ident $($op:tt $offset:expr)?] $operator:tt $b:tt $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::BinOp($crate::references::BinOpExpression {
                op: $crate::cell_expr_operator!($operator),
                a: casm::deref!([$a $($op $offset)?]),
                b: casm::deref_or_immediate!($b),
        }));
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, [$a:ident $($op:tt $offset:expr)?] $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::Deref(casm::deref!([$a $($op $offset)?]))
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, [[$a:ident $($op:tt $offset:expr)?]] $(, $tok:tt)*) => {
        $cells.push(
            $crate::references::CellExpression::DoubleDeref(casm::deref!([$a $($op $offset)?]))
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, & $a:ident $($op:tt $offset:expr)? $(, $tok:tt)*) => {
        $cells.push($crate::references::CellExpression::IntoSingleCellRef(
            casm::deref!([$a $($op $offset)?])
        ));
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, $a:expr $(, $tok:tt)*) => {
        cells.push(
            $crate::references::CellExpression::Immediate(num_bigint::BigInt::from($a))
        );
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
    ($cells:ident, _ $(, $tok:tt)*) => {
        cells.push($crate::references::CellExpression::Padding);
        $crate::ref_expr_extend!($cells $(, $tok)*)
    };
}

// TODO(orizi): Make this flexible enough to be used in outside of testing.
/// Creates a reference expression.
#[macro_export]
macro_rules! ref_expr {
    {$($tok:tt)*} => {
        {
            let mut cells = Vec::new();
            #[allow(clippy::vec_init_then_push)]
            {
                $crate::ref_expr_extend!(cells, $($tok)*);
            }
            ReferenceExpression { cells }
        }
    }
}
