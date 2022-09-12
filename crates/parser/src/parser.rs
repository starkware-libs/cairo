#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

use std::mem;

use diagnostics::Diagnostics;
use filesystem::ids::FileId;
use filesystem::span::{TextOffset, TextSpan};
use syntax::node::ast::*;
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, Token, TypedSyntaxNode};

use crate::diagnostic::ParserDiagnosticKind;
use crate::lexer::{Lexer, LexerTerminal};
use crate::operators::{get_binary_operator_precedence, get_unary_operator_precedence};
use crate::recovery::is_of_kind;
use crate::ParserDiagnostic;

// TODO(yuval): add diagnostics.

pub struct Parser<'a> {
    db: &'a dyn SyntaxGroup,
    file_id: FileId,
    lexer: Lexer<'a>,
    /// The next terminal to handle.
    next_terminal: LexerTerminal,
    /// A vector of pending trivia to be added as leading trivia to the next valid terminal.
    pending_trivia: Vec<TriviumGreen>,
    /// The current offset, excluding the current terminal.
    offset: u32,
    /// The width of the current terminal being handled.
    current_width: u32,
    diagnostics: &'a mut Diagnostics<ParserDiagnostic>,
}

// try_parse_<something>: returns a green ID with a kind that represents 'something' or None if
// 'something' can't be parsed.
// Used when something may or may not be there and we can act differently according to each case.
//
// parse_option_<something>: returns a green ID with a kind that represents 'something'. If
// 'something' can't be parsed, returns a green ID with the relevant empty kind. Used for an
// optional child. Always returns some green ID.
//
// parse_<something>: returns a green ID with a kind that represents 'something'. If
// 'something' can't be parsed, returns a green ID with the relevant missing kind. Used when we
// expect 'something' to be there. Always returns some green ID.
//
// expect_<something>: similar to parse_<something>, but assumes the current token is as expected.
// Therefore, it always returns a GreenId of a node with a kind that represents 'something' and
// never a missing kind.
// Should only be called after checking the current token.

const MAX_PRECEDENCE: usize = 10;
impl<'a> Parser<'a> {
    /// Parses a file.
    pub fn parse_file(
        db: &'a dyn SyntaxGroup,
        diagnostics: &mut Diagnostics<ParserDiagnostic>,
        file_id: FileId,
        text: &'a str,
    ) -> SyntaxFile {
        let mut lexer = Lexer::from_text(db, file_id, text);
        let next_terminal = lexer.next().unwrap();
        let parser = Parser {
            db,
            file_id,
            lexer,
            next_terminal,
            pending_trivia: Vec::new(),
            offset: 0,
            current_width: 0,
            diagnostics,
        };
        let green = parser.parse_syntax_file();
        SyntaxFile::from_syntax_node(db, SyntaxNode::new_root(db, green))
    }

    /// Returns a GreenId of an ExprMissing and adds a diagnostic describing it.
    fn create_and_report_missing<T: TypedSyntaxNode>(
        &mut self,
        missing_kind: ParserDiagnosticKind,
    ) -> T::Green {
        let next_offset = (self.offset
            + self.current_width
            + self.peek().leading_trivia.iter().map(|t| t.0.width(self.db)).sum::<u32>())
            as usize;
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: missing_kind,
            span: TextSpan {
                start: TextOffset(next_offset),
                end: TextOffset(next_offset + self.peek().text.len() as usize),
            },
        });
        T::missing(self.db)
    }

    pub fn parse_syntax_file(mut self) -> SyntaxFileGreen {
        let items = ItemList::new_green(
            self.db,
            self.parse_list(Self::try_parse_top_level_item, SyntaxKind::TerminalEndOfFile),
        );
        while self.peek().kind != SyntaxKind::TerminalEndOfFile {
            self.skip_token();
        }

        // Fix offset in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.offset += self.current_width;

        let eof = self.add_trivia_to_terminal::<TerminalEndOfFile>(self.next_terminal.clone());
        SyntaxFile::new_green(self.db, items, eof)
    }

    // ------------------------------- Top level items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]).
    /// If can't parse as a top level item, keeps skipping tokens until it can.
    /// Returns None only when it reaches EOF.
    pub fn try_parse_top_level_item(&mut self) -> Option<ItemGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalModule => Some(self.expect_module().into()),
            SyntaxKind::TerminalStruct => Some(self.expect_struct().into()),
            SyntaxKind::TerminalExtern => Some(self.expect_extern_item()),
            SyntaxKind::TerminalFunction => Some(self.expect_free_function().into()),
            SyntaxKind::TerminalUse => Some(self.expect_use().into()),
            _ => None,
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: mod<Identifier>\{<ItemList>\}
    fn expect_module(&mut self) -> ItemModuleGreen {
        let module_kw = self.take::<TerminalModule>();
        let name = self.parse_token::<TerminalIdentifier>();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemModule::new_green(self.db, module_kw, name, semicolon)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: struct<Identifier><ParamListBraced>
    fn expect_struct(&mut self) -> ItemStructGreen {
        let struct_kw = self.take::<TerminalStruct>();
        let name = self.parse_token::<TerminalIdentifier>();
        let generic_args = self.parse_optional_generic_args();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let members = self.parse_param_list(SyntaxKind::TerminalRBrace);
        let rbrace = self.parse_token::<TerminalRBrace>();
        ItemStruct::new_green(self.db, struct_kw, name, generic_args, lbrace, members, rbrace)
    }

    /// Expected pattern: <ParenthesizedParamList><ReturnTypeClause>
    fn expect_function_signature(&mut self) -> FunctionSignatureGreen {
        // TODO(yuval): support generics
        let lparen = self.parse_token::<TerminalLParen>();
        let params = self.parse_param_list(SyntaxKind::TerminalRParen);
        let rparen = self.parse_token::<TerminalRParen>();
        let return_type_clause = self.parse_option_return_type_clause();
        FunctionSignature::new_green(self.db, lparen, params, rparen, return_type_clause)
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: extern(<FunctionSignature>|type<Identifier>);
    fn expect_extern_item(&mut self) -> ItemGreen {
        let extern_kw = self.take::<TerminalExtern>();
        match self.peek().kind {
            SyntaxKind::TerminalFunction => {
                let function_kw = self.take::<TerminalFunction>();
                let name = self.parse_token::<TerminalIdentifier>();
                let generic_args = self.parse_optional_generic_args();
                let signature = self.expect_function_signature();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                ItemExternFunction::new_green(
                    self.db,
                    extern_kw,
                    function_kw,
                    name,
                    generic_args,
                    signature,
                    semicolon,
                )
                .into()
            }
            _ => {
                // TODO(spapini): Do'nt return ItemExternType if we don't see a type.
                let type_kw = self.parse_token::<TerminalType>();
                let name = self.parse_token::<TerminalIdentifier>();
                let generic_args = self.parse_optional_generic_args();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                // If the next token is not type, assume it is missing.
                ItemExternType::new_green(
                    self.db,
                    extern_kw,
                    type_kw,
                    name,
                    generic_args,
                    semicolon,
                )
                .into()
            }
        }
    }

    /// Assumes the current token is Use.
    /// Expected pattern: use<Path>;
    fn expect_use(&mut self) -> ItemUseGreen {
        let use_kw = self.take::<TerminalUse>();
        let path = self.parse_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemUse::new_green(self.db, use_kw, path, semicolon)
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    fn expect_free_function(&mut self) -> ItemFreeFunctionGreen {
        let function_kw = self.take::<TerminalFunction>();
        let name = self.parse_token::<TerminalIdentifier>();
        let generic_args = self.parse_optional_generic_args();
        let signature = self.expect_function_signature();
        let function_body = self.parse_block();
        ItemFreeFunction::new_green(
            self.db,
            function_kw,
            name,
            generic_args,
            signature,
            function_body,
        )
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<ExprGreen> {
        match self.peek().kind {
            // Call parse_block() and not expect_block() because it's cheap.
            SyntaxKind::TerminalLBrace => Some(self.parse_block().into()),
            SyntaxKind::TerminalMatch => Some(self.expect_match_expr().into()),
            _ => self.try_parse_simple_expression(MAX_PRECEDENCE),
        }
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or a node
    /// with kind ExprMissing if an expression can't be parsed.
    pub fn parse_expr(&mut self) -> ExprGreen {
        match self.try_parse_expr() {
            Some(green) => green,
            None => self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression),
        }
    }

    fn parse_binary_operator(&mut self) -> BinaryOperatorGreen {
        match self.peek().kind {
            SyntaxKind::TerminalDot => self.take::<TerminalDot>().into(),
            SyntaxKind::TerminalNot => self.take::<TerminalNot>().into(),
            SyntaxKind::TerminalMul => self.take::<TerminalMul>().into(),
            SyntaxKind::TerminalDiv => self.take::<TerminalDiv>().into(),
            SyntaxKind::TerminalPlus => self.take::<TerminalPlus>().into(),
            SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
            SyntaxKind::TerminalEqEq => self.take::<TerminalEqEq>().into(),
            _ => unreachable!(),
        }
    }
    fn parse_unary_operator(&mut self) -> UnaryOperatorGreen {
        match self.peek().kind {
            SyntaxKind::TerminalNot => self.take::<TerminalNot>().into(),
            SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
            _ => unreachable!(),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or None if such an expression can't be parsed.
    fn try_parse_simple_expression(&mut self, parent_precedence: usize) -> Option<ExprGreen> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            let op = self.parse_unary_operator();
            let expr = self.parse_simple_expression(precedence);
            ExprUnary::new_green(self.db, op, expr).into()
        } else {
            self.try_parse_atom()?
        };

        while let Some(precedence) = get_binary_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            let op = self.parse_binary_operator();
            let rhs = self.parse_simple_expression(precedence);
            expr = ExprBinary::new_green(self.db, expr, op, rhs).into();
        }
        Some(expr)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or ExprMissing if such an expression can't be parsed.
    fn parse_simple_expression(&mut self, parent_precedence: usize) -> ExprGreen {
        match self.try_parse_simple_expression(parent_precedence) {
            Some(green) => green,
            None => self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprStructCtorCall|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_atom(&mut self) -> Option<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            SyntaxKind::TerminalIdentifier => {
                // Call parse_path() and not expect_path(), because it's cheap.
                let path = self.parse_path();
                match self.peek().kind {
                    SyntaxKind::TerminalLParen => Some(self.expect_function_call(path).into()),
                    SyntaxKind::TerminalLBrace => Some(self.expect_constructor_call(path).into()),
                    _ => Some(path.into()),
                }
            }
            SyntaxKind::TerminalFalse => Some(self.take::<TerminalFalse>().into()),
            SyntaxKind::TerminalTrue => Some(self.take::<TerminalTrue>().into()),
            SyntaxKind::TerminalLiteralNumber => Some(self.take::<TerminalLiteralNumber>().into()),
            SyntaxKind::TerminalLParen => Some(self.expect_parenthesized_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Returns a GreenId of a node with an ExprPath|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_type_expr(&mut self) -> Option<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            SyntaxKind::TerminalIdentifier => Some(self.parse_path().into()),
            SyntaxKind::TerminalLParen => Some(self.expect_parenthesized_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    fn expect_expression_list_parenthesized(&mut self) -> ExprListParenthesizedGreen {
        let lparen = self.take::<TerminalLParen>();
        let expression_list = ExprList::new_green(
            self.db,
            self.parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "expression",
            ),
        );
        let rparen = self.parse_token::<TerminalRParen>();
        ExprListParenthesized::new_green(self.db, lparen, expression_list, rparen)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<StructArgList>\}
    fn expect_struct_ctor_argument_list_braced(&mut self) -> ArgListBracedGreen {
        let lbrace = self.take::<TerminalLBrace>();
        let arg_list = StructArgList::new_green(
            self.db,
            self.parse_separated_list::<StructArg, TerminalComma, StructArgListElementOrSeparatorGreen>(
                Self::try_parse_struct_ctor_argument,
                is_of_kind!(rparen, block, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "struct constructor argument",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();

        ArgListBraced::new_green(self.db, lbrace, arg_list, rbrace)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: <ExprListParenthesized>
    fn expect_function_call(&mut self, path: ExprPathGreen) -> ExprFunctionCallGreen {
        let func_name = path;
        let parenthesized_args = self.expect_expression_list_parenthesized();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: <ExprListBraced>
    fn expect_constructor_call(&mut self, path: ExprPathGreen) -> ExprStructCtorCallGreen {
        let ctor_name = path;
        let args = self.expect_struct_ctor_argument_list_braced();
        ExprStructCtorCall::new_green(self.db, ctor_name, args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \((<expr>,)*<expr>?\)
    /// Returns a GreenId of a node with kind ExprParenthesized|ExprTuple.
    fn expect_parenthesized_expr(&mut self) -> ExprGreen {
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();

        if let [ExprListElementOrSeparatorGreen::Element(expr)] = &exprs[..] {
            // We have exactly one item and no separator --> This is not a tuple.
            ExprParenthesized::new_green(self.db, lparen, *expr, rparen).into()
        } else {
            ExprTuple::new_green(self.db, lparen, ExprList::new_green(self.db, exprs), rparen)
                .into()
        }
    }

    /// Assumes the current token is DotDot.
    /// Expected pattern: \.\.<Expr>
    fn expect_struct_argument_tail(&mut self) -> StructArgTailGreen {
        let dotdot = self.take::<TerminalDotDot>(); // ..
        // TODO(yuval): consider changing this to SimpleExpr once it exists.
        let expr = self.parse_expr();
        StructArgTail::new_green(self.db, dotdot, expr)
    }

    // For the similar syntax in Rust, see
    // https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax.
    /// Like parse_argument, but also allows a struct-arg-tail, e.g. 'let s2 = S{"s2", ..s1};'
    /// Returns a GreenId of a node with kind StructArgSingle|StructArgTail.
    fn try_parse_struct_ctor_argument(&mut self) -> Option<StructArgGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalDotDot => Some(self.expect_struct_argument_tail().into()),
            SyntaxKind::TerminalIdentifier => Some(self.expect_argument_single().into()),
            _ => None,
        }
    }

    /// Returns a GreenId of a node with kind StructArgExpr or OptionStructArgExprEmpty if an
    /// argument expression (":<value>") can't be parsed.
    fn parse_option_struct_arg_expression(&mut self) -> OptionStructArgExprGreen {
        if self.peek().kind == SyntaxKind::TerminalColon {
            let colon = self.take::<TerminalColon>();
            let value = self.parse_expr();
            StructArgExpr::new_green(self.db, colon, value).into()
        } else {
            OptionStructArgExprEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind StructArgSingle.
    fn expect_argument_single(&mut self) -> StructArgSingleGreen {
        let identifier = self.take::<TerminalIdentifier>();
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        StructArgSingle::new_green(self.db, identifier, struct_arg_expr)
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let lbrace = self.parse_token::<TerminalLBrace>();
        let statements = StatementList::new_green(
            self.db,
            self.parse_list(Self::try_parse_statement, SyntaxKind::TerminalRBrace),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        ExprBlock::new_green(self.db, lbrace, statements, rbrace)
    }

    /// Assumes the current token is Match.
    /// Expected pattern: match \{<MatchArm>*\}
    fn expect_match_expr(&mut self) -> ExprMatchGreen {
        let match_kw = self.take::<TerminalMatch>();
        // TODO(yuval): change to simple expression.
        let expr = self.parse_path().into();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let arms = MatchArms::new_green(
            self.db,
            self.parse_separated_list::<MatchArm, TerminalComma, MatchArmsElementOrSeparatorGreen>(
                Self::try_parse_match_arm,
                is_of_kind!(block, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "match arm",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, arms, rbrace)
    }

    /// Returns a GreenId of a node with a MatchArm kind or None if a match arm can't be parsed.
    pub fn try_parse_match_arm(&mut self) -> Option<MatchArmGreen> {
        let pattern = self.try_parse_pattern()?;
        let arrow = self.parse_token::<TerminalMatchArrow>();
        let expr = self.parse_expr();
        Some(MatchArm::new_green(self.db, pattern, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see [syntax::node::ast::Pattern]) or
    /// None if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> Option<PatternGreen> {
        // TODO(yuval): Support more options.
        match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => Some(self.take::<TerminalLiteralNumber>().into()),
            SyntaxKind::TerminalUnderscore => Some(self.take::<TerminalUnderscore>().into()),
            _ => None,
        }
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see [syntax::node::ast::Statement]) or
    /// None if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<StatementGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalLet => {
                let let_kw = self.take::<TerminalLet>();
                // TODO(yuval): support patterns instead of only an identifier.
                let name = self.parse_token::<TerminalIdentifier>();
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token::<TerminalEq>();
                let rhs = self.parse_expr();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(
                    StatementLet::new_green(self.db, let_kw, name, type_clause, eq, rhs, semicolon)
                        .into(),
                )
            }
            SyntaxKind::TerminalReturn => {
                let return_kw = self.take::<TerminalReturn>();
                let expr = self.parse_expr();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(StatementReturn::new_green(self.db, return_kw, expr, semicolon).into())
            }
            _ => match self.try_parse_expr() {
                None => None,
                Some(expr) => {
                    let optional_semicolon = if self.peek().kind == SyntaxKind::TerminalSemicolon {
                        self.take::<TerminalSemicolon>().into()
                    } else {
                        OptionSemicolonEmpty::new_green(self.db).into()
                    };
                    Some(StatementExpr::new_green(self.db, expr, optional_semicolon).into())
                }
            },
        }
    }

    /// Returns a GreenId of a node with kind TypeClause or OptionTypeClauseEmpty if a type clause
    /// can't be parsed.
    fn parse_option_type_clause(&mut self) -> OptionTypeClauseGreen {
        match self.try_parse_type_clause() {
            Some(green) => green.into(),
            None => OptionTypeClauseEmpty::new_green(self.db).into(),
        }
    }

    fn parse_type_clause(&mut self) -> TypeClauseGreen {
        match self.try_parse_type_clause() {
            Some(green) => green,
            None => self
                .create_and_report_missing::<TypeClause>(ParserDiagnosticKind::MissingTypeClause),
        }
    }
    fn try_parse_type_clause(&mut self) -> Option<TypeClauseGreen> {
        if self.peek().kind == SyntaxKind::TerminalColon {
            let colon = self.take::<TerminalColon>();
            let ty = self
                .try_parse_type_expr()
                .unwrap_or_else(|| ExprMissing::new_green(self.db).into());
            Some(TypeClause::new_green(self.db, colon, ty))
        } else {
            None
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_option_return_type_clause(&mut self) -> OptionReturnTypeClauseGreen {
        if self.peek().kind == SyntaxKind::TerminalArrow {
            let arrow = self.take::<TerminalArrow>();
            let return_type = self
                .try_parse_type_expr()
                .unwrap_or_else(|| ExprMissing::new_green(self.db).into());
            ReturnTypeClause::new_green(self.db, arrow, return_type).into()
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self, closing_token: SyntaxKind) -> ParamListGreen {
        ParamList::new_green(
            self.db,
            self.parse_separated_list::<Param, TerminalComma, ParamListElementOrSeparatorGreen>(
                Self::try_parse_param,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "parameter",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Param or None if a parameter can't be parsed.
    fn try_parse_param(&mut self) -> Option<ParamGreen> {
        let name = self.try_parse_token::<TerminalIdentifier>()?;
        let type_clause = self.parse_type_clause();
        Some(Param::new_green(self.db, name, type_clause))
    }

    /// Assumes the first token is Identifier.
    /// Expected pattern: <PathSegment>(::<PathSegment>)*
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> ExprPathGreen {
        // Initialize the list with the first path segment.
        let mut children: Vec<ExprPathElementOrSeparatorGreen> =
            vec![self.parse_path_segment().into()];
        while let Some(separator) = self.try_parse_token::<TerminalColonColon>() {
            children.push(separator.into());
            if let Some(segment) = self.try_parse_path_segment() {
                children.push(segment.into());
            } else {
                children.push(
                    self.create_and_report_missing::<PathSegment>(
                        ParserDiagnosticKind::MissingPathSegment,
                    )
                    .into(),
                );
                break;
            }
        }
        ExprPath::new_green(self.db, children)
    }

    /// Returns a GreenId of a node with kind PathSegment or None if a segment can't be parsed.
    fn try_parse_path_segment(&mut self) -> Option<PathSegmentGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalIdentifier => {
                let identifier = self.try_parse_token::<TerminalIdentifier>()?;
                Some(PathSegmentIdent::new_green(self.db, identifier).into())
            }
            SyntaxKind::TerminalLT => {
                Some(PathSegmentGenericArgs::new_green(self.db, self.expect_generic_args()).into())
            }
            _ => None,
        }
    }

    /// Returns a GreenId of a node with kind PathSegment or None if a segment can't be parsed.
    fn parse_path_segment(&mut self) -> PathSegmentGreen {
        self.try_parse_path_segment().unwrap_or_else(|| {
            self.create_and_report_missing::<PathSegment>(ParserDiagnosticKind::MissingPathSegment)
        })
    }

    /// Assumes the current token is LT.
    /// Expected pattern: \< <GenericArgList> \>
    fn expect_generic_args(&mut self) -> OptionGenericArgsSomeGreen {
        let langle = self.take::<TerminalLT>();
        let generic_args = GenericArgList::new_green(
            self.db,
            self.parse_separated_list::<Expr, TerminalComma, GenericArgListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, top_level),
                SyntaxKind::TerminalComma,
                "expression",
            ),
        );
        let rangle = self.parse_token::<TerminalGT>();
        OptionGenericArgsSome::new_green(self.db, langle, generic_args, rangle).into()
    }

    fn parse_optional_generic_args(&mut self) -> OptionGenericArgsGreen {
        if self.peek().kind != SyntaxKind::TerminalLT {
            return OptionGenericArgsEmpty::new_green(self.db).into();
        }
        self.expect_generic_args().into()
    }

    // ------------------------------- Helpers -------------------------------

    /// Parses a list of elements (without separators), where the elements are parsed using
    /// `try_parse_list_element`. The `closing` token indicates when to stop parsing.
    /// Returns the list of green ids of the elements. If it can't parse an element and the current
    /// token is not `closing`, skips the current token and tries again.
    // TODO(yuval): we want more advanced logic here - decide if token is missing or skipped
    // according to context. Same for the other list-parsing functions.
    fn parse_list<ElementGreen>(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> Option<ElementGreen>,
        should_stop: fn(SyntaxKind) -> bool,
        element_name: &'static str,
    ) -> Vec<ElementGreen> {
        let mut children: Vec<ElementGreen> = Vec::new();
        loop {
            let element = try_parse_list_element(self);
            if let Some(green) = element {
                children.push(green);
            } else {
                if should_stop(self.peek().kind) {
                    break;
                }
                self.skip_token(ParserDiagnosticKind::SkippedElement { element_name });
            }
        }
        children
    }

    /// Parses a list of elements with `separator`s, where the elements are parsed using
    /// `try_parse_list_element`. The separator may or may not appear in the end of the list.
    /// The `closing` token indicates when to stop parsing.
    /// Return the list of elements and separators. This list contains alternating children:
    /// [element, separator, element, separator, ...]. Both elements and separators may be missing.
    /// The length of the list is either 2 * #elements - 1 or 2 * #elements (a separator for each
    /// element or for each element but the last one).
    fn parse_separated_list<
        Element: TypedSyntaxNode,
        Separator: syntax::node::Terminal,
        ElementOrSeparatorGreen,
    >(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> Option<Element::Green>,
        should_stop: fn(SyntaxKind) -> bool,
        separator: SyntaxKind,
        element_name: &'static str,
    ) -> Vec<ElementOrSeparatorGreen>
    where
        ElementOrSeparatorGreen: From<Separator::Green> + From<Element::Green>,
    {
        let mut children: Vec<ElementOrSeparatorGreen> = Vec::new();
        loop {
            match try_parse_list_element(self) {
                None if should_stop(self.peek().kind) => {
                    break;
                }
                None => {
                    self.skip_token(ParserDiagnosticKind::SkippedElement { element_name });
                    continue;
                }
                Some(element) => {
                    children.push(element.into());
                }
            };

            let separator = match self.try_parse_token::<Separator>() {
                None if should_stop(self.peek().kind) => {
                    break;
                }
                None => self.create_and_report_missing::<Separator>(
                    ParserDiagnosticKind::MissingToken(separator),
                ),
                Some(separator) => separator,
            };
            children.push(separator.into());
        }
        children
    }

    /// Peeks at the next terminal from the Lexer without taking it.
    fn peek(&self) -> &LexerTerminal {
        &self.next_terminal
    }

    /// Takes a terminal from the Lexer and places it in self.next_terminal.
    fn take_raw(&mut self) -> LexerTerminal {
        self.offset += self.current_width;
        self.current_width = self.next_terminal.width(self.db);
        let next_terminal = self.lexer.next().unwrap();
        std::mem::replace(&mut self.next_terminal, next_terminal)
    }

    /// Skips a token. A skipped token is a token which is not expected where it is found. Skipping
    /// this token means reporting an error and ignoring it and continuing the compilation as if it
    /// wasn't there.
    fn skip_token(&mut self, diagnostic_kind: ParserDiagnosticKind) {
        let terminal = self.take_raw();

        let diag_start =
            (terminal.leading_trivia.iter().map(|trivium| trivium.0.width(self.db)).sum::<u32>()
                + self.offset) as usize;
        let diag_end = diag_start + terminal.text.len();

        // Add to pending trivia.
        self.pending_trivia.extend(terminal.leading_trivia);
        self.pending_trivia.push(TokenSkipped::new_green(self.db, terminal.text).into());
        self.pending_trivia.extend(terminal.trailing_trivia);
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: diagnostic_kind,
            span: TextSpan {
                start: TextOffset(diag_start as usize),
                end: TextOffset(diag_end as usize),
            },
        });
    }

    /// Builds a new terminal to replace the given terminal by gluing the recently skipped terminals
    /// to the given terminal as extra leading trivia.
    fn add_trivia_to_terminal<Terminal: syntax::node::Terminal>(
        &mut self,
        lexer_terminal: LexerTerminal,
    ) -> Terminal::Green {
        let LexerTerminal { text, kind: _, leading_trivia, trailing_trivia } = lexer_terminal;
        let token = Terminal::TokenType::new_green(self.db, text);
        let mut new_leading_trivia = mem::take(&mut self.pending_trivia);
        new_leading_trivia.extend(leading_trivia);
        Terminal::new_green(
            self.db,
            Trivia::new_green(self.db, new_leading_trivia),
            token,
            Trivia::new_green(self.db, trailing_trivia),
        )
    }

    /// Takes a token from the Lexer and place it in self.current. If tokens were skipped, glue them
    /// to this token as leading trivia.
    fn take<Terminal: syntax::node::Terminal>(&mut self) -> Terminal::Green {
        let token = self.take_raw();
        self.add_trivia_to_terminal::<Terminal>(token)
    }

    /// If the current terminal is of kind `Terminal`, returns its Green wrapper. Otherwise, returns
    /// None.
    fn try_parse_token<Terminal: syntax::node::Terminal>(&mut self) -> Option<Terminal::Green> {
        if Terminal::KIND == Some(self.peek().kind) {
            Some(self.take::<Terminal>())
        } else {
            // TODO(yuval): report to diagnostics.
            None
        }
    }

    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns Token::Missing.
    fn parse_token<Terminal: syntax::node::Terminal>(&mut self) -> Terminal::Green {
        match self.try_parse_token::<Terminal>() {
            Some(green) => green,
            None => self.create_and_report_missing::<Terminal>(ParserDiagnosticKind::MissingToken(
                Terminal::KIND.unwrap(),
            )),
        }
    }
}
