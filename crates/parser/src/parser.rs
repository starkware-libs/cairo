#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

use std::mem;

use diagnostics::Diagnostics;
use filesystem::ids::FileId;
use filesystem::span::{TextOffset, TextSpan};
use syntax::node::ast::*;
use syntax::node::db::SyntaxGroup;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use syntax::token::TokenKind;

use crate::diagnostic::ParserDiagnosticKind;
use crate::lexer::{Lexer, LexerTerminal};
use crate::operators::{get_binary_operator_precedence, get_unary_operator_precedence};
use crate::ParserDiagnostic;

// TODO(yuval): add diagnostics.

pub struct Parser<'a> {
    db: &'a dyn SyntaxGroup,
    file_id: FileId,
    lexer: Lexer<'a>,
    /// The next terminal to handle.
    next_terminal: LexerTerminal,
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
        let next_offset = (self.offset + self.current_width) as usize;
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: missing_kind,
            span: TextSpan {
                start: TextOffset(next_offset),
                end: TextOffset(next_offset + (self.peek().width(self.db)) as usize),
            },
        });
        T::missing(self.db)
    }

    pub fn parse_syntax_file(mut self) -> SyntaxFileGreen {
        let items = ItemList::new_green(
            self.db,
            self.parse_list(Self::try_parse_top_level_item, TokenKind::EndOfFile, "item"),
        );
        // This will not panic since the above parsing only stops when reaches EOF.
        assert_eq!(self.peek().kind, TokenKind::EndOfFile);

        // Fix offset in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.offset += self.current_width;

        let eof = self.add_trivia_to_terminal(self.next_terminal.clone());
        SyntaxFile::new_green(self.db, items, eof)
    }

    // ------------------------------- Top level items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]).
    /// If can't parse as a top level item, keeps skipping tokens until it can.
    /// Returns None only when it reaches EOF.
    pub fn try_parse_top_level_item(&mut self) -> Option<ItemGreen> {
        match self.peek().kind {
            TokenKind::Module => Some(self.expect_module().into()),
            TokenKind::Struct => Some(self.expect_struct().into()),
            TokenKind::Extern => Some(self.expect_extern_item()),
            TokenKind::Function => Some(self.expect_function().into()),
            TokenKind::Use => Some(self.expect_use().into()),
            _ => None,
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: mod<Identifier>\{<ItemList>\}
    fn expect_module(&mut self) -> ItemModuleGreen {
        let module_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let semicolon = self.parse_token(TokenKind::Semicolon);
        ItemModule::new_green(self.db, module_kw, name, semicolon)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: struct<Identifier><ParamListBraced>
    fn expect_struct(&mut self) -> ItemStructGreen {
        let struct_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let generic_args = self.parse_generic_args();
        let lbrace = self.parse_token(TokenKind::LBrace);
        let members = self.parse_param_list(TokenKind::RBrace);
        let rbrace = self.parse_token(TokenKind::RBrace);
        ItemStruct::new_green(self.db, struct_kw, name, generic_args, lbrace, members, rbrace)
    }

    /// Expected pattern: <ParenthesizedParamList><ReturnTypeClause>
    fn expect_function_signature(&mut self) -> FunctionSignatureGreen {
        // TODO(yuval): support generics
        let lparen = self.parse_token(TokenKind::LParen);
        let params = self.parse_param_list(TokenKind::RParen);
        let rparen = self.parse_token(TokenKind::RParen);
        let return_type_clause = self.parse_option_return_type_clause();
        FunctionSignature::new_green(self.db, lparen, params, rparen, return_type_clause)
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: extern(<FunctionSignature>|type<Identifier>);
    fn expect_extern_item(&mut self) -> ItemGreen {
        let extern_kw = self.take();
        match self.peek().kind {
            TokenKind::Function => {
                let function_kw = self.take();
                let name = self.parse_token(TokenKind::Identifier);
                let generic_args = self.parse_generic_args();
                let signature = self.expect_function_signature();
                let semicolon = self.parse_token(TokenKind::Semicolon);
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
                let type_kw = self.parse_token(TokenKind::Type);
                let name = self.parse_token(TokenKind::Identifier);
                let generic_args = self.parse_generic_args();
                let semicolon = self.parse_token(TokenKind::Semicolon);
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
        let usekw = self.take();
        let name = self.parse_path();
        let semicolon = self.parse_token(TokenKind::Semicolon);
        ItemUse::new_green(self.db, usekw, name, semicolon)
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    fn expect_function(&mut self) -> ItemFreeFunctionGreen {
        let function_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let generic_args = self.parse_generic_args();
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
            TokenKind::LBrace => Some(self.parse_block().into()),
            TokenKind::Match => Some(self.expect_match_expr().into()),
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

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or None if such an expression can't be parsed.
    fn try_parse_simple_expression(&mut self, parent_precedence: usize) -> Option<ExprGreen> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            let unary_op = self.take();
            let expr = self.parse_simple_expression(precedence);
            ExprUnary::new_green(self.db, unary_op, expr).into()
        } else {
            self.try_parse_atom()?
        };

        while let Some(precedence) = get_binary_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            let op = self.take();
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
            TokenKind::Identifier => {
                // Call parse_path() and not expect_path(), because it's cheap.
                let path = self.parse_path();
                match self.peek().kind {
                    TokenKind::LParen => Some(self.expect_function_call(path).into()),
                    TokenKind::LBrace => Some(self.expect_constructor_call(path).into()),
                    _ => Some(path.into()),
                }
            }
            TokenKind::False | TokenKind::True | TokenKind::LiteralNumber => {
                Some(ExprLiteral::new_green(self.db, self.take()).into())
            }
            TokenKind::LParen => Some(self.expect_parenthesized_expr()),
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
            TokenKind::Identifier => Some(self.parse_path().into()),
            TokenKind::LParen => Some(self.expect_parenthesized_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    fn expect_expression_list_parenthesized(&mut self) -> ExprListParenthesizedGreen {
        let lparen = self.take();
        let expression_list = ExprList::new_green(
            self.db,
            self.parse_separated_list(
                Self::try_parse_expr,
                TokenKind::Comma,
                TokenKind::RParen,
                "expression",
            ),
        );
        let rparen = self.parse_token(TokenKind::RParen);
        ExprListParenthesized::new_green(self.db, lparen, expression_list, rparen)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<StructArgList>\}
    fn expect_struct_ctor_argument_list_braced(&mut self) -> ArgListBracedGreen {
        let lbrace = self.take();
        let arg_list = StructArgList::new_green(
            self.db,
            self.parse_separated_list(
                Self::try_parse_struct_ctor_argument,
                TokenKind::Comma,
                TokenKind::RBrace,
                "struct constructor argument",
            ),
        );
        let rbrace = self.parse_token(TokenKind::RBrace);

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
        let lparen = self.take();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self.parse_separated_list(
            Self::try_parse_expr,
            TokenKind::Comma,
            TokenKind::RParen,
            "expression",
        );
        let rparen = self.parse_token(TokenKind::RParen);

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
        let dotdot = self.take(); // ..
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
            TokenKind::DotDot => Some(self.expect_struct_argument_tail().into()),
            TokenKind::Identifier => Some(self.expect_argument_single().into()),
            _ => None,
        }
    }

    /// Returns a GreenId of a node with kind StructArgExpr or OptionStructArgExprEmpty if an
    /// argument expression (":<value>") can't be parsed.
    fn parse_option_struct_arg_expression(&mut self) -> OptionStructArgExprGreen {
        if self.peek().kind == TokenKind::Colon {
            let colon = self.take();
            let value = self.parse_expr();
            StructArgExpr::new_green(self.db, colon, value).into()
        } else {
            OptionStructArgExprEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind StructArgSingle.
    fn expect_argument_single(&mut self) -> StructArgSingleGreen {
        let identifier = self.take();
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        StructArgSingle::new_green(self.db, identifier, struct_arg_expr)
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let lbrace = self.parse_token(TokenKind::LBrace);
        let statements = StatementList::new_green(
            self.db,
            self.parse_list(Self::try_parse_statement, TokenKind::RBrace, "statement"),
        );
        let rbrace = self.parse_token(TokenKind::RBrace);
        ExprBlock::new_green(self.db, lbrace, statements, rbrace)
    }

    /// Assumes the current token is Match.
    /// Expected pattern: match \{<MatchArm>*\}
    fn expect_match_expr(&mut self) -> ExprMatchGreen {
        let match_kw = self.take();
        // TODO(yuval): change to simple expression.
        let expr = self.parse_path().into();
        let lbrace = self.parse_token(TokenKind::LBrace);
        let match_arms = MatchArms::new_green(
            self.db,
            self.parse_separated_list(
                Self::try_parse_match_arm,
                TokenKind::Comma,
                TokenKind::RBrace,
                "match arm",
            ),
        );
        let rbrace = self.parse_token(TokenKind::RBrace);
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, match_arms, rbrace)
    }

    /// Returns a GreenId of a node with a MatchArm kind or None if a match arm can't be parsed.
    pub fn try_parse_match_arm(&mut self) -> Option<MatchArmGreen> {
        let pattern = self.try_parse_pattern()?;
        let arrow = self.parse_token(TokenKind::MatchArrow);
        let expr = self.parse_expr();
        Some(MatchArm::new_green(self.db, pattern, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see [syntax::node::ast::Pattern]) or
    /// None if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> Option<PatternGreen> {
        // TODO(yuval): Support more options.
        match self.peek().kind {
            TokenKind::LiteralNumber => Some(ExprLiteral::new_green(self.db, self.take()).into()),
            TokenKind::Underscore => Some(self.take().into()),
            _ => None,
        }
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see [syntax::node::ast::Statement]) or
    /// None if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<StatementGreen> {
        match self.peek().kind {
            TokenKind::Let => {
                let let_kw = self.take();
                // TODO(yuval): support patterns instead of only an identifier.
                let identifier = self.parse_token(TokenKind::Identifier);
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token(TokenKind::Eq);
                let expr = self.parse_expr();
                let semicolon = self.parse_token(TokenKind::Semicolon);
                Some(
                    StatementLet::new_green(
                        self.db,
                        let_kw,
                        identifier,
                        type_clause,
                        eq,
                        expr,
                        semicolon,
                    )
                    .into(),
                )
            }
            TokenKind::Return => {
                let return_kw = self.take();
                let expr = self.parse_expr();
                let semicolon = self.parse_token(TokenKind::Semicolon);
                Some(StatementReturn::new_green(self.db, return_kw, expr, semicolon).into())
            }
            _ => match self.try_parse_expr() {
                None => None,
                Some(expr) => {
                    let optional_semicolon = if self.peek().kind == TokenKind::Semicolon {
                        self.take().into()
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
        if self.peek().kind == TokenKind::Colon {
            let colon = self.take();
            let ty = self.try_parse_type_expr().unwrap_or_else(|| {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingTypeExpression)
            });
            Some(TypeClause::new_green(self.db, colon, ty))
        } else {
            None
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_option_return_type_clause(&mut self) -> OptionReturnTypeClauseGreen {
        if self.peek().kind == TokenKind::Arrow {
            let arrow = self.take();
            let return_type = self.try_parse_type_expr().unwrap_or_else(|| {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingTypeExpression)
            });
            ReturnTypeClause::new_green(self.db, arrow, return_type).into()
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self, closing_token: TokenKind) -> ParamListGreen {
        ParamList::new_green(
            self.db,
            self.parse_separated_list(
                Self::try_parse_param,
                TokenKind::Comma,
                closing_token,
                "parameter",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Param or None if a parameter can't be parsed.
    fn try_parse_param(&mut self) -> Option<ParamGreen> {
        let identifier = self.try_parse_token(TokenKind::Identifier)?;
        let type_clause = self.parse_type_clause();
        Some(Param::new_green(self.db, identifier, type_clause))
    }

    /// Assumes the first token is Identifier.
    /// Expected pattern: <PathSegment>(::<PathSegment>)*
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> ExprPathGreen {
        // Initialize the list with the first path segment.
        let mut children: Vec<ExprPathElementOrSeparatorGreen> =
            vec![self.parse_path_segment().into()];
        while let Some(separator) = self.try_parse_token(TokenKind::ColonColon) {
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
        let identifier = self.try_parse_token(TokenKind::Identifier)?;
        // TODO(yuval): support generics.
        let generic_args = OptionGenericArgsEmpty::new_green(self.db);
        Some(PathSegment::new_green(self.db, identifier, generic_args.into()))
    }

    /// Returns a GreenId of a node with kind PathSegment or None if a segment can't be parsed.
    fn parse_path_segment(&mut self) -> PathSegmentGreen {
        self.try_parse_path_segment().unwrap_or_else(|| {
            self.create_and_report_missing::<PathSegment>(ParserDiagnosticKind::MissingPathSegment)
        })
    }

    fn parse_generic_args(&mut self) -> OptionGenericArgsGreen {
        if self.peek().kind != TokenKind::LT {
            return OptionGenericArgsEmpty::new_green(self.db).into();
        }
        let langle = self.take();
        let generic_args = GenericArgList::new_green(
            self.db,
            self.parse_separated_list(
                Self::try_parse_expr,
                TokenKind::Comma,
                TokenKind::GT,
                "expression",
            ),
        );
        let rangle = self.parse_token(TokenKind::GT);
        OptionGenericArgsSome::new_green(self.db, langle, generic_args, rangle).into()
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
        closing: TokenKind,
        element_name: &'static str,
    ) -> Vec<ElementGreen> {
        let mut children: Vec<ElementGreen> = Vec::new();
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let element = try_parse_list_element(self);
            if let Some(green) = element {
                children.push(green);
            } else {
                self.skip_token(ParserDiagnosticKind::SkippedElement { element_name, closing });
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
    fn parse_separated_list<ElementGreen, ElementOrSeparator>(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> Option<ElementGreen>,
        separator: TokenKind,
        closing: TokenKind,
        element_name: &'static str,
    ) -> Vec<ElementOrSeparator>
    where
        ElementOrSeparator: From<TerminalGreen> + From<ElementGreen>,
    {
        let mut children: Vec<ElementOrSeparator> = Vec::new();
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let element = try_parse_list_element(self);
            // None means try_parse_list_element could not parse the next tokens as the expected
            // element.
            match element {
                None => {
                    self.skip_token(ParserDiagnosticKind::SkippedElement { element_name, closing });
                    continue;
                }
                Some(green) => {
                    children.push(green.into());
                }
            };
            if self.peek().kind == closing {
                break;
            }
            let separator = if self.peek().kind == separator {
                self.take()
            } else {
                self.create_and_report_missing::<Terminal>(ParserDiagnosticKind::MissingToken(
                    separator,
                ))
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
            terminal.leading_trivia.iter().map(|trivium| trivium.0.width(self.db)).sum::<u32>()
                + self.offset;
        let diag_end = diag_start + terminal.token.0.width(self.db);

        // Add to pending trivia.
        self.pending_trivia.extend(terminal.leading_trivia);
        self.pending_trivia.push(TriviumSkippedToken::new_green(self.db, terminal.token).into());
        self.pending_trivia.extend(terminal.trailing_trivia);
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: diagnostic_kind,
            span: TextSpan {
                start: TextOffset(diag_start as usize),
                end: TextOffset((diag_end) as usize),
            },
        });
    }

    /// Takes a token from the Lexer and place it in self.current. If tokens were skipped, glue them
    /// to this token as leading trivia.
    fn take(&mut self) -> TerminalGreen {
        let token = self.take_raw();
        self.add_trivia_to_terminal(token)
    }

    /// Builds a new terminal to replace the given terminal by gluing the recently skipped terminals
    /// to the given terminal as extra leading trivia.
    fn add_trivia_to_terminal(&mut self, lexer_terminal: LexerTerminal) -> TerminalGreen {
        let LexerTerminal { token, kind: _, leading_trivia, trailing_trivia } = lexer_terminal;
        let mut new_leading_trivia = mem::take(&mut self.pending_trivia);
        new_leading_trivia.extend(leading_trivia);
        Terminal::new_green(
            self.db,
            Trivia::new_green(self.db, new_leading_trivia),
            token,
            Trivia::new_green(self.db, trailing_trivia),
        )
    }

    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns None.
    fn try_parse_token(&mut self, token_kind: TokenKind) -> Option<TerminalGreen> {
        if self.peek().kind == token_kind {
            Some(self.take())
        } else {
            // TODO(yuval): report to diagnostics.
            None
        }
    }

    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns Token::Missing.
    fn parse_token(&mut self, token_kind: TokenKind) -> TerminalGreen {
        match self.try_parse_token(token_kind) {
            Some(green) => green,
            None => self.create_and_report_missing::<Terminal>(ParserDiagnosticKind::MissingToken(
                token_kind,
            )),
        }
    }
}
