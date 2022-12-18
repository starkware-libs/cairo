#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

use std::mem;

use diagnostics::DiagnosticsBuilder;
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
    /// The length of the trailing trivia following the last read token.
    last_trivia_length: u32,
    diagnostics: &'a mut DiagnosticsBuilder<ParserDiagnostic>,
}

// try_parse_<something>: returns a green ID with a kind that represents 'something' or None if
// 'something' can't be parsed.
// If None is returned, the current token is not consumed, otherwise it is (taken or skipped).
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
        diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
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
            last_trivia_length: 0,
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
        let next_offset = (self.offset + self.current_width - self.last_trivia_length) as usize;
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: missing_kind,
            span: TextSpan { start: TextOffset(next_offset), end: TextOffset(next_offset + 1) },
        });
        T::missing(self.db)
    }

    /// Returns the missing terminal and adds the corresponding missing token
    /// diagnostic report.
    fn create_and_report_missing_terminal<Terminal: syntax::node::Terminal>(
        &mut self,
    ) -> Terminal::Green {
        self.create_and_report_missing::<Terminal>(ParserDiagnosticKind::MissingToken(
            Terminal::KIND,
        ))
    }

    pub fn parse_syntax_file(mut self) -> SyntaxFileGreen {
        let items = ItemList::new_green(
            self.db,
            self.parse_list(Self::try_parse_top_level_item, is_of_kind!(), "item"),
        );
        // This will not panic since the above parsing only stops when reaches EOF.
        assert_eq!(self.peek().kind, SyntaxKind::TerminalEndOfFile);

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
        let attributes = self.parse_attribute_list();

        match self.peek().kind {
            SyntaxKind::TerminalModule => Some(self.expect_module(attributes).into()),
            SyntaxKind::TerminalStruct => Some(self.expect_struct(attributes).into()),
            SyntaxKind::TerminalEnum => Some(self.expect_enum(attributes).into()),
            SyntaxKind::TerminalExtern => Some(self.expect_extern_item(attributes)),
            SyntaxKind::TerminalFunction => Some(self.expect_free_function(attributes).into()),
            SyntaxKind::TerminalUse => Some(self.expect_use(attributes).into()),
            SyntaxKind::TerminalTrait => Some(self.expect_trait(attributes).into()),
            SyntaxKind::TerminalImpl => Some(self.expect_impl(attributes).into()),
            _ => None,
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: `mod <Identifier> \{<ItemList>\}` or `mod <Identifier>;`.
    fn expect_module(&mut self, attributes: AttributeListGreen) -> ItemModuleGreen {
        let module_kw = self.take::<TerminalModule>();
        let name = self.parse_identifier();

        let body = match self.peek().kind {
            SyntaxKind::TerminalLBrace => {
                let lbrace = self.take::<TerminalLBrace>();
                let items = ItemList::new_green(
                    self.db,
                    self.parse_list(Self::try_parse_top_level_item, is_of_kind!(rbrace), "item"),
                );
                let rbrace = self.parse_token::<TerminalRBrace>();
                ModuleBody::new_green(self.db, lbrace, items, rbrace).into()
            }
            // TODO: Improve diagnostic to indicate semicolon or a body were expected.
            _ => self.parse_token::<TerminalSemicolon>().into(),
        };

        ItemModule::new_green(self.db, attributes, module_kw, name, body)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: `struct<Identifier>{<ParamList>}`
    fn expect_struct(&mut self, attributes: AttributeListGreen) -> ItemStructGreen {
        let struct_kw = self.take::<TerminalStruct>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let members = self.parse_member_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        ItemStruct::new_green(
            self.db,
            attributes,
            struct_kw,
            name,
            generic_params,
            lbrace,
            members,
            rbrace,
        )
    }

    /// Assumes the current token is Enum.
    /// Expected pattern: `enum<Identifier>{<ParamList>}`
    fn expect_enum(&mut self, attributes: AttributeListGreen) -> ItemEnumGreen {
        let enum_kw = self.take::<TerminalEnum>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let variants = self.parse_member_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        ItemEnum::new_green(
            self.db,
            attributes,
            enum_kw,
            name,
            generic_params,
            lbrace,
            variants,
            rbrace,
        )
    }

    /// Expected pattern: `<ParenthesizedParamList><ReturnTypeClause>`
    fn expect_function_signature(&mut self) -> FunctionSignatureGreen {
        let lparen = self.parse_token::<TerminalLParen>();
        let params = self.parse_param_list();
        let rparen = self.parse_token::<TerminalRParen>();
        let return_type_clause = self.parse_option_return_type_clause();
        let implicits_clause = self.parse_option_implicits_clause();
        let optional_no_panic = if self.peek().kind == SyntaxKind::TerminalNoPanic {
            self.take::<TerminalNoPanic>().into()
        } else {
            OptionTerminalNoPanicEmpty::new_green(self.db).into()
        };

        FunctionSignature::new_green(
            self.db,
            lparen,
            params,
            rparen,
            return_type_clause,
            implicits_clause,
            optional_no_panic,
        )
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionSignature>|type<Identifier>);`
    fn expect_extern_item(&mut self, attributes: AttributeListGreen) -> ItemGreen {
        let extern_kw = self.take::<TerminalExtern>();
        match self.peek().kind {
            SyntaxKind::TerminalFunction => {
                let function_kw = self.take::<TerminalFunction>();

                let name = self.parse_identifier();
                let generic_params = self.parse_optional_generic_params();
                let signature = self.expect_function_signature();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                ItemExternFunction::new_green(
                    self.db,
                    attributes,
                    extern_kw,
                    function_kw,
                    name,
                    generic_params,
                    signature,
                    semicolon,
                )
                .into()
            }
            _ => {
                // TODO(spapini): Do'nt return ItemExternType if we don't see a type.
                let type_kw = self.parse_token::<TerminalType>();

                let name = self.parse_identifier();
                let generic_params = self.parse_optional_generic_params();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                // If the next token is not type, assume it is missing.
                ItemExternType::new_green(
                    self.db,
                    extern_kw,
                    type_kw,
                    name,
                    generic_params,
                    semicolon,
                )
                .into()
            }
        }
    }

    /// Assumes the current token is Use.
    /// Expected pattern: `use<Path>;`
    fn expect_use(&mut self, attributes: AttributeListGreen) -> ItemUseGreen {
        let use_kw = self.take::<TerminalUse>();
        let path = self.parse_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemUse::new_green(self.db, attributes, use_kw, path, semicolon)
    }

    /// Returns a GreenId of a node with an identifier kind or None if an identifier can't be
    /// parsed.
    /// Note that if the terminal is a keyword or an underscore, it is skipped, and
    /// Some(missing-identifier) is returned.
    fn try_parse_identifier(&mut self) -> Option<TerminalIdentifierGreen> {
        if self.peek().kind.is_keyword_terminal() {
            Some(self.skip_token_and_return_missing::<TerminalIdentifier>(
                ParserDiagnosticKind::ReservedIdentifier { identifier: self.peek().text.clone() },
            ))
        } else if self.peek().kind == SyntaxKind::TerminalUnderscore {
            Some(self.skip_token_and_return_missing::<TerminalIdentifier>(
                ParserDiagnosticKind::UnderscoreNotAllowedAsIdentifier,
            ))
        } else {
            self.try_parse_token::<TerminalIdentifier>()
        }
    }
    /// Returns whether the current token is an identifier, a keyword or an underscore ('_'),
    /// without consuming it. This should be used mostly, instead of checking whether the current
    /// token is an identifier, because in many cases we'd want to consume the keyword/underscore as
    /// the identifier and raise a relevant diagnostic
    /// (ReservedIdentifier/UnderscoreNotAllowedAsIdentifier).
    fn is_peek_identifier_like(&self) -> bool {
        let kind = self.peek().kind;
        kind.is_keyword_terminal()
            || matches!(kind, SyntaxKind::TerminalUnderscore | SyntaxKind::TerminalIdentifier)
    }

    /// Returns a GreenId of a node with an identifier kind.
    fn parse_identifier(&mut self) -> TerminalIdentifierGreen {
        match self.try_parse_identifier() {
            Some(identifier) => identifier,
            None => self.create_and_report_missing_terminal::<TerminalIdentifier>(),
        }
    }

    /// Parses the arguments of an attributes if exists.
    /// Expected pattern: `\(<ExprList>\)`
    fn try_attribute_arg_list_parenthesized(&mut self) -> OptionAttributeArgsGreen {
        if self.peek().kind != SyntaxKind::TerminalLParen {
            return OptionAttributeArgsEmpty::new_green(self.db).into();
        }
        let lparen = self.take::<TerminalLParen>();
        let args = self
            .parse_separated_list::<Expr, TerminalComma, AttributeArgListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                "expression",
            );
        let arg_list = AttributeArgList::new_green(self.db, args);
        let rparen = self.parse_token::<TerminalRParen>();
        AttributeArgs::new_green(self.db, lparen, arg_list, rparen).into()
    }

    /// Returns a GreenId of a node with an attribute kind or None if an attribute can't be parsed.
    fn try_parse_attribute(&mut self) -> Option<AttributeGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalHash => {
                let hash = self.take::<TerminalHash>();
                let lbrack = self.parse_token::<TerminalLBrack>();

                let attr = self.parse_identifier();
                let args = self.try_attribute_arg_list_parenthesized();
                let rbrack = self.parse_token::<TerminalRBrack>();

                Some(Attribute::new_green(self.db, hash, lbrack, attr, args, rbrack))
            }
            _ => None,
        }
    }

    /// Parses an attribute list.
    fn parse_attribute_list(&mut self) -> AttributeListGreen {
        let expected_elements =
            "Module/Use/FreeFunction/ExternFunction/ExternType/Trait/Impl/Struct/Enum/Attribute";
        AttributeList::new_green(
            self.db,
            self.parse_list(
                Self::try_parse_attribute,
                is_of_kind!(rbrace, top_level),
                expected_elements,
            ),
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionSignature><Block>`
    fn expect_free_function(&mut self, attributes: AttributeListGreen) -> ItemFreeFunctionGreen {
        let function_kw = self.take::<TerminalFunction>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let signature = self.expect_function_signature();
        let function_body = self.parse_block();
        ItemFreeFunction::new_green(
            self.db,
            attributes,
            function_kw,
            name,
            generic_params,
            signature,
            function_body,
        )
    }

    /// Assumes the current token is Trait.
    fn expect_trait(&mut self, attributes: AttributeListGreen) -> ItemTraitGreen {
        let trait_kw = self.take::<TerminalTrait>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            let lbrace = self.take::<TerminalLBrace>();
            let items = TraitItemList::new_green(
                self.db,
                self.parse_list(Self::try_parse_trait_item, is_of_kind!(rbrace), "trait item"),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            TraitBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        ItemTrait::new_green(self.db, attributes, trait_kw, name, generic_params, body)
    }

    /// Returns a GreenId of a node with a TraitItem.* kind (see [syntax::node::ast::TraitItem]).
    pub fn try_parse_trait_item(&mut self) -> Option<TraitItemGreen> {
        let attributes = self.parse_attribute_list();

        match self.peek().kind {
            SyntaxKind::TerminalFunction => Some(self.expect_trait_function(attributes).into()),
            _ => None,
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionSignature><SemiColon>`
    fn expect_trait_function(&mut self, attributes: AttributeListGreen) -> TraitItemFunctionGreen {
        let function_kw = self.take::<TerminalFunction>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let signature = self.expect_function_signature();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        TraitItemFunction::new_green(
            self.db,
            attributes,
            function_kw,
            name,
            generic_params,
            signature,
            semicolon,
        )
    }

    /// Assumes the current token is Impl.
    fn expect_impl(&mut self, attributes: AttributeListGreen) -> ItemImplGreen {
        let impl_kw = self.take::<TerminalImpl>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let of_kw = self.parse_token::<TerminalOf>();
        let trait_path = self.parse_path();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            let lbrace = self.take::<TerminalLBrace>();
            let items = ItemList::new_green(
                self.db,
                self.parse_list(Self::try_parse_top_level_item, is_of_kind!(rbrace), "item"),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            ImplBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        ItemImpl::new_green(
            self.db,
            attributes,
            impl_kw,
            name,
            generic_params,
            of_kw,
            trait_path,
            body,
        )
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<ExprGreen> {
        self.try_parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Allow)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or a node
    /// with kind ExprMissing if an expression can't be parsed.
    pub fn parse_expr(&mut self) -> ExprGreen {
        match self.try_parse_expr() {
            Some(green) => green,
            None => self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Assumes the current token is an operator (binary or unary).
    /// Returns a GreenId of the operator or None if the operator is a unary-only operator.
    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperatorGreen> {
        if self.peek().kind == SyntaxKind::TerminalNot {
            None
        } else {
            Some(match self.peek().kind {
                SyntaxKind::TerminalDot => self.take::<TerminalDot>().into(),
                SyntaxKind::TerminalMul => self.take::<TerminalMul>().into(),
                SyntaxKind::TerminalDiv => self.take::<TerminalDiv>().into(),
                SyntaxKind::TerminalMod => self.take::<TerminalMod>().into(),
                SyntaxKind::TerminalPlus => self.take::<TerminalPlus>().into(),
                SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
                SyntaxKind::TerminalEq => self.take::<TerminalEq>().into(),
                SyntaxKind::TerminalEqEq => self.take::<TerminalEqEq>().into(),
                SyntaxKind::TerminalNeq => self.take::<TerminalNeq>().into(),
                SyntaxKind::TerminalLT => self.take::<TerminalLT>().into(),
                SyntaxKind::TerminalGT => self.take::<TerminalGT>().into(),
                SyntaxKind::TerminalLE => self.take::<TerminalLE>().into(),
                SyntaxKind::TerminalGE => self.take::<TerminalGE>().into(),
                SyntaxKind::TerminalAnd => self.take::<TerminalAnd>().into(),
                SyntaxKind::TerminalOr => self.take::<TerminalOr>().into(),
                _ => unreachable!(),
            })
        }
    }
    /// Assumes the current token is a unary operator, and returns a GreenId of the operator.
    fn parse_unary_operator(&mut self) -> UnaryOperatorGreen {
        match self.peek().kind {
            SyntaxKind::TerminalNot => self.take::<TerminalNot>().into(),
            SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
            _ => unreachable!(),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// such an expression can't be parsed.
    ///
    /// Parsing will be limited by:
    /// `parent_precedence` - parsing of boolean operators limited to this.
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_expr_limited(
        &mut self,
        parent_precedence: usize,
        lbrace_allowed: LbraceAllowed,
    ) -> Option<ExprGreen> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            let op = self.parse_unary_operator();
            let expr = self.parse_expr_limited(precedence, lbrace_allowed);
            ExprUnary::new_green(self.db, op, expr).into()
        } else {
            self.try_parse_atom(lbrace_allowed)?
        };

        // ? operator has the highest precedence, so we now find all the usages after.
        while self.peek().kind == SyntaxKind::TerminalQuestionMark {
            expr = ExprErrorPropagate::new_green(
                self.db,
                expr,
                self.parse_token::<TerminalQuestionMark>(),
            )
            .into();
        }
        while let Some(precedence) = get_binary_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            if let Some(op) = self.try_parse_binary_operator() {
                let rhs = self.parse_expr_limited(precedence, lbrace_allowed);
                expr = ExprBinary::new_green(self.db, expr, op, rhs).into();
            } else {
                return Some(expr);
            }
        }
        Some(expr)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or ExprMissing if such an expression can't be parsed.
    ///
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn parse_expr_limited(
        &mut self,
        parent_precedence: usize,
        lbrace_allowed: LbraceAllowed,
    ) -> ExprGreen {
        match self.try_parse_expr_limited(parent_precedence, lbrace_allowed) {
            Some(green) => green,
            None => self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprStructCtorCall|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    ///
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_atom(&mut self, lbrace_allowed: LbraceAllowed) -> Option<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            SyntaxKind::TerminalIdentifier => {
                // Call parse_path() and not expect_path(), because it's cheap.
                let path = self.parse_path();
                match self.peek().kind {
                    SyntaxKind::TerminalLParen => Some(self.expect_function_call(path).into()),
                    SyntaxKind::TerminalLBrace if lbrace_allowed == LbraceAllowed::Allow => {
                        Some(self.expect_constructor_call(path).into())
                    }
                    _ => Some(path.into()),
                }
            }
            SyntaxKind::TerminalFalse => Some(self.take::<TerminalFalse>().into()),
            SyntaxKind::TerminalTrue => Some(self.take::<TerminalTrue>().into()),
            SyntaxKind::TerminalLiteralNumber => Some(self.take::<TerminalLiteralNumber>().into()),
            SyntaxKind::TerminalShortString => Some(self.take::<TerminalShortString>().into()),
            SyntaxKind::TerminalLParen => {
                // Note that LBrace is allowed inside parenthesis, even if `lbrace_allowed` is
                // [LbraceAllowed::Forbid].
                Some(self.expect_parenthesized_expr())
            }
            SyntaxKind::TerminalLBrace if lbrace_allowed == LbraceAllowed::Allow => {
                Some(self.parse_block().into())
            }
            SyntaxKind::TerminalMatch if lbrace_allowed == LbraceAllowed::Allow => {
                Some(self.expect_match_expr().into())
            }
            SyntaxKind::TerminalIf if lbrace_allowed == LbraceAllowed::Allow => {
                Some(self.expect_if_expr().into())
            }
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
    /// Expected pattern: `\(<ExprList>\)`
    fn expect_expression_list_parenthesized(&mut self) -> ExprListParenthesizedGreen {
        let lparen = self.take::<TerminalLParen>();
        let expression_list = ExprList::new_green(
            self.db,
            self.parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                "expression",
            ),
        );
        let rparen = self.parse_token::<TerminalRParen>();
        ExprListParenthesized::new_green(self.db, lparen, expression_list, rparen)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `\{<StructArgList>\}`
    fn expect_struct_ctor_argument_list_braced(&mut self) -> ArgListBracedGreen {
        let lbrace = self.take::<TerminalLBrace>();
        let arg_list = StructArgList::new_green(
            self.db,
            self.parse_separated_list::<StructArg, TerminalComma, StructArgListElementOrSeparatorGreen>(
                Self::try_parse_struct_ctor_argument,
                is_of_kind!(rparen, block, rbrace, top_level),
                "struct constructor argument",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();

        ArgListBraced::new_green(self.db, lbrace, arg_list, rbrace)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `<ExprListParenthesized>`
    fn expect_function_call(&mut self, path: ExprPathGreen) -> ExprFunctionCallGreen {
        let func_name = path;
        let parenthesized_args = self.expect_expression_list_parenthesized();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `<ExprListBraced>`
    fn expect_constructor_call(&mut self, path: ExprPathGreen) -> ExprStructCtorCallGreen {
        let ctor_name = path;
        let args = self.expect_struct_ctor_argument_list_braced();
        ExprStructCtorCall::new_green(self.db, ctor_name, args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\((<expr>,)*<expr>?\)`
    /// Returns a GreenId of a node with kind ExprParenthesized|ExprTuple.
    fn expect_parenthesized_expr(&mut self) -> ExprGreen {
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
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
    /// Expected pattern: `\.\.<Expr>`
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
            _ => Some(self.try_parse_argument_single()?.into()),
        }
    }

    /// Returns a GreenId of a node with kind StructArgExpr or OptionStructArgExprEmpty if an
    /// argument expression `(":<value>")` can't be parsed.
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
    fn try_parse_argument_single(&mut self) -> Option<StructArgSingleGreen> {
        let identifier = self.try_parse_identifier()?;
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        Some(StructArgSingle::new_green(self.db, identifier, struct_arg_expr))
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let lbrace = self.parse_token::<TerminalLBrace>();
        let statements = StatementList::new_green(
            self.db,
            self.parse_list(Self::try_parse_statement, is_of_kind!(rbrace, top_level), "statement"),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        ExprBlock::new_green(self.db, lbrace, statements, rbrace)
    }

    /// Assumes the current token is `Match`.
    /// Expected pattern: `match <expr> \{<MatchArm>*\}`
    fn expect_match_expr(&mut self) -> ExprMatchGreen {
        let match_kw = self.take::<TerminalMatch>();
        let expr = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
        let lbrace = self.parse_token::<TerminalLBrace>();
        let arms = MatchArms::new_green(
            self.db,
            self.parse_separated_list::<MatchArm, TerminalComma, MatchArmsElementOrSeparatorGreen>(
                Self::try_parse_match_arm,
                is_of_kind!(block, rbrace, top_level),
                "match arm",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, arms, rbrace)
    }

    /// Assumes the current token is `If`.
    /// Expected pattern: `if <expr> <block> [else <block>]`.
    fn expect_if_expr(&mut self) -> ExprIfGreen {
        let if_kw = self.take::<TerminalIf>();
        let condition = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
        let if_block = self.parse_block();

        let else_clause: OptionElseClauseGreen = if self.peek().kind == SyntaxKind::TerminalElse {
            let else_kw = self.take::<TerminalElse>();
            let else_block_or_if = if self.peek().kind == SyntaxKind::TerminalIf {
                BlockOrIfGreen::from(self.expect_if_expr())
            } else {
                BlockOrIfGreen::from(self.parse_block())
            };
            ElseClause::new_green(self.db, else_kw, else_block_or_if).into()
        } else {
            OptionElseClauseEmpty::new_green(self.db).into()
        };

        ExprIf::new_green(self.db, if_kw, condition, if_block, else_clause)
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
        let modifier_list = self.parse_modifier_list();
        if !modifier_list.is_empty() {
            let modifiers = ModifierList::new_green(self.db, modifier_list);
            let name = self.parse_identifier();
            return Some(PatternIdentifier::new_green(self.db, modifiers, name).into());
        };

        // TODO(yuval): Support "Or" patterns.
        Some(match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => self.take::<TerminalLiteralNumber>().into(),
            SyntaxKind::TerminalShortString => self.take::<TerminalShortString>().into(),
            SyntaxKind::TerminalUnderscore => self.take::<TerminalUnderscore>().into(),
            SyntaxKind::TerminalIdentifier => {
                // TODO(ilya): Consider parsing a single identifier as PatternIdentifier rather
                // then ExprPath.
                let path = self.parse_path();
                match self.peek().kind {
                    SyntaxKind::TerminalLBrace => {
                        let lbrace = self.take::<TerminalLBrace>();
                        let params = PatternStructParamList::new_green(
                            self.db,
                            self.parse_separated_list::<
                                PatternStructParam,
                                TerminalComma,
                                PatternStructParamListElementOrSeparatorGreen>
                            (
                                Self::try_parse_pattern_struct_param,
                                is_of_kind!(rparen, block, rbrace, top_level),
                                "struct pattern parameter",
                            ),
                        );
                        let rbrace = self.take::<TerminalRBrace>();
                        PatternStruct::new_green(self.db, path, lbrace, params, rbrace).into()
                    }
                    SyntaxKind::TerminalLParen => {
                        // Enum pattern.
                        let lparen = self.take::<TerminalLParen>();
                        let pattern = self.parse_pattern();
                        let rparen = self.parse_token::<TerminalRParen>();
                        PatternEnum::new_green(self.db, path, lparen, pattern, rparen).into()
                    }
                    _ => path.into(),
                }
            }
            SyntaxKind::TerminalLParen => {
                let lparen = self.take::<TerminalLParen>();
                let patterns = PatternList::new_green(self.db,  self.parse_separated_list::<
                    Pattern,
                    TerminalComma,
                    PatternListElementOrSeparatorGreen>
                (
                    Self::try_parse_pattern,
                    is_of_kind!(rparen, block, rbrace, top_level),
                    "pattern",
                ));
                let rparen = self.parse_token::<TerminalRParen>();
                PatternTuple::new_green(self.db, lparen, patterns, rparen).into()
            }
            _ => return None,
        })
    }
    /// Returns a GreenId of a node with some Pattern kind (see [syntax::node::ast::Pattern]).
    fn parse_pattern(&mut self) -> PatternGreen {
        // If not found, return a missing underscore pattern.
        self.try_parse_pattern().unwrap_or_else(|| {
            self.create_and_report_missing_terminal::<TerminalUnderscore>().into()
        })
    }

    /// Returns a GreenId of a syntax in side a struct pattern. Example:
    /// `MyStruct { param0, param1: _, .. }`.
    fn try_parse_pattern_struct_param(&mut self) -> Option<PatternStructParamGreen> {
        Some(match self.peek().kind {
            SyntaxKind::TerminalDotDot => self.take::<TerminalDotDot>().into(),
            _ => {
                let name = self.try_parse_identifier()?;
                if self.peek().kind == SyntaxKind::TerminalColon {
                    let colon = self.take::<TerminalColon>();
                    let pattern = self.parse_pattern();
                    PatternStructParamWithExpr::new_green(self.db, name, colon, pattern).into()
                } else {
                    name.into()
                }
            }
        })
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see [syntax::node::ast::Statement]) or
    /// None if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<StatementGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalLet => {
                let let_kw = self.take::<TerminalLet>();
                let pattern = self.parse_pattern();
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token::<TerminalEq>();
                let rhs = self.parse_expr();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(
                    StatementLet::new_green(
                        self.db,
                        let_kw,
                        pattern,
                        type_clause,
                        eq,
                        rhs,
                        semicolon,
                    )
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
                        OptionTerminalSemicolonEmpty::new_green(self.db).into()
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
        if self.peek().kind == SyntaxKind::TerminalArrow {
            let arrow = self.take::<TerminalArrow>();
            let return_type = self.try_parse_type_expr().unwrap_or_else(|| {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingTypeExpression)
            });
            ReturnTypeClause::new_green(self.db, arrow, return_type).into()
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ImplicitsClause or OptionImplicitsClauseEmpty if a
    /// implicits-clause can't be parsed.
    fn parse_option_implicits_clause(&mut self) -> OptionImplicitsClauseGreen {
        if self.peek().kind == SyntaxKind::TerminalImplicits {
            let implicits_kw = self.take::<TerminalImplicits>();
            let lparen = self.parse_token::<TerminalLParen>();
            let implicits = ImplicitsList::new_green(
                self.db,
                self.parse_separated_list::<ExprPath, TerminalComma, ImplicitsListElementOrSeparatorGreen>(
                    Self::try_parse_path,
                    // Don't stop at keywords as try_parse_path handles keywords inside it. Otherwise the diagnostic is less accurate.
                    is_of_kind!(rparen, lbrace, rbrace),
                    "implicit type",
                ),
            );
            let rparen = self.parse_token::<TerminalRParen>();
            ImplicitsClause::new_green(self.db, implicits_kw, lparen, implicits, rparen).into()
        } else {
            OptionImplicitsClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self) -> ParamListGreen {
        ParamList::new_green(
            self.db,
            self.parse_separated_list::<Param, TerminalComma, ParamListElementOrSeparatorGreen>(
                Self::try_parse_param,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                "parameter",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Modifier or None if a modifier can't be parsed.
    fn try_parse_modifier(&mut self) -> Option<ModifierGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalRef => Some(self.take::<TerminalRef>().into()),
            SyntaxKind::TerminalMut => Some(self.take::<TerminalMut>().into()),
            _ => None,
        }
    }

    /// Returns a vector of GreenIds with kind Modifier.
    fn parse_modifier_list(&mut self) -> Vec<ModifierGreen> {
        let mut modifier_list = vec![];

        while let Some(modifier) = self.try_parse_modifier() {
            modifier_list.push(modifier);
        }
        modifier_list
    }

    /// Returns a GreenId of a node with kind Param or None if a parameter can't be parsed.
    fn try_parse_param(&mut self) -> Option<ParamGreen> {
        let modifier_list = self.parse_modifier_list();
        let name = if modifier_list.is_empty() {
            self.try_parse_param_name()?
        } else {
            // If we had modifiers then the identifier is not optional and can't be '_'.
            self.parse_identifier().into()
        };

        let type_clause = self.parse_type_clause();
        Some(Param::new_green(
            self.db,
            ModifierList::new_green(self.db, modifier_list),
            name,
            type_clause,
        ))
    }

    /// Returns a GreenId of a node with some ParamName kind (see [syntax::node::ast::ParamName]) or
    /// None if a param name can't be parsed.
    fn try_parse_param_name(&mut self) -> Option<ParamNameGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalUnderscore => Some(self.take::<TerminalUnderscore>().into()),
            _ => Some(self.try_parse_identifier()?.into()),
        }
    }

    /// Returns a GreenId of a node with kind MemberList.
    fn parse_member_list(&mut self) -> MemberListGreen {
        MemberList::new_green(
            self.db,
            self.parse_separated_list::<Member, TerminalComma, MemberListElementOrSeparatorGreen>(
                Self::try_parse_member,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                "member or variant",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Member or None if a struct member/enum variant can't
    /// be parsed.
    fn try_parse_member(&mut self) -> Option<MemberGreen> {
        let name = self.try_parse_identifier()?;
        let type_clause = self.parse_type_clause();
        Some(Member::new_green(self.db, name, type_clause))
    }

    /// Expected pattern: `<PathSegment>(::<PathSegment>)*`
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> ExprPathGreen {
        let mut children: Vec<ExprPathElementOrSeparatorGreen> = vec![];
        loop {
            let (segment, optional_separator) = self.parse_path_segment();
            children.push(segment.into());

            if let Some(separator) = optional_separator {
                children.push(separator.into());
                continue;
            }
            break;
        }

        ExprPath::new_green(self.db, children)
    }
    /// Returns a GreenId of a node with kind ExprPath or None if a path can't be parsed.
    fn try_parse_path(&mut self) -> Option<ExprPathGreen> {
        if self.is_peek_identifier_like() { Some(self.parse_path()) } else { None }
    }

    /// Returns a PathSegment and an optional separator.
    fn parse_path_segment(&mut self) -> (PathSegmentGreen, Option<TerminalColonColonGreen>) {
        let identifier = match self.try_parse_identifier() {
            Some(identifier) => identifier,
            None => {
                return (
                    self.create_and_report_missing::<PathSegment>(
                        ParserDiagnosticKind::MissingPathSegment,
                    ),
                    // TODO(ilya, 10/10/2022): Should we continue parsing the path here?
                    None,
                );
            }
        };

        match self.try_parse_token::<TerminalColonColon>() {
            Some(separator) if self.peek().kind == SyntaxKind::TerminalLT => (
                PathSegmentWithGenericArgs::new_green(
                    self.db,
                    identifier,
                    separator,
                    self.expect_generic_args(),
                )
                .into(),
                self.try_parse_token::<TerminalColonColon>(),
            ),
            optional_separator => {
                (PathSegmentSimple::new_green(self.db, identifier).into(), optional_separator)
            }
        }
    }

    /// Returns a GreenId of a node with an ExprLiteral|ExprPath|ExprParenthesized|ExprTuple kind,
    /// or None if such an expression can't be parsed.
    fn try_parse_generic_arg(&mut self) -> Option<ExprGreen> {
        if self.peek().kind == SyntaxKind::TerminalLiteralNumber {
            Some(self.take::<TerminalLiteralNumber>().into())
        } else if self.peek().kind == SyntaxKind::TerminalShortString {
            Some(self.take::<TerminalShortString>().into())
        } else {
            self.try_parse_type_expr()
        }
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericArgList> \>`
    fn expect_generic_args(&mut self) -> GenericArgsGreen {
        let langle = self.take::<TerminalLT>();
        let generic_args = GenericArgList::new_green(
            self.db,
            self.parse_separated_list::<Expr, TerminalComma, GenericArgListElementOrSeparatorGreen>(
                Self::try_parse_generic_arg,
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, top_level),
                "generic arg",
            ),
        );
        let rangle = self.parse_token::<TerminalGT>();
        GenericArgs::new_green(self.db, langle, generic_args, rangle)
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericParamList> \>`
    fn expect_generic_params(&mut self) -> WrappedGenericParamListGreen {
        let langle = self.take::<TerminalLT>();
        let generic_params = GenericParamList::new_green(
            self.db,
            self.parse_separated_list::<GenericParam, TerminalComma, GenericParamListElementOrSeparatorGreen>(
                Self::try_parse_generic_param,
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, top_level),
                "generic param",
            ),
        );
        let rangle = self.parse_token::<TerminalGT>();
        WrappedGenericParamList::new_green(self.db, langle, generic_params, rangle)
    }

    fn parse_optional_generic_params(&mut self) -> OptionWrappedGenericParamListGreen {
        if self.peek().kind != SyntaxKind::TerminalLT {
            return OptionWrappedGenericParamListEmpty::new_green(self.db).into();
        }
        self.expect_generic_params().into()
    }

    fn try_parse_generic_param(&mut self) -> Option<GenericParamGreen> {
        self.try_parse_identifier().map(|name| GenericParam::new_green(self.db, name))
    }

    // ------------------------------- Helpers -------------------------------

    /// Parses a list of elements (without separators), where the elements are parsed using
    /// `try_parse_list_element`.
    /// Returns the list of green ids of the elements.
    ///
    /// `should_stop` is a predicate to decide how to proceed in case an element can't be parsed,
    /// according to the current token. If it returns true, the parsing of the list stops. If it
    /// returns false, the current token is skipped and we try to parse an element again.
    fn parse_list<ElementGreen>(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> Option<ElementGreen>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &'static str,
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
                self.skip_token(ParserDiagnosticKind::SkippedElement {
                    element_name: expected_element,
                });
            }
        }
        children
    }

    /// Parses a list of elements with `separator`s, where the elements are parsed using
    /// `try_parse_list_element`. The separator may or may not appear in the end of the list.
    /// Returns the list of elements and separators. This list contains alternating children:
    /// [element, separator, element, separator, ...]. Separators may be missing.
    /// The length of the list is either 2 * #elements - 1 or 2 * #elements (a separator for each
    /// element or for each element but the last one).
    ///
    /// `should_stop` is a predicate to decide how to proceed in case an element or a separator
    /// can't be parsed, according to the current token.
    /// When parsing an element:
    /// If it returns true, the parsing of the list stops. If it returns false, the current token
    /// is skipped and we try to parse an element again.
    /// When parsing a separator:
    /// If it returns true, the parsing of the list stops. If it returns false, a missing separator
    /// is added and we continue to try to parse another element (with the same token).
    fn parse_separated_list<
        Element: TypedSyntaxNode,
        Separator: syntax::node::Terminal,
        ElementOrSeparatorGreen,
    >(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> Option<Element::Green>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &'static str,
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
                    self.skip_token(ParserDiagnosticKind::SkippedElement {
                        element_name: expected_element,
                    });
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
                    ParserDiagnosticKind::MissingToken(Separator::KIND),
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
        self.last_trivia_length =
            self.next_terminal.trailing_trivia.iter().map(|y| y.0.width(self.db)).sum();
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
            span: TextSpan { start: TextOffset(diag_start), end: TextOffset(diag_end) },
        });
    }

    /// Skips the current token, reports the given diagnostic and returns missing kind of the
    /// expected terminal.
    fn skip_token_and_return_missing<ExpectedTerminal: syntax::node::Terminal>(
        &mut self,
        diagnostic: ParserDiagnosticKind,
    ) -> ExpectedTerminal::Green {
        self.skip_token(diagnostic);
        ExpectedTerminal::missing(self.db)
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
        assert_eq!(token.kind, Terminal::KIND);
        self.add_trivia_to_terminal::<Terminal>(token)
    }

    /// If the current terminal is of kind `Terminal`, returns its Green wrapper. Otherwise, returns
    /// None.
    /// Note that this function should not be called for 'TerminalIdentifier' -
    /// try_parse_identifier() should be used instead.
    fn try_parse_token<Terminal: syntax::node::Terminal>(&mut self) -> Option<Terminal::Green> {
        if Terminal::KIND == self.peek().kind { Some(self.take::<Terminal>()) } else { None }
    }

    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns Token::Missing.
    ///
    /// Note that this function should not be called for 'TerminalIdentifier' - parse_identifier()
    /// should be used instead.
    fn parse_token<Terminal: syntax::node::Terminal>(&mut self) -> Terminal::Green {
        match self.try_parse_token::<Terminal>() {
            Some(green) => green,
            None => self.create_and_report_missing_terminal::<Terminal>(),
        }
    }
}

/// Controls whether Lbrace (`{`) is allowed in the expression.
///
/// Lbrace is always allowed in sub-expressions (e.g. in parenthesized expression). For example,
/// while `1 + MyStruct { ... }` may not be valid, `1 + (MyStruct { ... })` is always ok.
///
/// This can be used to parse the argument of a `match` statement,
/// so that the `{` that opens the `match` body is not confused with other potential uses of
/// `{`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LbraceAllowed {
    Forbid,
    Allow,
}
