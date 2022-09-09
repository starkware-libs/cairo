#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

use std::mem;

use diagnostics::{Diagnostics, WithDiagnostics};
use filesystem::ids::FileId;
use filesystem::span::{TextOffset, TextSpan};
use syntax::node::ast::*;
use syntax::node::db::SyntaxGroup;
use syntax::node::green::{GreenNode, GreenNodeInternal};
use syntax::node::ids::GreenId;
use syntax::node::kind::SyntaxKind;
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
    skipped_terminals: Vec<LexerTerminal>,
    /// The current offset, excluding the current terminal.
    offset: u32,
    /// The width of the current terminal being handled.
    current_width: u32,
    diagnostics: Diagnostics<ParserDiagnostic>,
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
    /// Ctor.
    pub fn from_text(db: &'a dyn SyntaxGroup, file_id: FileId, text: &'a str) -> Parser<'a> {
        let mut lexer = Lexer::from_text(db, file_id, text);
        let next_terminal = lexer.next().unwrap();
        Parser {
            db,
            file_id,
            lexer,
            next_terminal,
            skipped_terminals: Vec::new(),
            offset: 0,
            current_width: 0,
            diagnostics: Diagnostics::new(),
        }
    }

    pub fn parse_syntax_file(mut self) -> WithDiagnostics<SyntaxFile, ParserDiagnostic> {
        let items = self.parse_list(
            Self::try_parse_top_level_item,
            TokenKind::EndOfFile,
            ItemList::new_green,
        );
        while self.peek().kind != TokenKind::EndOfFile {
            self.skip_token();
        }

        // Fix offset in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.offset += self.current_width;

        let eof = self.add_skipped_to_terminal(self.next_terminal.clone());
        let syntax_file = SyntaxFile::from_syntax_node(
            self.db,
            SyntaxNode::new_root(self.db, SyntaxFile::new_green(self.db, items, eof)),
        );
        WithDiagnostics { value: syntax_file, diagnostics: self.diagnostics }
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
        // TODO(yuval): support generics
        let lbrace = self.parse_token(TokenKind::LBrace);
        let members = self.parse_param_list(TokenKind::RBrace);
        let rbrace = self.parse_token(TokenKind::RBrace);
        ItemStruct::new_green(self.db, struct_kw, name, lbrace, members, rbrace)
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
                let signature = self.expect_function_signature();
                let semicolon = self.parse_token(TokenKind::Semicolon);
                ItemExternFunction::new_green(
                    self.db,
                    extern_kw,
                    function_kw,
                    name,
                    signature,
                    semicolon,
                )
                .into()
            }
            _ => {
                let type_kw = self.parse_token(TokenKind::Type);
                let name = self.parse_token(TokenKind::Identifier);
                let semicolon = self.parse_token(TokenKind::Semicolon);
                // If the next token is not type, assume it is missing.
                ItemExternType::new_green(self.db, extern_kw, type_kw, name, semicolon).into()
            }
        }
    }

    /// Assumes the current token is Use.
    /// Expected pattern: use<Path>;
    fn expect_use(&mut self) -> ItemUseGreen {
        ItemUse::new_green(
            self.db,
            self.take(),                            // usekw
            self.parse_path(),                      // name
            self.parse_token(TokenKind::Semicolon), // semicolon
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    fn expect_function(&mut self) -> ItemFreeFunctionGreen {
        let function_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let signature = self.expect_function_signature();
        let function_body = self.parse_block();
        ItemFreeFunction::new_green(self.db, function_kw, name, signature, function_body)
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<ExprGreen> {
        match self.peek().kind {
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
            None => ExprMissing::new_green(self.db).into(),
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
            None => ExprMissing::new_green(self.db).into(),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprStructCtorCall|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_atom(&mut self) -> Option<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            TokenKind::Identifier => {
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

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    fn expect_expression_list_parenthesized(&mut self) -> ExprListParenthesizedGreen {
        let lparen = self.take();
        let expression_list = ExprList::new_green(
            self.db,
            self.parse_separated_list(Self::try_parse_expr, TokenKind::Comma, TokenKind::RParen),
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
        let exprs: Vec<ExprListSingleGreen> =
            self.parse_separated_list(Self::try_parse_expr, TokenKind::Comma, TokenKind::RParen);
        let rparen = self.parse_token(TokenKind::RParen);

        if let [ExprListSingleGreen::Element(expr)] = &exprs[..] {
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
        let statements =
            self.parse_list(Self::try_parse_statement, TokenKind::RBrace, StatementList::new_green);
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

    fn parse_type_clause(&mut self) -> NonOptionTypeClauseGreen {
        match self.try_parse_type_clause() {
            Some(green) => green.into(),
            None => NonOptionTypeClauseMissing::new_green(self.db).into(),
        }
    }
    fn try_parse_type_clause(&mut self) -> Option<TypeClauseGreen> {
        if self.peek().kind == TokenKind::Colon {
            let colon = self.take();
            // TODO(yuval): support reacher types.
            let ty = self.parse_path();
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
            // TODO(yuval): support reacher types.
            let return_type = self.parse_path();
            ReturnTypeClause::new_green(self.db, arrow, return_type).into()
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self, closing_token: TokenKind) -> ParamListGreen {
        ParamList::new_green(
            self.db,
            self.parse_separated_list(Self::try_parse_param, TokenKind::Comma, closing_token),
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
        let mut children: Vec<ExprPathSingleGreen> = vec![self.parse_path_segment().into()];
        while let Some(separator) = self.try_parse_token(TokenKind::ColonColon) {
            children.push(separator.into());
            if let Some(segment) = self.try_parse_path_segment() {
                children.push(segment.into());
            } else {
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
            PathSegment::new_green(
                self.db,
                Terminal::missing(self.db),
                OptionGenericArgsEmpty::new_green(self.db).into(),
            )
        })
    }

    // ------------------------------- Helpers -------------------------------

    /// Parses a list of items (without separators), where the items are parsed using
    /// `try_parse_list_item`. The `closing` token indicates when to stop parsing.
    /// Creates a node using the given `new_green` function and returns it. If it can't parse an
    /// item and the current token is not `closing`, skips the current token and tries again.
    // TODO(yuval): we want more advanced logic here - decide if token is missing or skipped
    // according to context. Same for the other list-parsing functions.
    fn parse_list<ElementGreen, ListGreen>(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<ElementGreen>,
        closing: TokenKind,
        new_green: fn(&dyn SyntaxGroup, Vec<ElementGreen>) -> ListGreen,
    ) -> ListGreen {
        let mut children: Vec<ElementGreen> = Vec::new();
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let item = try_parse_list_item(self);
            if let Some(green) = item {
                children.push(green);
            } else {
                self.skip_token();
            }
        }
        new_green(self.db, children)
    }

    /// Parses a list of items with `separator`s, where the items are parsed using
    /// `try_parse_list_item`. The separator may or may not appear in the end of the list.
    /// The `closing` token indicates when to stop parsing.
    /// Creates a node using the given `new_green` function and returns it. This list contains
    /// alternating children: [item, separator, item, separator, ...]. Both items and separators
    /// may be missing. The length of the list is either 2 * #items - 1 or 2 * #items (a
    /// separator for each item or for each item but the last one).
    fn parse_separated_list<ElementGreen, SingleGreen>(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<ElementGreen>,
        separator: TokenKind,
        closing: TokenKind,
    ) -> Vec<SingleGreen>
    where
        SingleGreen: From<TerminalGreen> + From<ElementGreen>,
    {
        let mut children: Vec<SingleGreen> = Vec::new();
        let mut last_is_missing = false;
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let item = try_parse_list_item(self);
            // None means try_parse_list_item could not parse the next tokens as the expected item.
            match item {
                None => {
                    self.skip_token();
                }
                Some(green) => {
                    let separator = if self.peek().kind == separator {
                        last_is_missing = false;
                        self.take()
                    } else {
                        last_is_missing = true;
                        TerminalGreen(GreenId::missing_token(self.db))
                    };

                    children.push(green.into());
                    children.push(separator.into());
                }
            }
        }
        if last_is_missing {
            children.pop();
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
    fn skip_token(&mut self) {
        let token = self.take_raw();
        self.skipped_terminals.push(token);
    }

    /// Takes a token from the Lexer and place it in self.current. If tokens were skipped, glue them
    /// to this token as leading trivia.
    fn take(&mut self) -> TerminalGreen {
        let token = self.take_raw();
        self.add_skipped_to_terminal(token)
    }

    /// Builds a new terminal to replace the given terminal by gluing the recently skipped terminals
    /// to the given terminal as extra leading trivia.
    fn add_skipped_to_terminal(&mut self, lexer_terminal: LexerTerminal) -> TerminalGreen {
        if self.skipped_terminals.is_empty() {
            let LexerTerminal { token, kind: _, leading_trivia, trailing_trivia } = lexer_terminal;
            return Terminal::new_green(
                self.db,
                Trivia::new_green(self.db, leading_trivia),
                token,
                Trivia::new_green(self.db, trailing_trivia),
            );
        }

        let mut total_width: u32 = 0;

        // Collect all the skipped terminals.
        let skipped_terminals = mem::take(&mut self.skipped_terminals);

        // Extract current leading trivia of the given terminal.
        // let terminal_internal = self.unpack_internal_node(terminal, SyntaxKind::Terminal);
        // let leading_trivia = terminal_internal.children[0];
        // let leading_trivia_internal = self.unpack_internal_node(leading_trivia,
        // SyntaxKind::Trivia);

        // Build a replacement for the leading trivia.
        let mut new_leading_trivia_children = vec![];
        for lexer_terminal in skipped_terminals {
            total_width += lexer_terminal.width(self.db);
            let LexerTerminal { token, kind: _, leading_trivia, trailing_trivia } = lexer_terminal;
            new_leading_trivia_children.extend(leading_trivia);
            new_leading_trivia_children.push(TriviumSkippedToken::new_green(self.db, token).into());
            new_leading_trivia_children.extend(trailing_trivia);
        }

        let LexerTerminal { token, kind: _, leading_trivia, trailing_trivia } = lexer_terminal;
        new_leading_trivia_children.extend(leading_trivia);

        self.report_skipped_diagnostics(total_width, &new_leading_trivia_children);

        let new_leading_trivia = Trivia::new_green(self.db, new_leading_trivia_children);
        let new_trailing_trivia = Trivia::new_green(self.db, trailing_trivia);

        // Build a replacement for the current terminal, with the new leading trivia instead of the
        // old one.
        Terminal::new_green(self.db, new_leading_trivia, token, new_trailing_trivia)
    }

    /// Given a sequence of Trivia nodes, finds consecutive SkippedTrivia and reports to
    /// diagnostics.
    fn report_skipped_diagnostics(&mut self, total_width: u32, trivia: &[TriviumGreen]) {
        // Report diagnostics for each consecutive batch of skipped tokens.
        let mut append_diagnostic = |start, end| {
            if end > start {
                self.diagnostics.add(ParserDiagnostic {
                    file_id: self.file_id,
                    kind: ParserDiagnosticKind::SkippedTokens,
                    span: TextSpan { start, end },
                });
            }
        };
        // TODO(spapini): Clean up by always saving offsets as TextOffset, and possible not use
        // offset arithmetic, and instead, keep the correct TextOffset when generated.
        let mut current_start = TextOffset((self.offset - total_width) as usize);
        let mut current_offset = current_start;
        for trivium in trivia.iter().copied() {
            let width = trivium.0.width(self.db);
            match self.db.lookup_intern_green(trivium.0) {
                GreenNode::Internal(GreenNodeInternal {
                    kind: SyntaxKind::TriviumSkippedToken,
                    ..
                }) => {
                    current_offset = current_offset.add(width as usize);
                }
                _ => {
                    append_diagnostic(current_start, current_offset);
                    current_offset = current_offset.add(width as usize);
                    current_start = current_offset;
                }
            };
        }
        append_diagnostic(current_start, current_offset);
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
            None => TerminalGreen(GreenId::missing_token(self.db)),
        }
    }
}
