#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

use std::mem;

use diagnostics::Diagnostics;
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
use crate::lexer::{Lexer, TerminalWithKind};
use crate::operators::{get_binary_operator_precedence, get_unary_operator_precedence};
use crate::ParserDiagnostic;

// TODO(yuval): add diagnostics.

pub struct Parser<'a> {
    db: &'a dyn SyntaxGroup,
    file_id: FileId,
    lexer: Lexer<'a>,
    /// The next terminal to handle.
    next_terminal: TerminalWithKind,
    skipped_terminals: Vec<GreenId>,
    /// The current offset, excluding the current terminal.
    offset: u32,
    /// The width of the current terminal being handled.
    current_width: u32,
    diagnostics: &'a mut Diagnostics<ParserDiagnostic>,
}

/// Fields for a terminal node. See Parser::unpack_terminal.
struct UnpackedTerminal {
    leading_trivia: Vec<GreenId>,
    token: GreenId,
    trailing_trivia: Vec<GreenId>,
    width: u32,
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
            skipped_terminals: Vec::new(),
            offset: 0,
            current_width: 0,
            diagnostics,
        };
        let green = parser.parse_syntax_file();
        SyntaxFile::from_syntax_node(db, SyntaxNode::new_root(db, green))
    }

    /// Returns a GreenId of an ExprMissing and adds a diagnostic describing it.
    fn create_and_report_missing(&mut self, missing_kind: ParserDiagnosticKind) -> GreenId {
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: missing_kind,
            span: TextSpan {
                start: TextOffset((self.offset + self.current_width) as usize),
                end: TextOffset(
                    (self.offset + self.current_width + self.peek().terminal.width(self.db))
                        as usize,
                ),
            },
        });
        ExprMissing::new_green(self.db)
    }

    pub fn parse_syntax_file(mut self) -> GreenId {
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

        let eof = self.add_skipped_to_terminal(self.next_terminal.terminal);
        SyntaxFile::new_green(self.db, items, eof)
    }

    // ------------------------------- Top level items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]).
    /// If can't parse as a top level item, keeps skipping tokens until it can.
    /// Returns None only when it reaches EOF.
    pub fn try_parse_top_level_item(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Module => Some(self.expect_module()),
            TokenKind::Struct => Some(self.expect_struct()),
            TokenKind::Extern => Some(self.expect_extern_item()),
            TokenKind::Function => Some(self.expect_function()),
            TokenKind::Use => Some(self.expect_use()),
            _ => None,
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: mod<Identifier>\{<ItemList>\}
    fn expect_module(&mut self) -> GreenId {
        let module_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let semicolon = self.parse_token(TokenKind::Semicolon);
        ItemModule::new_green(self.db, module_kw, name, semicolon)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: struct<Identifier><ParamListBraced>
    fn expect_struct(&mut self) -> GreenId {
        let struct_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        // TODO(yuval): support generics
        let lbrace = self.parse_token(TokenKind::LBrace);
        let members = self.parse_param_list(TokenKind::RBrace);
        let rbrace = self.parse_token(TokenKind::RBrace);
        ItemStruct::new_green(self.db, struct_kw, name, lbrace, members, rbrace)
    }

    /// Expected pattern: <ParenthesizedParamList><ReturnTypeClause>
    fn expect_function_signature(&mut self) -> GreenId {
        // TODO(yuval): support generics
        let lparen = self.parse_token(TokenKind::LParen);
        let params = self.parse_param_list(TokenKind::RParen);
        let rparen = self.parse_token(TokenKind::RParen);
        let return_type_clause = self.parse_option_return_type_clause();
        FunctionSignature::new_green(self.db, lparen, params, rparen, return_type_clause)
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: extern(<FunctionSignature>|type<Identifier>);
    fn expect_extern_item(&mut self) -> GreenId {
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
            }
            _ => {
                let type_kw = self.parse_token(TokenKind::Type);
                let name = self.parse_token(TokenKind::Identifier);
                let semicolon = self.parse_token(TokenKind::Semicolon);
                // If the next token is not type, assume it is missing.
                ItemExternType::new_green(self.db, extern_kw, type_kw, name, semicolon)
            }
        }
    }

    /// Assumes the current token is Use.
    /// Expected pattern: use<Path>;
    fn expect_use(&mut self) -> GreenId {
        ItemUse::new_green(
            self.db,
            self.take(),                            // usekw
            self.parse_path(),                      // name
            self.parse_token(TokenKind::Semicolon), // semicolon
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    fn expect_function(&mut self) -> GreenId {
        let function_kw = self.take();
        let name = self.parse_token(TokenKind::Identifier);
        let signature = self.expect_function_signature();
        let function_body = self.parse_block();
        ItemFreeFunction::new_green(self.db, function_kw, name, signature, function_body)
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::LBrace => Some(self.expect_block()),
            TokenKind::Match => Some(self.expect_match_expr()),
            _ => self.try_parse_simple_expression(MAX_PRECEDENCE),
        }
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or a node
    /// with kind ExprMissing if an expression can't be parsed.
    pub fn parse_expr(&mut self) -> GreenId {
        match self.try_parse_expr() {
            Some(green) => green,
            None => self.create_and_report_missing(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or None if such an expression can't be parsed.
    fn try_parse_simple_expression(&mut self, parent_precedence: usize) -> Option<GreenId> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            let unary_op = self.take();
            let expr = self.parse_simple_expression(precedence);
            ExprUnary::new_green(self.db, unary_op, expr)
        } else {
            self.try_parse_atom()?
        };

        while let Some(precedence) = get_binary_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            let op = self.take();
            let rhs = self.parse_simple_expression(precedence);
            expr = ExprBinary::new_green(self.db, expr, op, rhs);
        }
        Some(expr)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or ExprMissing if such an expression can't be parsed.
    fn parse_simple_expression(&mut self, parent_precedence: usize) -> GreenId {
        match self.try_parse_simple_expression(parent_precedence) {
            Some(green) => green,
            None => self.create_and_report_missing(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprStructCtorCall|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_atom(&mut self) -> Option<GreenId> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            TokenKind::Identifier => {
                let path = self.expect_path();
                match self.peek().kind {
                    TokenKind::LParen => Some(self.expect_function_call(path)),
                    TokenKind::LBrace => Some(self.expect_constructor_call(path)),
                    _ => Some(path),
                }
            }
            TokenKind::False | TokenKind::True | TokenKind::LiteralNumber => {
                Some(ExprLiteral::new_green(self.db, self.take()))
            }
            TokenKind::LParen => Some(self.expect_parenthesized_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_type_expr(&mut self) -> Option<GreenId> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            TokenKind::Identifier => Some(self.expect_path()),
            TokenKind::LParen => Some(self.expect_parenthesized_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    fn expect_expression_list_parenthesized(&mut self) -> GreenId {
        let lparen = self.take();
        let expression_list = self.parse_separated_list(
            Self::try_parse_expr,
            TokenKind::Comma,
            TokenKind::RParen,
            ExprList::new_green,
        );
        let rparen = self.parse_token(TokenKind::RParen);
        ExprListParenthesized::new_green(self.db, lparen, expression_list, rparen)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<StructArgList>\}
    fn expect_struct_ctor_argument_list_braced(&mut self) -> GreenId {
        let lbrace = self.take();
        let arg_list = self.parse_separated_list(
            Self::try_parse_struct_ctor_argument,
            TokenKind::Comma,
            TokenKind::RBrace,
            StructArgList::new_green,
        );
        let rbrace = self.parse_token(TokenKind::RBrace);

        ArgListBraced::new_green(self.db, lbrace, arg_list, rbrace)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: <ExprListParenthesized>
    fn expect_function_call(&mut self, path: GreenId) -> GreenId {
        let func_name = path;
        let parenthesized_args = self.expect_expression_list_parenthesized();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: <ExprListBraced>
    fn expect_constructor_call(&mut self, path: GreenId) -> GreenId {
        let ctor_name = path;
        let args = self.expect_struct_ctor_argument_list_braced();
        ExprStructCtorCall::new_green(self.db, ctor_name, args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \((<expr>,)*<expr>?\)
    /// Returns a GreenId of a node with kind ExprParenthesized|ExprTuple.
    fn expect_parenthesized_expr(&mut self) -> GreenId {
        let lparen = self.take();

        let expr_list_green = self.parse_separated_list(
            Self::try_parse_expr,
            TokenKind::Comma,
            TokenKind::RParen,
            ExprList::new_green,
        );

        match self.db.lookup_intern_green(expr_list_green) {
            GreenNode::Internal(list_internal) if list_internal.children.len() == 1 => {
                // We have exactly one item and no separator --> This is not a tuple.
                let expr = list_internal.children[0];
                let rparen = self.parse_token(TokenKind::RParen);
                ExprParenthesized::new_green(self.db, lparen, expr, rparen)
            }
            GreenNode::Internal(_list_internal) => {
                let rparen = self.parse_token(TokenKind::RParen);
                ExprTuple::new_green(self.db, lparen, expr_list_green, rparen)
            }
            GreenNode::Token(_) => {
                // This should never happen.
                panic!("Unexpected token. Expected an internal node")
            }
        }
    }

    /// Assumes the current token is DotDot.
    /// Expected pattern: \.\.<Expr>
    fn expect_struct_argument_tail(&mut self) -> GreenId {
        let dotdot = self.take(); // ..
        // TODO(yuval): consider changing this to SimpleExpr once it exists.
        let expr = self.parse_expr();
        StructArgTail::new_green(self.db, dotdot, expr)
    }

    // For the similar syntax in Rust, see
    // https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax.
    /// Like parse_argument, but also allows a struct-arg-tail, e.g. 'let s2 = S{"s2", ..s1};'
    /// Returns a GreenId of a node with kind StructArgSingle|StructArgTail.
    fn try_parse_struct_ctor_argument(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::DotDot => Some(self.expect_struct_argument_tail()),
            TokenKind::Identifier => Some(self.expect_argument_single()),
            _ => None,
        }
    }

    /// Returns a GreenId of a node with kind StructArgExpr or OptionStructArgExprEmpty if an
    /// argument expression (":<value>") can't be parsed.
    fn parse_option_struct_arg_expression(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Colon {
            let colon = self.take();
            let value = self.parse_expr();
            StructArgExpr::new_green(self.db, colon, value)
        } else {
            OptionStructArgExprEmpty::new_green(self.db)
        }
    }

    /// Returns a GreenId of a node with kind StructArgSingle.
    fn expect_argument_single(&mut self) -> GreenId {
        let identifier = self.take();
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        StructArgSingle::new_green(self.db, identifier, struct_arg_expr)
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<Statement>*\}
    fn expect_block(&mut self) -> GreenId {
        let lbrace = self.take();
        let statements =
            self.parse_list(Self::try_parse_statement, TokenKind::RBrace, StatementList::new_green);
        let rbrace = self.parse_token(TokenKind::RBrace);
        ExprBlock::new_green(self.db, lbrace, statements, rbrace)
    }
    /// Returns a GreenId of a node with kind ExprBlock or None if a block can't be parsed.
    fn try_parse_block(&mut self) -> Option<GreenId> {
        if self.peek().kind == TokenKind::LBrace { Some(self.expect_block()) } else { None }
    }
    /// Returns a GreenId of a node with kind ExprBlock or ExprMissing if a block can't be parsed.
    fn parse_block(&mut self) -> GreenId {
        match self.try_parse_block() {
            Some(green) => green,
            None => self.create_and_report_missing(ParserDiagnosticKind::MissingBlock),
        }
    }

    /// Assumes the current token is Match.
    /// Expected pattern: match \{<MatchArm>*\}
    fn expect_match_expr(&mut self) -> GreenId {
        let match_kw = self.take();
        // TODO(yuval): change to simple expression.
        let expr = self.parse_path();
        let lbrace = self.parse_token(TokenKind::LBrace);
        let match_arms = self.parse_separated_list(
            Self::try_parse_match_arm,
            TokenKind::Comma,
            TokenKind::RBrace,
            MatchArms::new_green,
        );
        let rbrace = self.parse_token(TokenKind::RBrace);
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, match_arms, rbrace)
    }

    /// Returns a GreenId of a node with a MatchArm kind or None if a match arm can't be parsed.
    pub fn try_parse_match_arm(&mut self) -> Option<GreenId> {
        let pattern = self.try_parse_pattern()?;
        let arrow = self.parse_token(TokenKind::MatchArrow);
        let expr = self.parse_expr();
        Some(MatchArm::new_green(self.db, pattern, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see [syntax::node::ast::Pattern]) or
    /// None if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> Option<GreenId> {
        // TODO(yuval): Support more options.
        match self.peek().kind {
            TokenKind::LiteralNumber => Some(ExprLiteral::new_green(self.db, self.take())),
            TokenKind::Underscore => Some(self.take()),
            _ => None,
        }
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see [syntax::node::ast::Statement]) or
    /// None if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Let => {
                let let_kw = self.take();
                // TODO(yuval): support patterns instead of only an identifier.
                let identifier = self.parse_token(TokenKind::Identifier);
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token(TokenKind::Eq);
                let expr = self.parse_expr();
                let semicolon = self.parse_token(TokenKind::Semicolon);
                Some(StatementLet::new_green(
                    self.db,
                    let_kw,
                    identifier,
                    type_clause,
                    eq,
                    expr,
                    semicolon,
                ))
            }
            TokenKind::Return => {
                let return_kw = self.take();
                let expr = self.parse_expr();
                let semicolon = self.parse_token(TokenKind::Semicolon);
                Some(StatementReturn::new_green(self.db, return_kw, expr, semicolon))
            }
            _ => match self.try_parse_expr() {
                None => None,
                Some(expr) => {
                    let optional_semicolon = if self.peek().kind == TokenKind::Semicolon {
                        self.take()
                    } else {
                        OptionSemicolonEmpty::new_green(self.db)
                    };
                    Some(StatementExpr::new_green(self.db, expr, optional_semicolon))
                }
            },
        }
    }

    /// Returns a GreenId of a node with kind TypeClause or OptionTypeClauseEmpty if a type clause
    /// can't be parsed.
    fn parse_option_type_clause(&mut self) -> GreenId {
        match self.try_parse_type_clause() {
            Some(green) => green,
            None => OptionTypeClauseEmpty::new_green(self.db),
        }
    }

    fn parse_type_clause(&mut self) -> GreenId {
        match self.try_parse_type_clause() {
            Some(green) => green,
            None => NonOptionTypeClauseMissing::new_green(self.db),
        }
    }
    fn try_parse_type_clause(&mut self) -> Option<GreenId> {
        if self.peek().kind == TokenKind::Colon {
            let colon = self.take();
            let ty = self.try_parse_type_expr().unwrap_or_else(|| ExprMissing::new_green(self.db));
            Some(TypeClause::new_green(self.db, colon, ty))
        } else {
            None
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_option_return_type_clause(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Arrow {
            let arrow = self.take();
            let return_type =
                self.try_parse_type_expr().unwrap_or_else(|| ExprMissing::new_green(self.db));
            ReturnTypeClause::new_green(self.db, arrow, return_type)
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db)
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self, closing_token: TokenKind) -> GreenId {
        self.parse_separated_list(
            Self::try_parse_param,
            TokenKind::Comma,
            closing_token,
            ParamList::new_green,
        )
    }

    /// Returns a GreenId of a node with kind Param or None if a parameter can't be parsed.
    fn try_parse_param(&mut self) -> Option<GreenId> {
        let identifier = self.try_parse_token(TokenKind::Identifier)?;
        let type_clause = self.parse_type_clause();
        Some(Param::new_green(self.db, identifier, type_clause))
    }

    /// Assumes the first token is Identifier.
    /// Expected pattern: <PathSegment>(::<PathSegment>)*
    /// Returns a GreenId of a node with kind ExprPath.
    fn expect_path(&mut self) -> GreenId {
        // Initialize the list with the first path segment.
        let mut children: Vec<GreenId> = vec![self.expect_path_segment()];
        loop {
            if self.peek().kind == TokenKind::ColonColon {
                // There might not be any additional item, so this might be skipped eventually.
                // Therefore, we must take_raw() and not take().
                let pending_separator = self.take_raw();
                if self.peek().kind == TokenKind::Identifier {
                    self.add_skipped_to_terminal(pending_separator);
                    children.push(pending_separator); // ::
                    children.push(self.expect_path_segment()); // path segment
                } else {
                    self.skipped_terminals.push(pending_separator);
                }
            } else if self.peek().kind == TokenKind::Identifier {
                children.push(GreenId::missing_token(self.db)); // missing separator
                children.push(self.expect_path_segment()); // path segment
            } else {
                break;
            }
        }
        ExprPath::new_green(self.db, children)
    }
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Identifier {
            self.expect_path()
        } else {
            self.create_and_report_missing(ParserDiagnosticKind::MissingPath)
        }
    }

    /// Assumes the current token is Identifier.
    /// Expected pattern: <Identifier><GenericArgs>
    fn expect_path_segment(&mut self) -> GreenId {
        let identifier = self.take();
        // TODO(yuval): support generics.
        let generic_args = OptionGenericArgsEmpty::new_green(self.db);
        PathSegment::new_green(self.db, identifier, generic_args)
    }

    // ------------------------------- Helpers -------------------------------

    /// Parses a list of items (without separators), where the items are parsed using
    /// `try_parse_list_item`. The `closing` token indicates when to stop parsing.
    /// Creates a node using the given `new_green` function and returns it. If it can't parse an
    /// item and the current token is not `closing`, skips the current token and tries again.
    // TODO(yuval): we want more advanced logic here - decide if token is missing or skipped
    // according to context. Same for the other list-parsing functions.
    fn parse_list(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<GreenId>,
        closing: TokenKind,
        new_green: fn(&dyn SyntaxGroup, Vec<GreenId>) -> GreenId,
    ) -> GreenId {
        let mut children: Vec<GreenId> = Vec::new();
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
    fn parse_separated_list(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<GreenId>,
        separator: TokenKind,
        closing: TokenKind,
        new_green: fn(&dyn SyntaxGroup, Vec<GreenId>) -> GreenId,
    ) -> GreenId {
        let mut children: Vec<GreenId> = Vec::new();
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let item = try_parse_list_item(self);
            // None means try_parse_list_item could not parse the next tokens as the expected item.
            match item {
                None => {
                    self.skip_token();
                }
                Some(green) => {
                    let separator = if self.peek().kind == separator {
                        self.take()
                    } else {
                        GreenId::missing_token(self.db)
                    };

                    children.push(green);
                    children.push(separator);
                }
            }
        }
        if let Some(last) = children.last() {
            if last.get_token_kind(self.db) == Some(TokenKind::Missing) {
                children.pop();
            }
        }
        new_green(self.db, children)
    }

    /// Peeks at the next terminal from the Lexer without taking it.
    fn peek(&self) -> &TerminalWithKind {
        &self.next_terminal
    }

    /// Takes a terminal from the Lexer and places it in self.next_terminal.
    fn take_raw(&mut self) -> GreenId {
        self.offset += self.current_width;
        self.current_width = self.next_terminal.terminal.width(self.db);
        let next_terminal = self.lexer.next().unwrap();
        std::mem::replace(&mut self.next_terminal, next_terminal).terminal
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
    fn take(&mut self) -> GreenId {
        let token = self.take_raw();
        self.add_skipped_to_terminal(token)
    }

    /// Unpack the given GreenId to an internal node of the expected kind, or panic if it's not of
    /// this kind.
    fn unpack_internal_node(&self, green: GreenId, expected_kind: SyntaxKind) -> GreenNodeInternal {
        match self.db.lookup_intern_green(green) {
            GreenNode::Internal(internal) if internal.kind == expected_kind => internal,
            GreenNode::Internal(internal) => {
                panic!("Unexpected node kind {:?}, expected {:?}", internal.kind, expected_kind);
            }
            GreenNode::Token(_) => {
                panic!("Unexpected token, expected internal node of kind {:?}", expected_kind);
            }
        }
    }

    /// Unpacks a terminal from its GreenID to its fields and width.
    fn unpack_terminal(&self, terminal: GreenId) -> UnpackedTerminal {
        let width = terminal.width(self.db);
        let terminal_internal = self.unpack_internal_node(terminal, SyntaxKind::Terminal);

        let leading_trivia =
            self.unpack_internal_node(terminal_internal.children[0], SyntaxKind::Trivia).children;
        let token = terminal_internal.children[1];
        let trailing_trivia =
            self.unpack_internal_node(terminal_internal.children[2], SyntaxKind::Trivia).children;
        UnpackedTerminal { leading_trivia, token, trailing_trivia, width }
    }

    /// Builds a new terminal to replace the given terminal by gluing the recently skipped terminals
    /// to the given terminal as extra leading trivia.
    fn add_skipped_to_terminal(&mut self, terminal: GreenId) -> GreenId {
        if self.skipped_terminals.is_empty() {
            return terminal;
        }

        let mut total_width = 0;

        // Collect all the skipped terminals.
        let unpacked_terminals: Vec<UnpackedTerminal> = mem::take(&mut self.skipped_terminals)
            .into_iter()
            .map(|t| self.unpack_terminal(t))
            .collect();

        // Extract current leading trivia of the given terminal.
        let terminal_internal = self.unpack_internal_node(terminal, SyntaxKind::Terminal);
        let leading_trivia = terminal_internal.children[0];
        let leading_trivia_internal = self.unpack_internal_node(leading_trivia, SyntaxKind::Trivia);

        // Build a replacement for the leading trivia.
        let mut new_leading_trivia_children = vec![];
        for UnpackedTerminal { leading_trivia, token, trailing_trivia, width } in unpacked_terminals
        {
            new_leading_trivia_children.extend(leading_trivia);
            new_leading_trivia_children.push(TriviumSkippedToken::new_green(self.db, token));
            new_leading_trivia_children.extend(trailing_trivia);
            total_width += width;
        }
        new_leading_trivia_children.extend(leading_trivia_internal.children);

        self.report_skipped_diagnostics(total_width, &new_leading_trivia_children);

        let new_leading_trivia = Trivia::new_green(self.db, new_leading_trivia_children);

        // Build a replacement for the current terminal, with the new leading trivia instead of the
        // old one.
        let token = terminal_internal.children[1];
        let trailing_trivia = terminal_internal.children[2];
        Terminal::new_green(self.db, new_leading_trivia, token, trailing_trivia)
    }

    /// Given a sequence of Trivia nodes, finds consecutive SkippedTrivia and reports to
    /// diagnostics.
    fn report_skipped_diagnostics(&mut self, total_width: u32, trivia: &[GreenId]) {
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
            let width = trivium.width(self.db);
            match self.db.lookup_intern_green(trivium) {
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
    fn try_parse_token(&mut self, token_kind: TokenKind) -> Option<GreenId> {
        if self.peek().kind == token_kind {
            Some(self.take())
        } else {
            // TODO(yuval): report to diagnostics.
            None
        }
    }
    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns Token::Missing.
    fn parse_token(&mut self, token_kind: TokenKind) -> GreenId {
        match self.try_parse_token(token_kind) {
            Some(green) => green,
            None => GreenId::missing_token(self.db),
        }
    }
}
