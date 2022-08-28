#[cfg(test)]
#[path = "parser_test.rs"]
mod tests;

use std::mem;

use filesystem::ids::FileId;
use syntax::node::ast::*;
use syntax::node::db::GreenInterner;
use syntax::node::green::{GreenNode, GreenNodeInternal};
use syntax::node::ids::GreenId;
use syntax::node::kind::SyntaxKind;
use syntax::node::{SyntaxNode, TypedSyntaxNode};
use syntax::token::TokenKind;

use crate::lexer::{Lexer, TerminalWithKind};
use crate::operators::{get_binary_operator_precedence, get_unary_operator_precedence};

// TODO(yuval): add diagnostics.

pub struct Parser<'a> {
    db: &'a dyn GreenInterner,
    lexer: Lexer<'a>,
    /// The next terminal to handle.
    next_terminal: TerminalWithKind,
    skipped_tokens: Vec<GreenId>,
    /// The current offset, including the current terminal.
    offset: u32,
    /// The width of the current terminal being handled.
    current_width: u32,
}

pub struct GreenCompilationUnit {
    pub root: GreenId,
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
    pub fn from_text(db: &'a dyn GreenInterner, source: FileId, text: &'a str) -> Parser<'a> {
        let mut lexer = Lexer::from_text(db, source, text);
        let next_terminal = lexer.next().unwrap();
        Parser { lexer, next_terminal, skipped_tokens: Vec::new(), db, offset: 0, current_width: 0 }
    }

    pub fn parse_syntax_file(&mut self) -> SyntaxFile {
        let items = self.parse_list(
            Self::try_parse_top_level_item,
            TokenKind::EndOfFile,
            ItemList::new_green,
        );
        while self.peek().kind != TokenKind::EndOfFile {
            self.skip_token();
        }

        // Fix widths in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.current_width = 0; // EOF is of 0 width

        let eof = self.add_skipped_to_terminal(self.next_terminal.terminal);
        SyntaxFile::from_syntax_node(
            self.db,
            SyntaxNode::new_root(SyntaxFile::new_green(self.db, items, eof)),
        )
    }

    // ------------------------------- Top level items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]).
    /// If can't parse as a top level item, keeps skipping tokens until it can.
    /// Returns None only when it reaches EOF.
    pub fn try_parse_top_level_item(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Module => Some(self.expect_module()),
            TokenKind::Struct => Some(self.expect_struct()),
            TokenKind::Function => Some(self.expect_function()),
            _ => None,
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: mod<Identifier>\{<ItemList>\}
    fn expect_module(&mut self) -> GreenId {
        ItemModule::new_green(
            self.db,
            self.take(),                             // module keyword
            self.parse_token(TokenKind::Identifier), // name
            self.parse_token(TokenKind::Semi),       // semicolon
        )
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: struct<Identifier><ParamListBraced>
    fn expect_struct(&mut self) -> GreenId {
        ItemStruct::new_green(
            self.db,
            self.take(),                             // struct keyword
            self.parse_token(TokenKind::Identifier), // name
            // TODO(yuval): support generics
            self.parse_token(TokenKind::LBrace),      // {
            self.parse_param_list(TokenKind::RBrace), // members
            self.parse_token(TokenKind::RBrace),      // }
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: fn<Identifier><ParenthesizedParamList><ReturnTypeClause>
    fn expect_function_signature(&mut self) -> GreenId {
        FunctionSignature::new_green(
            self.db,
            self.take(),                             // function keyword
            self.parse_token(TokenKind::Identifier), // name
            // TODO(yuval): support generics
            self.parse_token(TokenKind::LParen),      // (
            self.parse_param_list(TokenKind::RParen), // params
            self.parse_token(TokenKind::RParen),      // )
            self.parse_option_return_type_clause(),   // return type clause
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    fn expect_function(&mut self) -> GreenId {
        ItemFunction::new_green(
            self.db,
            self.expect_function_signature(), // signature
            self.parse_block(),               // function body
        )
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or None if
    /// an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::LBrace => Some(self.expect_block()),
            _ => self.try_parse_simple_expression(MAX_PRECEDENCE),
        }
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]) or a node
    /// with kind ExprMissing if an expression can't be parsed.
    fn parse_expr(&mut self) -> GreenId {
        match self.try_parse_expr() {
            Some(green) => green,
            None => ExprMissing::new_green(self.db),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or None if such an expression can't be parsed.
    fn try_parse_simple_expression(&mut self, parent_precedence: usize) -> Option<GreenId> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            ExprUnary::new_green(
                self.db,
                self.take(),                              // unary operator
                self.parse_simple_expression(precedence), // expression
            )
        } else {
            self.try_parse_atom()?
        };

        while let Some(precedence) = get_binary_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            expr = ExprBinary::new_green(
                self.db,
                expr,                                     // LHS expression
                self.take(),                              // operator
                self.parse_simple_expression(precedence), // RHS expression
            );
        }
        Some(expr)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]), excluding
    /// ExprBlock, or ExprMissing if such an expression can't be parsed.
    fn parse_simple_expression(&mut self, parent_precedence: usize) -> GreenId {
        match self.try_parse_simple_expression(parent_precedence) {
            Some(green) => green,
            None => ExprMissing::new_green(self.db),
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

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    fn expect_expression_list_parenthesized(&mut self) -> GreenId {
        ExprListParenthesized::new_green(
            self.db,
            // left parenthesis
            self.take(),
            // Expression list
            self.parse_separated_list(
                Self::try_parse_expr,
                TokenKind::Comma,
                TokenKind::RParen,
                ExprList::new_green,
            ),
            // right parenthesis
            self.parse_token(TokenKind::RParen),
        )
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<StructArgList>\}
    fn expect_struct_ctor_argument_list_braced(&mut self) -> GreenId {
        ArgListBraced::new_green(
            self.db,
            // left brace
            self.take(),
            // Argument list
            self.parse_separated_list(
                Self::try_parse_struct_ctor_argument,
                TokenKind::Comma,
                TokenKind::RBrace,
                StructArgList::new_green,
            ),
            // right brace
            self.parse_token(TokenKind::RBrace),
        )
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: <ExprListParenthesized>
    fn expect_function_call(&mut self, path: GreenId) -> GreenId {
        ExprFunctionCall::new_green(
            self.db,
            path,                                        // function name
            self.expect_expression_list_parenthesized(), // (arguments)
        )
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: <ExprListBraced>
    fn expect_constructor_call(&mut self, path: GreenId) -> GreenId {
        ExprStructCtorCall::new_green(
            self.db,
            path,                                           // constructor name
            self.expect_struct_ctor_argument_list_braced(), // arguments
        )
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
                ExprParenthesized::new_green(
                    self.db,
                    lparen,                              // left parenthesis
                    list_internal.children[0],           // expression
                    self.parse_token(TokenKind::RParen), // right parenthesis
                )
            }
            GreenNode::Internal(_list_internal) => ExprTuple::new_green(
                self.db,
                lparen,                              // left parenthesis
                expr_list_green,                     // expressions
                self.parse_token(TokenKind::RParen), // right parenthesis
            ),
            GreenNode::Token(_) => {
                // This should never happen.
                panic!("Unexpected token. Expected an internal node")
            }
        }
    }

    /// Assumes the current token is DotDot.
    /// Expected pattern: \.\.<Expr>
    fn expect_struct_argument_tail(&mut self) -> GreenId {
        StructArgTail::new_green(
            self.db,
            self.take(), // ..
            // TODO(yuval): consider changing this to SimpleExpr once it exists.
            self.parse_expr(), // expression
        )
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
            StructArgExpr::new_green(
                self.db,
                self.take(),       // ':'
                self.parse_expr(), // value
            )
        } else {
            OptionStructArgExprEmpty::new_green(self.db)
        }
    }

    /// Returns a GreenId of a node with kind StructArgSingle.
    fn expect_argument_single(&mut self) -> GreenId {
        StructArgSingle::new_green(
            self.db,
            self.take(),                               // identifier
            self.parse_option_struct_arg_expression(), // :<expr>
        )
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: \{<Statement>*\}
    fn expect_block(&mut self) -> GreenId {
        ExprBlock::new_green(
            self.db,
            // left brace
            self.take(),
            // statements
            self.parse_list(Self::try_parse_statement, TokenKind::RBrace, StatementList::new_green),
            // right brace
            self.parse_token(TokenKind::RBrace),
        )
    }
    /// Returns a GreenId of a node with kind ExprBlock or None if a block can't be parsed.
    fn try_parse_block(&mut self) -> Option<GreenId> {
        if self.peek().kind == TokenKind::LBrace { Some(self.expect_block()) } else { None }
    }
    /// Returns a GreenId of a node with kind ExprBlock or ExprMissing if a block can't be parsed.
    fn parse_block(&mut self) -> GreenId {
        match self.try_parse_block() {
            Some(green) => green,
            None => ExprMissing::new_green(self.db),
        }
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see [syntax::node::ast::Statement]) or
    /// None if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Let => {
                Some(StatementLet::new_green(
                    self.db,
                    self.take(), // let keyword
                    // TODO(yuval): support patterns instead of only an identifier.
                    self.parse_token(TokenKind::Identifier), // identifier
                    self.parse_option_type_clause(),         // type clause
                    self.parse_token(TokenKind::Eq),         // '='
                    self.parse_expr(),                       // expression
                    self.parse_token(TokenKind::Semi),       // ';'
                ))
            }
            TokenKind::Return => {
                Some(StatementReturn::new_green(
                    self.db,
                    self.take(),                       // return keyword
                    self.parse_expr(),                 // expression
                    self.parse_token(TokenKind::Semi), // ';'
                ))
            }
            _ => match self.try_parse_expr() {
                None => None,
                Some(expr) => {
                    let optional_semi = if self.peek().kind == TokenKind::Semi {
                        self.take()
                    } else {
                        OptionSemicolonEmpty::new_green(self.db)
                    };
                    Some(StatementExpr::new_green(self.db, expr, optional_semi))
                }
            },
        }
    }

    /// Returns a GreenId of a node with kind TypeClause or OptionTypeClauseEmpty if a type clause
    /// can't be parsed.
    fn parse_option_type_clause(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Colon {
            TypeClause::new_green(
                self.db,
                self.take(), // ':'
                // TODO(yuval): support reacher types.
                self.parse_path(), // type
            )
        } else {
            OptionTypeClauseEmpty::new_green(self.db)
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_option_return_type_clause(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Arrow {
            ReturnTypeClause::new_green(
                self.db,
                self.take(), // '->'
                // TODO(yuval): support reacher types.
                self.parse_path(), // return type
            )
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
        Some(Param::new_green(
            self.db,
            self.try_parse_token(TokenKind::Identifier)?, // identifier
            self.parse_option_type_clause(),              // type_clause
        ))
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
                    self.skipped_tokens.push(pending_separator);
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
            ExprMissing::new_green(self.db)
        }
    }

    /// Assumes the current token is Identifier.
    /// Expected pattern: <Identifier><GenericArgs>
    fn expect_path_segment(&mut self) -> GreenId {
        PathSegment::new_green(
            self.db,
            // identifier
            self.take(),
            // TODO(yuval): support generics.
            // generic args
            OptionGenericArgsEmpty::new_green(self.db),
        )
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
        new_green: fn(&dyn GreenInterner, Vec<GreenId>) -> GreenId,
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
        new_green: fn(&dyn GreenInterner, Vec<GreenId>) -> GreenId,
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
        self.current_width = self.next_terminal.terminal.width(self.db);
        self.offset += self.current_width;
        let next_terminal = self.lexer.next().unwrap();
        std::mem::replace(&mut self.next_terminal, next_terminal).terminal
    }

    /// Skips a token. A skipped token is a token which is not expected where it is found. Skipping
    /// this token means reporting an error and ignoring it and continuing the compilation as if it
    /// wasn't there.
    fn skip_token(&mut self) {
        let token = self.take_raw();
        self.skipped_tokens.push(token);
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

    /// Builds a TriviumSkippedTerminal from the given `terminal` (same children, but with kind
    /// TriviumSkippedTerminal instead of Terminal).
    fn build_skipped_terminal(&self, terminal: GreenId) -> GreenId {
        let terminal_internal = self.unpack_internal_node(terminal, SyntaxKind::Terminal);

        TriviumSkippedTerminal::new_green(
            self.db,
            terminal_internal.children[0],
            terminal_internal.children[1],
            terminal_internal.children[2],
        )
    }

    /// Builds a new terminal to replace the given terminal by gluing the recently skipped terminals
    /// to the given terminal as extra leading trivia.
    fn add_skipped_to_terminal(&mut self, terminal: GreenId) -> GreenId {
        if self.skipped_tokens.is_empty() {
            return terminal;
        }

        let mut total_width = 0;

        // Collect all the skipped terminal with kind TriviumSkippedTerminal.
        let skipped_terminals: Vec<GreenId> = mem::take(&mut self.skipped_tokens)
            .into_iter()
            .map(|t| self.build_skipped_terminal(t))
            .collect();

        // Extract current leading trivia of the given terminal.
        let terminal_internal = self.unpack_internal_node(terminal, SyntaxKind::Terminal);
        let leading_trivia = terminal_internal.children[0];
        let leading_trivia_internal = self.unpack_internal_node(leading_trivia, SyntaxKind::Trivia);

        // Build a replacement for the leading trivia.
        let mut new_leading_trivia_children = vec![];
        for skipped in skipped_terminals {
            total_width += skipped.width(self.db);
            new_leading_trivia_children.push(skipped);
        }
        for trivium in leading_trivia_internal.children {
            new_leading_trivia_children.push(trivium);
        }
        let new_leading_trivia = Trivia::new_green(self.db, new_leading_trivia_children);

        let skipped_end = self.offset - self.current_width;
        // TODO(spapini): report to diagnostics.
        println!("Skipped tokens from: {} to: {}", skipped_end - total_width, skipped_end);

        // Build a replacement for the current terminal, with the new leading trivia instead of the
        // old one.
        Terminal::new_green(
            self.db,
            new_leading_trivia,
            terminal_internal.children[1],
            terminal_internal.children[2],
        )
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
