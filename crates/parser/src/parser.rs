#![allow(unused_imports)]
#![allow(dead_code)]
#[cfg(test)]
#[path = "parser_test.rs"]
mod tests;

use std::collections::HashMap;
use std::mem;

use filesystem::ids::FileId;
use smol_str::SmolStr;
use syntax::node::ast::StatementList;
use syntax::node::db::GreenInterner;
use syntax::node::green::{GreenNode, GreenNodeInternal};
use syntax::node::ids::GreenId;
use syntax::node::kind::SyntaxKind;
use syntax::token::{self, Token, TokenKind};

use crate::lexer::{Lexer, TerminalWithKind};

// TODO: add diagnostics.

pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    current: TerminalWithKind,
    skipped_tokens: Vec<GreenId>,
    // pub diagnostics: DiagnosticsBag,
    db: &'a dyn GreenInterner,
}

pub struct GreenCompilationUnit {
    pub root: GreenId,
}

// match_<something>: expects something to be present and returns a green ID of a node of the
// relevant kind. If it's not present - panics.
// Unless it panics - always returns some green ID which is not of a missing kind.
//
// parse_<something>: returns a green ID with a kind that represents 'something'. If 'something'
// can't be parsed, returns a green ID with the relevant missing/empty kind. Used when we expect
// 'something' to be present (can return a missing kind) or for an optional child (can return an
// empty kind).
// Unless it panics - always returns some green ID (could be of a missing kind).
//
// try_parse_<something>: if something is there, returns a green ID of a node
// of kind Something wrapped with Some(). If something is not there, returns None. used when
// something may or may not be there and we can act differently according to each case.
// Never panics or returns a green ID of a missing kind.

type FunctionPointer<'c> = fn(&mut Parser<'c>) -> GreenId;
const MAX_PRECEDENCE: usize = 10;
impl<'a> Parser<'a> {
    /// Ctor.
    pub fn from_text(db: &'a dyn GreenInterner, source: FileId, text: &'a str) -> Parser<'a> {
        let mut lexer = Lexer::from_text(db, source, text);
        let current = lexer.next().unwrap();
        Parser {
            lexer,
            current,
            skipped_tokens: Vec::new(), // , diagnostics: DiagnosticsBag::new()
            db,
        }
    }

    pub fn parse_compilation_unit(&mut self) -> GreenCompilationUnit {
        let root = self.parse_top_level_items();
        while self.peek().kind != TokenKind::EndOfFile {
            self.skip_token();
        }
        GreenCompilationUnit { root }
    }

    // Top level items

    /// Returns a GreenId of a node with kind ItemList.
    pub fn parse_top_level_items(&mut self) -> GreenId {
        self.parse_list(Self::try_parse_top_level_item, TokenKind::EndOfFile, SyntaxKind::ItemList)
    }

    /// Returns a GreenId of a node with an Item.* kind (see syntax::node::ast::Item).
    /// If can't parse as a top level item, keeps skipping tokens until it can.
    /// Returns None only when it reaches EOF.
    pub fn try_parse_top_level_item(&mut self) -> Option<GreenId> {
        let map: HashMap<TokenKind, FunctionPointer<'a>> = HashMap::from([
            (TokenKind::Module, Parser::match_module as FunctionPointer<'a>),
            (TokenKind::Struct, Parser::match_struct as FunctionPointer<'a>),
            (TokenKind::Function, Parser::match_function as FunctionPointer<'a>),
        ]);
        self.try_match_item(map, TokenKind::EndOfFile)
    }

    /// Matches one item using a match function from the given map, according to the current token.
    /// If the current token is not in the map, skips tokens until one is in the map or the
    /// terminating_token appears. If an item is parsed successfully, returns a GreenId of the node
    /// with the relevant SyntaxKind. If the terminating_token is reached, returns None.
    fn try_match_item(
        &mut self,
        match_functions_map: HashMap<TokenKind, fn(&mut Self) -> GreenId>,
        terminating_token: TokenKind,
    ) -> Option<GreenId> {
        while self.peek().kind != terminating_token && self.peek().kind != TokenKind::EndOfFile {
            if let Some(parse_item_function) = match_functions_map.get(&self.peek().kind) {
                return Some(parse_item_function(self));
            }
            self.skip_token();
            // TODO: report error to diagnostics.
        }
        None
    }

    /// Assumes the current token is Module.
    /// Expected pattern: mod<Identifier>\{<ItemList>\}
    /// Returns a GreenId of a node with kind ItemModule.
    fn match_module(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ItemModule,
            vec![
                self.take(),                             // module keyword
                self.match_token(TokenKind::Identifier), // name
                self.match_token(TokenKind::LBrace),     // left brace
                self.parse_module_items(),               // body
                self.match_token(TokenKind::RBrace),     // right brace
            ],
            self.db,
        )
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: struct<Identifier><ParamListBraced>
    /// Returns a GreenId of a node with kind ItemStruct.
    fn match_struct(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ItemStruct,
            vec![
                self.take(),                             // struct keyword
                self.match_token(TokenKind::Identifier), // name
                // TODO: support generics
                self.match_braced_param_list(), // body
            ],
            self.db,
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: fn<Identifier><ParenthesizedParamList><ReturnTypeClause>
    /// Returns a GreenId of a node with kind FunctionSignature.
    fn match_function_signature(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::FunctionSignature,
            vec![
                self.take(),                             // function keyword
                self.match_token(TokenKind::Identifier), // name
                // TODO: support generics
                self.match_parenthesized_param_list(), // (params)
                self.parse_return_type_clause(),       // return type clause
            ],
            self.db,
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: <FunctionSignature><Block>
    /// Returns a GreenId of a node with kind ItemFunction.
    fn match_function(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ItemFunction,
            vec![
                self.match_function_signature(), // signature
                self.parse_block(),              // function body
            ],
            self.db,
        )
    }

    /// Returns a GreenId of a node with kind ItemList.
    fn parse_module_items(&mut self) -> GreenId {
        self.parse_list(Self::try_parse_module_item, TokenKind::RBrace, SyntaxKind::ItemList)
    }

    /// Returns a GreenId of a node with kind ItemStruct|ItemFunction or None if a module item
    /// can't be parsed.
    fn try_parse_module_item(&mut self) -> Option<GreenId> {
        let map: HashMap<TokenKind, FunctionPointer<'a>> = HashMap::from([
            (TokenKind::Struct, Self::match_struct as FunctionPointer<'a>),
            (TokenKind::Function, Self::match_function as FunctionPointer<'a>),
        ]);
        self.try_match_item(map, TokenKind::RBrace)
    }

    ///////////////////////////////// Expression parsing /////////////////////////////////

    /// Returns a GreenId of a node with an Expr.* kind (see syntax::node::ast::Expr) or None if an
    /// expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::LBrace => Some(self.parse_block()),
            _ => self.try_parse_simple_expression(MAX_PRECEDENCE),
        }
    }
    /// Returns a GreenId of a node with an Expr.* kind (see syntax::node::ast::Expr) or a node with
    /// kind ExprMissing if an expression can't be parsed.
    fn parse_expr(&mut self) -> GreenId {
        match self.try_parse_expr() {
            Some(green) => green,
            None => GreenId::new_node(SyntaxKind::ExprMissing, Vec::new(), self.db),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see syntax::node::ast::Expr), excluding
    /// ExprBlock, or None if such an expression can't be parsed.
    fn try_parse_simple_expression(&mut self, parent_precedence: usize) -> Option<GreenId> {
        let mut expr = if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            GreenId::new_node(
                SyntaxKind::ExprUnary,
                vec![
                    self.take(),                              // unary operator
                    self.parse_simple_expression(precedence), // expression
                ],
                self.db,
            )
        } else {
            self.try_parse_atom()?
        };

        while let Some(precedence) = get_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            expr = GreenId::new_node(
                SyntaxKind::ExprBinary,
                vec![
                    expr,                                     // LHS expression
                    self.take(),                              // operator
                    self.parse_simple_expression(precedence), // RHS expression
                ],
                self.db,
            );
        }
        Some(expr)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see syntax::node::ast::Expr), excluding
    /// ExprBlock, or ExprMissing if such an expression can't be parsed.
    fn parse_simple_expression(&mut self, parent_precedence: usize) -> GreenId {
        match self.try_parse_simple_expression(parent_precedence) {
            Some(green) => green,
            None => GreenId::new_node(SyntaxKind::ExprMissing, Vec::new(), self.db),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprConstructorCall|ExprParenthesized|ExprTuple kind, or None if
    /// such an expression can't be parsed.
    fn try_parse_atom(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Identifier => {
                let path = self.parse_path();
                match self.peek().kind {
                    TokenKind::LParen => Some(self.match_function_call(path)),
                    TokenKind::LBrace => Some(self.match_constructor_call(path)),
                    _ => Some(path),
                }
            }
            TokenKind::False | TokenKind::True | TokenKind::LiteralNumber => Some(self.take()),
            TokenKind::LParen => Some(self.match_parenthesized_expr()),
            _ => {
                // TODO: report to diagnostics.
                None
            }
        }
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: \(<ExprList>\)
    /// Returns a GreenId of a node with kind ExprListParenthesized.
    fn match_expression_list_parenthesized(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ExprListParenthesized,
            vec![
                // left parenthesis
                self.match_token(TokenKind::LParen),
                // Expression list
                self.parse_separated_list(
                    Self::try_parse_expr,
                    TokenKind::Comma,
                    TokenKind::RParen,
                    SyntaxKind::ExprList,
                ),
                // right parenthesis
                self.match_token(TokenKind::RParen),
            ],
            self.db,
        )
    }

    /// Assumes the current token is LBrace
    /// Expected pattern: \{<CtorArgList>\}
    /// Returns a GreenId of a node with kind ArgListBraced
    fn match_argument_list_braced(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ArgListBraced,
            vec![
                // left brace
                self.match_token(TokenKind::LBrace),
                // Argument list
                self.parse_separated_list(
                    Self::try_parse_constructor_argument,
                    TokenKind::Comma,
                    TokenKind::RBrace,
                    SyntaxKind::CtorArgList,
                ),
                // right brace
                self.match_token(TokenKind::RBrace),
            ],
            self.db,
        )
    }

    /// Assumes the current token is LParen
    /// Expected pattern: <ExprListParenthesized>
    /// Returns a GreenId of a node with kind ExprFunctionCall
    fn match_function_call(&mut self, path: GreenId) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ExprFunctionCall,
            vec![
                path,                                       // function name
                self.match_expression_list_parenthesized(), // (arguments)
            ],
            self.db,
        )
    }

    /// Assumes the current token is LBrace
    /// Expected pattern: <ExprListBraced>
    /// Returns a GreenId of a node with kind ExprConstructorCall
    fn match_constructor_call(&mut self, path: GreenId) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ExprConstructorCall,
            vec![path, self.match_argument_list_braced()],
            self.db,
        )
    }

    /// Assumes the current token is LParen
    /// Expected pattern: \((<expr>,)*<expr>?\)
    /// Returns a GreenId of a node with kind ExprParenthesized|ExprTuple
    fn match_parenthesized_expr(&mut self) -> GreenId {
        let mut children: Vec<GreenId> = vec![
            self.take(), // left parenthesis
        ];

        let expr_list_green = self.parse_separated_list(
            Self::try_parse_expr,
            TokenKind::Comma,
            TokenKind::RParen,
            SyntaxKind::ExprList,
        );

        let kind = match self.db.lookup_intern_green(expr_list_green) {
            // TODO(yuval): add support for the unit type ().
            GreenNode::Internal(list_internal) if list_internal.children().len() == 1 => {
                // We have exactly one item and no separator --> This is not a tuple.
                children.push(list_internal.child_at(0));
                SyntaxKind::ExprParenthesized
            }
            GreenNode::Internal(_list_internal) => {
                children.push(expr_list_green);
                SyntaxKind::ExprTuple
            }
            _ => panic!("This should never happen"),
        };

        children.push(self.match_token(TokenKind::RParen)); // right parenthesis
        GreenId::new_node(kind, children, self.db)
    }

    /// Assumes the current token is DotDot
    /// Expected pattern: \.\.<Expr>
    /// Returns a GreenId of a node with kind ExprStructUpdateTail
    fn match_struct_update_tail(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::StructUpdateTail,
            vec![
                self.take(), // ..
                // TODO(yuval): consider changing this to SimpleExpr once it exists.
                self.parse_expr(), // expression
            ],
            self.db,
        )
    }

    /// Like parse_argument, but also allows for the "struct update syntax" (see https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax).
    /// e.g. 'let s2 = S{"s2", ..s1};'
    /// Returns a GreenId of a node with kind Argument|StructUpdateTail.
    fn try_parse_constructor_argument(&mut self) -> Option<GreenId> {
        if self.peek().kind == TokenKind::DotDot {
            Some(self.match_struct_update_tail())
        } else {
            self.try_parse_argument()
        }
    }

    /// Returns a GreenId of a node with kind ArgExpr or OptionArgExprEmpty if an argument
    /// expression (":<value>") can't be parsed.
    fn parse_arg_expression(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Colon {
            GreenId::new_node(
                SyntaxKind::ArgExpr,
                vec![
                    self.match_token(TokenKind::Colon), // ':'
                    self.parse_expr(),                  // value
                ],
                self.db,
            )
        } else {
            GreenId::new_node(SyntaxKind::OptionArgExprEmpty, Vec::new(), self.db)
        }
    }

    /// Returns a GreenId of a node with kind Arg or None if an argument can't be parsed.
    fn try_parse_argument(&mut self) -> Option<GreenId> {
        if self.peek().kind != TokenKind::Identifier {
            return None;
        }
        Some(GreenId::new_node(
            SyntaxKind::Arg,
            vec![
                self.match_token(TokenKind::Identifier), // identifier
                self.parse_arg_expression(),             // :<expr>
            ],
            self.db,
        ))
    }

    /// Assumes the current token is LBrace
    /// Expected pattern: \{<Statement>*\}
    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ExprBlock,
            vec![
                // left brace
                self.match_token(TokenKind::LBrace),
                // statements
                self.parse_list(
                    Self::try_parse_statement,
                    TokenKind::RBrace,
                    SyntaxKind::StatementList,
                ),
                // right brace
                self.match_token(TokenKind::RBrace),
            ],
            self.db,
        )
    }

    // Statements.

    /// Returns a GreenId of a node with an Statement.* kind (see syntax::node::ast::Statement) or
    /// StatementMissing if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> Option<GreenId> {
        match self.peek().kind {
            TokenKind::Let => {
                Some(GreenId::new_node(
                    SyntaxKind::StatementLet,
                    vec![
                        self.take(), // let keyword
                        // TODO: support patterns instead of only an identifier.
                        self.match_token(TokenKind::Identifier), // identifier
                        self.parse_type_clause(),                // type clause
                        self.match_token(TokenKind::Eq),         // '='
                        self.parse_expr(),                       // expression
                        self.match_token(TokenKind::Semi),       // ';'
                    ],
                    self.db,
                ))
            }
            TokenKind::Return => {
                Some(GreenId::new_node(
                    SyntaxKind::StatementReturn,
                    vec![
                        self.take(),                       // return keyword
                        self.parse_expr(),                 // expression
                        self.match_token(TokenKind::Semi), // ';'
                    ],
                    self.db,
                ))
            }
            _ => match self.try_parse_expr() {
                None => None,
                Some(expr) => {
                    let mut children = vec![expr];
                    if self.peek().kind == TokenKind::Semi {
                        children.push(self.match_token(TokenKind::Semi));
                    } else {
                        children.push(GreenId::new_node(
                            SyntaxKind::OptionSemicolonEmpty,
                            Vec::new(),
                            self.db,
                        ));
                    }
                    Some(GreenId::new_node(SyntaxKind::StatementExpr, children, self.db))
                }
            },
        }
    }
    fn parse_statement(&mut self) -> GreenId {
        match self.try_parse_statement() {
            Some(green) => green,
            None => GreenId::new_node(SyntaxKind::StatementMissing, Vec::new(), self.db),
        }
    }

    /// Returns a GreenId of a node with kind TypeClause or OptionTypeClauseEmpty if a type clause
    /// can't be parsed.
    fn parse_type_clause(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Colon {
            GreenId::new_node(
                SyntaxKind::TypeClause,
                vec![
                    self.match_token(TokenKind::Colon), // ':'
                    // TODO: support reacher types.
                    self.parse_path(), // type
                ],
                self.db,
            )
        } else {
            GreenId::new_node(SyntaxKind::OptionTypeClauseEmpty, Vec::new(), self.db)
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_return_type_clause(&mut self) -> GreenId {
        if self.peek().kind == TokenKind::Arrow {
            GreenId::new_node(
                SyntaxKind::ReturnTypeClause,
                vec![
                    self.match_token(TokenKind::Arrow), // '->'
                    // TODO: support reacher types.
                    self.parse_path(), // return type
                ],
                self.db,
            )
        } else {
            GreenId::new_node(SyntaxKind::OptionReturnTypeClauseEmpty, Vec::new(), self.db)
        }
    }

    /// Returns a GreenId of a node with kind ParamListParenthesized.
    fn match_parenthesized_param_list(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ParamListParenthesized,
            vec![
                self.match_token(TokenKind::LParen),      // left
                self.parse_param_list(TokenKind::RParen), // param list
                self.match_token(TokenKind::RParen),      // right
            ],
            self.db,
        )
    }

    /// Returns a GreenId of a node with kind ParamListBraced.
    fn match_braced_param_list(&mut self) -> GreenId {
        GreenId::new_node(
            SyntaxKind::ParamListBraced,
            vec![
                self.match_token(TokenKind::LBrace),      // left
                self.parse_param_list(TokenKind::RBrace), // param list
                self.match_token(TokenKind::RBrace),      // right
            ],
            self.db,
        )
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self, closing_token: TokenKind) -> GreenId {
        self.parse_separated_list(
            Self::try_parse_param,
            TokenKind::Comma,
            closing_token,
            SyntaxKind::ParamList,
        )
    }

    /// Returns a GreenId of a node with kind Param or None if a parameter can't be parsed.
    fn try_parse_param(&mut self) -> Option<GreenId> {
        Some(GreenId::new_node(
            SyntaxKind::Param,
            vec![
                self.try_parse_identifier()?, // identifier
                self.parse_type_clause(),     // type_clause
            ],
            self.db,
        ))
    }

    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> GreenId {
        self.parse_strict_separated_list(
            Self::try_parse_path_segment,
            TokenKind::ColonColon,
            SyntaxKind::ExprPath,
        )
    }

    /// Returns a GreenId of a node with kind Identifier, or None if the current token is not an
    /// identifier.
    fn try_parse_identifier(&mut self) -> Option<GreenId> {
        if self.peek().kind == TokenKind::Identifier {
            Some(self.match_token(TokenKind::Identifier))
        } else {
            None
        }
    }

    /// Returns a GreenId of a node with kind PathSegment, or None if a path segment can't be
    /// parsed.
    fn try_parse_path_segment(&mut self) -> Option<GreenId> {
        Some(GreenId::new_node(
            SyntaxKind::PathSegment,
            vec![
                // identifier
                self.try_parse_identifier()?,
                // TODO(yuval): support generics.
                // generic args
                GreenId::new_node(SyntaxKind::OptionGenericArgsNone, vec![], self.db),
            ],
            self.db,
        ))
    }

    // Helpers.

    /// Parses a list of items (without separators), where the items are parsed using
    /// `try_parse_list_item`. The `closing` token indicates when to stop parsing. Returns a
    /// GreenId of a node with kind `list_kind`. If it can't parse an item and the current token
    /// is not `closing`, skips the current token and tries again.
    fn parse_list(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<GreenId>,
        closing: TokenKind,
        list_kind: SyntaxKind,
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
        GreenId::new_node(list_kind, children, self.db)
    }

    /// Parses a list of items with `separator`s, where the items are parsed using
    /// `try_parse_list_item`. The separator may or may not appear in the end of the list.
    /// The `closing` token indicates when to stop parsing.
    /// Returns a GreenId of a node with kind `list_kind`. This list contains alternating children:
    /// [item, separator, item, separator, ...]. Both items and separators may be missing.
    /// The length of the list is either 2 * #items - 1 or 2 * #items (a separator for each
    /// item or for each item but the last one).
    fn parse_separated_list(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<GreenId>,
        separator: TokenKind,
        closing: TokenKind,
        list_kind: SyntaxKind,
    ) -> GreenId {
        let mut children: Vec<GreenId> = Vec::new();
        while self.peek().kind != closing && self.peek().kind != TokenKind::EndOfFile {
            let item = try_parse_list_item(self);
            // None means parse_list_item could not parse the next tokens as the expected item.
            match item {
                None => {
                    self.skip_token();
                }
                Some(green) => {
                    let separator = if self.peek().kind == separator {
                        self.take()
                    } else {
                        Token::missing_green(self.db)
                    };

                    children.push(green);
                    children.push(separator);
                }
            }
        }
        if let Some(last) = children.last() {
            if last.is_token_missing(self.db) {
                children.pop();
            }
        }
        GreenId::new_node(list_kind, children, self.db)
    }

    /// Expects a separated list with at least one item, that both starts and ends with such an
    /// item. i.e. <item>(<separator><item>)* (no separator in the end)
    /// Returns a GreenId of a node with kind `list_kind`. The length of the list is
    /// 2 * #items - 1 (a separator for each item but the last one).
    fn parse_strict_separated_list(
        &mut self,
        try_parse_list_item: fn(&mut Self) -> Option<GreenId>,
        separator: TokenKind,
        list_kind: SyntaxKind,
    ) -> GreenId {
        let mut children: Vec<GreenId> = Vec::new();
        loop {
            let item = try_parse_list_item(self);
            if self.peek().kind == separator {
                children.push(item.map_or(Token::missing_green(self.db), |x| x));
                children.push(self.match_token(separator));
            } else {
                match item {
                    // No separator is missing. If item is missing too, skip the current token and
                    // try again.
                    None => {
                        self.skip_token();
                        continue;
                    }
                    Some(green) => {
                        children.push(green);
                        children.push(Token::missing_green(self.db));
                        break;
                    }
                }
            };
        }
        children.pop();
        GreenId::new_node(list_kind, children, self.db)
    }

    /// Peeks at the current token from the Lexer without taking it.
    fn peek(&self) -> &TerminalWithKind {
        &self.current
    }

    /// Takes a terminal from the Lexer and place it in self.current.
    fn take_raw(&mut self) -> GreenId {
        let next_token_with_kind = self.lexer.next().unwrap();
        std::mem::replace(&mut self.current, next_token_with_kind).terminal
    }

    /// Skips a token. A skipped token is a token which is not expected where it is found. Skipping
    /// this token means reporting an error and ignoring it and continuing the compilation as if it
    /// wasn't there.
    fn skip_token(&mut self) {
        let token = self.take_raw();
        if let GreenNode::Internal(internal) = self.db.lookup_intern_green(token) {
            if internal.kind() == SyntaxKind::Terminal {
                // TODO(yuval): report to diagnostics:
            }
        }
        self.skipped_tokens.push(token);
    }

    /// Takes a token from the Lexer and place it in self.current. If tokens were skipped, glue them
    /// to this token as leading trivia.
    fn take(&mut self) -> GreenId {
        let token = self.take_raw();
        self.add_skipped_to_terminal(token)
    }

    /// Changes the kind of a Terminal to "SkippedTerminal".
    fn modify_terminal_to_skipped(&self, terminal: GreenId) -> GreenId {
        let mut terminal_internal =
            if let GreenNode::Internal(terminal_internal) = self.db.lookup_intern_green(terminal) {
                terminal_internal
            } else {
                panic!();
            };
        assert_eq!(terminal_internal.kind(), SyntaxKind::Terminal);
        terminal_internal.set_kind(SyntaxKind::SkippedTerminal);
        self.db.intern_green(GreenNode::Internal(terminal_internal))
    }

    /// Glues the recently skipped tokens to the given terminal as leading trivia.
    fn add_skipped_to_terminal(&mut self, terminal: GreenId) -> GreenId {
        if self.skipped_tokens.is_empty() {
            return terminal;
        }

        let mut skipped_terminals: Vec<GreenId> = mem::take(&mut self.skipped_tokens)
            .into_iter()
            .map(|t| self.modify_terminal_to_skipped(t))
            .collect();

        let terminal_internal =
            if let GreenNode::Internal(terminal_internal) = self.db.lookup_intern_green(terminal) {
                terminal_internal
            } else {
                panic!();
            };
        assert_eq!(terminal_internal.kind(), SyntaxKind::Terminal);

        let leading_trivia = terminal_internal.child_at(0);
        let leading_trivia_internal = if let GreenNode::Internal(leading_trivia_internal) =
            self.db.lookup_intern_green(leading_trivia)
        {
            leading_trivia_internal
        } else {
            panic!();
        };
        assert_eq!(leading_trivia_internal.kind(), SyntaxKind::Trivia);

        let mut modified_leading_trivia_internal = leading_trivia_internal;
        modified_leading_trivia_internal.append_children(&mut skipped_terminals);
        let new_leading_trivia =
            self.db.intern_green(GreenNode::Internal(modified_leading_trivia_internal));

        let mut modified_terminal_internal = terminal_internal;
        modified_terminal_internal.modify_child(0, new_leading_trivia);
        self.db.intern_green(GreenNode::Internal(modified_terminal_internal))
    }

    fn try_parse_token(&mut self, token_kind: TokenKind) -> Option<GreenId> {
        if self.peek().kind == token_kind {
            Some(self.take())
        } else {
            // TODO: report to diagnostics
            None
        }
    }
    fn parse_token(&mut self, token_kind: TokenKind) -> GreenId {
        if let Some(green) = self.try_parse_token(token_kind) {
            green
        } else {
            Token::missing_green(self.db)
        }
    }
    /// Assumes the current token is of the given `token_kind`.
    /// Returns a GreenId of a node with kind `token_kind` or panics if the current token is not of
    /// this kind.
    fn match_token(&mut self, token_kind: TokenKind) -> GreenId {
        if let Some(green) = self.try_parse_token(token_kind) {
            green
        } else {
            // TODO: we want more advanced logic here - decide if token is missing or skipped
            // according to context. Currently we immediately panic.
            panic!("Expected token {:?} is missing!", token_kind);
        }
    }
}

fn get_unary_operator_precedence(kind: TokenKind) -> Option<usize> {
    if [TokenKind::Not, TokenKind::Minus].contains(&kind) {
        get_operator_precedence(kind)
    } else {
        None
    }
}
fn get_operator_precedence(kind: TokenKind) -> Option<usize> {
    match kind {
        TokenKind::Dot => Some(0),
        // TODO(yuval): support unary-only/non-binary operators. "not" can't be binary.
        TokenKind::Not => Some(1),
        TokenKind::Mul | TokenKind::Div => Some(2),
        TokenKind::Plus | TokenKind::Minus => Some(3),
        TokenKind::EqEq => Some(4),
        // TODO(yuval): add more operators.
        _ => None,
    }
}
