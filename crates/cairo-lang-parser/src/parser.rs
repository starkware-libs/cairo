use std::mem;

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::*;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use syntax::node::green::{GreenNode, GreenNodeDetails};

use crate::diagnostic::{ParserDiagnosticKind, ParserDiagnosticKindMissing};
use crate::lexer::{Lexer, LexerTerminal};
use crate::operators::{get_post_operator_precedence, get_unary_operator_precedence};
use crate::recovery::is_of_kind;
use crate::ParserDiagnostic;

#[cfg(test)]
#[path = "parser_test.rs"]
mod test;

pub struct Parser<'a> {
    db: &'a dyn SyntaxGroup,
    file_id: FileId,
    lexer: Lexer<'a>,
    /// The next terminal to handle.
    next_terminal: LexerTerminal,
    /// A vector of pending trivia to be added as leading trivia to the next valid terminal.
    pending_trivia: Vec<TriviumGreen>,
    /// The current offset, excluding the current terminal.
    offset: TextOffset,
    /// The width of the current terminal being handled.
    current_width: TextWidth,
    /// The length of the trailing trivia following the last read token.
    last_trivia_length: TextWidth,
    diagnostics: &'a mut DiagnosticsBuilder<ParserDiagnostic>,
    /// The stack of parsing contexts. Used for diagnostics of missing tokens.
    parsing_context: Vec<String>,
}

/// The possible results of a try_parse_* function failing to parse.
#[derive(PartialEq)]
pub enum TryParseFailure {
    /// The parsing failed, and no token was consumed, the current token is the token which caused
    /// the failure and thus should be skipped by the caller.
    SkipToken,
    /// The parsing failed, some tokens were consumed, and the current token is yet to be
    /// processed. Should be used when the failure cannot be
    /// determined by the first token alone, and thus the tokens until the token which
    /// determines the failure should be consumed.
    DoNothing,
}
/// The result of a try_parse_* functions.
pub type TryParseResult<GreenElement> = Result<GreenElement, TryParseFailure>;

// try_parse_<something>: returns a TryParseElementResult. A Result::Ok with green ID with a kind
// that represents 'something' or a Result::Err if 'something' can't be parsed.
// If the error kind is Failure, the current token is not consumed, otherwise (Success or
// error of kind FailureAndSkipped) it is (taken or skipped). Used when something may or may not be
// there and we can act differently according to each case.
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

const MAX_PRECEDENCE: usize = 1000;
const TOP_LEVEL_ITEM_DESCRIPTION: &str =
    "Const/Module/Use/FreeFunction/ExternFunction/ExternType/Trait/Impl/Struct/Enum/TypeAlias";
const TRAIT_ITEM_DESCRIPTION: &str = "trait item";
const IMPL_ITEM_DESCRIPTION: &str = "impl item";

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
            offset: Default::default(),
            current_width: Default::default(),
            last_trivia_length: Default::default(),
            diagnostics,
            parsing_context: vec!["a module".to_string()],
        };
        let green = parser.parse_syntax_file();
        SyntaxFile::from_syntax_node(db, SyntaxNode::new_root(db, file_id, green.0))
    }

    /// Parses a file expr.
    pub fn parse_file_expr(
        db: &'a dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
        file_id: FileId,
        text: &'a str,
    ) -> Expr {
        let mut lexer = Lexer::from_text(db, file_id, text);
        let next_terminal = lexer.next().unwrap();
        let mut parser = Parser {
            db,
            file_id,
            lexer,
            next_terminal,
            pending_trivia: Vec::new(),
            offset: Default::default(),
            current_width: Default::default(),
            last_trivia_length: Default::default(),
            diagnostics,
            parsing_context: vec!["an inline macro generated expression".to_string()],
        };
        let green = parser.parse_expr();
        if let Err(SkippedError(span)) = parser.skip_until(is_of_kind!()) {
            parser.diagnostics.add(ParserDiagnostic {
                file_id: parser.file_id,
                kind: ParserDiagnosticKind::SkippedElement { element_name: "end of expr".into() },
                span,
            });
        }
        Expr::from_syntax_node(db, SyntaxNode::new_root(db, file_id, green.0))
    }

    /// Returns a GreenId of an ExprMissing and adds a diagnostic describing it.
    fn create_and_report_missing<T: TypedSyntaxNode>(
        &mut self,
        missing_kind: ParserDiagnosticKindMissing,
    ) -> T::Green {
        let next_offset = self.offset.add_width(self.current_width - self.last_trivia_length);
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: ParserDiagnosticKind::Missing {
                kind: missing_kind,
                parsing_context: self.parsing_context.last().unwrap().into(),
            },
            span: TextSpan { start: next_offset, end: next_offset },
        });
        T::missing(self.db)
    }

    /// Returns the missing terminal and adds the corresponding missing token
    /// diagnostic report.
    fn create_and_report_missing_terminal<Terminal: syntax::node::Terminal>(
        &mut self,
    ) -> Terminal::Green {
        self.create_and_report_missing::<Terminal>(ParserDiagnosticKindMissing::MissingToken(
            Terminal::KIND,
        ))
    }

    pub fn parse_syntax_file(mut self) -> SyntaxFileGreen {
        let items: ItemListGreen = ItemList::new_green(
            self.db,
            self.parse_attributed_list(
                Self::try_parse_top_level_item,
                is_of_kind!(),
                TOP_LEVEL_ITEM_DESCRIPTION,
            ),
        );
        // This will not panic since the above parsing only stops when reaches EOF.
        assert_eq!(self.peek().kind, SyntaxKind::TerminalEndOfFile);

        // Fix offset in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.offset = self.offset.add_width(self.current_width);

        let eof = self.add_trivia_to_terminal::<TerminalEndOfFile>(self.next_terminal.clone());
        SyntaxFile::new_green(self.db, items, eof)
    }

    // ------------------------------- Top level items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]), or
    /// TryParseFauilre if a top-level item can't be parsed.
    pub fn try_parse_top_level_item(&mut self) -> TryParseResult<ItemGreen> {
        let maybe_attributes = self.try_parse_attribute_list(TOP_LEVEL_ITEM_DESCRIPTION);

        let (has_attrs, attributes) = match maybe_attributes {
            Ok(attributes) => (true, attributes),
            Err(_) => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalConst => Ok(self.expect_const(attributes).into()),
            SyntaxKind::TerminalModule => Ok(self.expect_module(attributes).into()),
            SyntaxKind::TerminalStruct => Ok(self.expect_struct(attributes).into()),
            SyntaxKind::TerminalEnum => Ok(self.expect_enum(attributes).into()),
            SyntaxKind::TerminalType => Ok(self.expect_type_alias(attributes).into()),
            SyntaxKind::TerminalExtern => Ok(self.expect_extern_item(attributes)),
            SyntaxKind::TerminalFunction => {
                Ok(self.expect_function_with_body(attributes, "a free function").into())
            }
            SyntaxKind::TerminalUse => Ok(self.expect_use(attributes).into()),
            SyntaxKind::TerminalTrait => Ok(self.expect_trait(attributes).into()),
            SyntaxKind::TerminalImpl => Ok(self.expect_item_impl(attributes)),
            _ => {
                if has_attrs {
                    Ok(self.create_and_report_missing::<Item>(
                        ParserDiagnosticKindMissing::AttributesWithoutItem,
                    ))
                } else {
                    Err(TryParseFailure::SkipToken)
                }
            }
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: `mod <Identifier> \{<ItemList>\}` or `mod <Identifier>;`.
    fn expect_module(&mut self, attributes: AttributeListGreen) -> ItemModuleGreen {
        self.subcontext("an inline module");
        let module_kw = self.take::<TerminalModule>();
        let name = self.parse_identifier();

        let body = match self.peek().kind {
            SyntaxKind::TerminalLBrace => {
                self.subcontext("a module body");
                let lbrace = self.take::<TerminalLBrace>();
                let items = ItemList::new_green(
                    self.db,
                    self.parse_attributed_list(
                        Self::try_parse_top_level_item,
                        is_of_kind!(rbrace),
                        TOP_LEVEL_ITEM_DESCRIPTION,
                    ),
                );
                let rbrace = self.parse_token::<TerminalRBrace>();
                self.pop_context();
                ModuleBody::new_green(self.db, lbrace, items, rbrace).into()
            }
            // TODO: Improve diagnostic to indicate semicolon or a body were expected.
            _ => self.parse_token::<TerminalSemicolon>().into(),
        };

        self.pop_context();
        ItemModule::new_green(self.db, attributes, module_kw, name, body)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: `struct<Identifier>{<ParamList>}`
    fn expect_struct(&mut self, attributes: AttributeListGreen) -> ItemStructGreen {
        self.subcontext("a struct");
        let struct_kw = self.take::<TerminalStruct>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let members = self.parse_member_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        self.pop_context();
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
        self.subcontext("an enum");
        let enum_kw = self.take::<TerminalEnum>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let variants = self.parse_variant_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        self.pop_context();
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

    /// Assumes the current token is type.
    /// Expected pattern: `type <Identifier>{<ParamList>} = <TypeExpression>`
    fn expect_type_alias(&mut self, attributes: AttributeListGreen) -> ItemTypeAliasGreen {
        self.subcontext("a type alias");
        let type_kw = self.take::<TerminalType>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let eq = self.parse_token::<TerminalEq>();
        let ty = self.parse_type_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        self.pop_context();
        ItemTypeAlias::new_green(
            self.db,
            attributes,
            type_kw,
            name,
            generic_params,
            eq,
            ty,
            semicolon,
        )
    }

    /// Expected pattern: `<ParenthesizedParamList><ReturnTypeClause>`
    fn expect_function_signature(&mut self) -> FunctionSignatureGreen {
        self.subcontext("a function signature");
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

        self.pop_context();
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

    /// Assumes the current token is [TerminalConst].
    /// Expected pattern: `const <Identifier> = <Expr>;`
    fn expect_const(&mut self, attributes: AttributeListGreen) -> ItemConstantGreen {
        self.subcontext("a const item");
        let const_kw = self.take::<TerminalConst>();
        let name = self.parse_identifier();
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(eq, semicolon, top_level),
        });
        let eq = self.parse_token::<TerminalEq>();
        let expr = self.parse_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();

        self.pop_context();
        ItemConstant::new_green(
            self.db,
            attributes,
            const_kw,
            name,
            type_clause,
            eq,
            expr,
            semicolon,
        )
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionDeclaration>|type<Identifier>);`
    fn expect_extern_item(&mut self, attributes: AttributeListGreen) -> ItemGreen {
        let extern_item = self.in_subcontext("an extern item (function/type)", |slf| {
            slf.expect_extern_item_inner(attributes)
        });
        match extern_item {
            ExternItem::Function(x) => x.into(),
            ExternItem::Type(x) => x.into(),
        }
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionDeclaration>|type<Identifier>);`
    fn expect_extern_impl_item(&mut self, attributes: AttributeListGreen) -> ImplItemGreen {
        let extern_item = self.in_subcontext("an extern impl item (function/type)", |slf| {
            slf.expect_extern_item_inner(attributes)
        });
        match extern_item {
            ExternItem::Function(x) => x.into(),
            ExternItem::Type(x) => x.into(),
        }
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionDeclaration>|type<Identifier>);`
    fn expect_extern_item_inner(&mut self, attributes: AttributeListGreen) -> ExternItem {
        let extern_kw = self.take::<TerminalExtern>();
        match self.peek().kind {
            SyntaxKind::TerminalFunction => {
                let declaration = self.expect_function_declaration();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                ExternItem::Function(ItemExternFunction::new_green(
                    self.db,
                    attributes,
                    extern_kw,
                    declaration,
                    semicolon,
                ))
            }
            _ => {
                // TODO(spapini): Do'nt return ItemExternType if we don't see a type.
                self.subcontext("an extern type");
                let type_kw = self.parse_token::<TerminalType>();

                let name = self.parse_identifier();
                let generic_params = self.parse_optional_generic_params();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                self.pop_context();
                // If the next token is not type, assume it is missing.
                ExternItem::Type(ItemExternType::new_green(
                    self.db,
                    attributes,
                    extern_kw,
                    type_kw,
                    name,
                    generic_params,
                    semicolon,
                ))
            }
        }
    }

    /// Assumes the current token is Use.
    /// Expected pattern: `use<Path>;`
    fn expect_use(&mut self, attributes: AttributeListGreen) -> ItemUseGreen {
        self.subcontext("a use item");
        let use_kw = self.take::<TerminalUse>();
        let use_path = self.parse_use_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        self.pop_context();
        ItemUse::new_green(self.db, attributes, use_kw, use_path, semicolon)
    }

    /// Returns a GreenId of a node with a UsePath kind or TryParseFailure if can't parse a UsePath.
    fn try_parse_use_path(&mut self) -> TryParseResult<UsePathGreen> {
        if !matches!(self.peek().kind, SyntaxKind::TerminalLBrace | SyntaxKind::TerminalIdentifier)
        {
            return Err(TryParseFailure::SkipToken);
        }
        Ok(self.parse_use_path())
    }

    /// Returns a GreenId of a node with a UsePath kind.
    fn parse_use_path(&mut self) -> UsePathGreen {
        if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.subcontext("a group use path");
            let lbrace = self.parse_token::<TerminalLBrace>();
            let items = UsePathList::new_green(self.db,
                    self.parse_separated_list::<
                        UsePath, TerminalComma, UsePathListElementOrSeparatorGreen
                    >(
                        Self::try_parse_use_path,
                        is_of_kind!(rbrace, top_level),
                        "path segment",
                    ));
            let rbrace = self.parse_token::<TerminalRBrace>();
            self.pop_context();
            UsePathMulti::new_green(self.db, lbrace, items, rbrace).into()
        } else if let Ok(ident) = self.try_parse_identifier() {
            self.subcontext("a use path");
            let ident = PathSegmentSimple::new_green(self.db, ident).into();
            let use_path = match self.peek().kind {
                SyntaxKind::TerminalColonColon => {
                    let colon_colon = self.parse_token::<TerminalColonColon>();
                    let use_path = self.parse_use_path();
                    UsePathSingle::new_green(self.db, ident, colon_colon, use_path).into()
                }
                SyntaxKind::TerminalAs => {
                    let as_kw = self.take::<TerminalAs>();
                    let alias = self.parse_identifier();
                    let alias_clause = AliasClause::new_green(self.db, as_kw, alias).into();
                    UsePathLeaf::new_green(self.db, ident, alias_clause).into()
                }
                _ => {
                    let alias_clause = OptionAliasClauseEmpty::new_green(self.db).into();
                    UsePathLeaf::new_green(self.db, ident, alias_clause).into()
                }
            };
            self.pop_context();
            use_path
        } else {
            let missing = self.create_and_report_missing::<TerminalIdentifier>(
                ParserDiagnosticKindMissing::MissingPathSegment,
            );
            let ident = PathSegmentSimple::new_green(self.db, missing).into();
            UsePathLeaf::new_green(
                self.db,
                ident,
                OptionAliasClauseEmpty::new_green(self.db).into(),
            )
            .into()
        }
    }

    /// Returns a GreenId of a node with an identifier kind or TryParseFailure if an identifier
    /// can't be parsed.
    /// Note that if the terminal is a keyword or an underscore, it is skipped, and
    /// Some(missing-identifier) is returned.
    fn try_parse_identifier(&mut self) -> TryParseResult<TerminalIdentifierGreen> {
        if self.peek().kind.is_keyword_terminal() {
            // TODO(spapini): don't skip every keyword. Instead, pass a recovery set.
            Ok(self.skip_token_and_return_missing::<TerminalIdentifier>(
                ParserDiagnosticKind::ReservedIdentifier { identifier: self.peek().text.clone() },
            ))
        } else if self.peek().kind == SyntaxKind::TerminalUnderscore {
            Ok(self.skip_token_and_return_missing::<TerminalIdentifier>(
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
            Ok(identifier) => identifier,
            Err(_) => self.create_and_report_missing_terminal::<TerminalIdentifier>(),
        }
    }

    /// Returns a GreenId of a node with an attribute list kind or TryParseFailure if an attribute
    /// list can't be parsed.
    /// `expected_elements_str` are the expected elements that these attributes are parsed for.
    /// Note: it should not include "attribute".
    fn try_parse_attribute_list(
        &mut self,
        expected_elements_str: &str,
    ) -> TryParseResult<AttributeListGreen> {
        if self.peek().kind == SyntaxKind::TerminalHash {
            Ok(self.parse_attribute_list(expected_elements_str))
        } else {
            Err(TryParseFailure::SkipToken)
        }
    }

    /// Parses an attribute list.
    /// `expected_elements_str` are the expected elements that these attributes are parsed for.
    /// Note: it should not include "attribute".
    fn parse_attribute_list(&mut self, expected_elements_str: &str) -> AttributeListGreen {
        self.subcontext("attributes");
        let attr_list = AttributeList::new_green(
            self.db,
            self.parse_list(
                Self::try_parse_attribute,
                |x| x != SyntaxKind::TerminalHash,
                format!("{expected_elements_str} or an attribute").as_str(),
            ),
        );
        self.pop_context();
        attr_list
    }

    /// Returns a GreenId of a node with an attribute kind or TryParseFailure if an attribute can't
    /// be parsed.
    fn try_parse_attribute(&mut self) -> TryParseResult<AttributeGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalHash => {
                self.subcontext("an attribute");
                let hash = self.take::<TerminalHash>();
                let lbrack = self.parse_token::<TerminalLBrack>();
                let attr = self.parse_path();
                let arguments = self.try_parse_parenthesized_argument_list();
                let rbrack = self.parse_token::<TerminalRBrack>();
                self.pop_context();

                Ok(Attribute::new_green(self.db, hash, lbrack, attr, arguments, rbrack))
            }
            _ => Err(TryParseFailure::SkipToken),
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration>`
    fn expect_function_declaration(&mut self) -> FunctionDeclarationGreen {
        self.subcontext("a function declaration");
        let function_kw = self.take::<TerminalFunction>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let signature = self.expect_function_signature();
        self.pop_context();

        FunctionDeclaration::new_green(self.db, function_kw, name, generic_params, signature)
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><Block>`
    fn expect_function_with_body(
        &mut self,
        attributes: AttributeListGreen,
        parsing_context: &'a str,
    ) -> FunctionWithBodyGreen {
        self.subcontext(parsing_context);
        let declaration = self.expect_function_declaration();
        let function_body = self.parse_block();
        self.pop_context();
        FunctionWithBody::new_green(self.db, attributes, declaration, function_body)
    }

    /// Assumes the current token is Trait.
    fn expect_trait(&mut self, attributes: AttributeListGreen) -> ItemTraitGreen {
        self.subcontext("a trait");
        let trait_kw = self.take::<TerminalTrait>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.subcontext("a trait body");
            let lbrace = self.take::<TerminalLBrace>();
            let items = TraitItemList::new_green(
                self.db,
                self.parse_attributed_list(
                    Self::try_parse_trait_item,
                    is_of_kind!(rbrace, top_level),
                    TRAIT_ITEM_DESCRIPTION,
                ),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            self.pop_context();
            TraitBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        self.pop_context();
        ItemTrait::new_green(self.db, attributes, trait_kw, name, generic_params, body)
    }

    /// Returns a GreenId of a node with a TraitItem.* kind (see
    /// [syntax::node::ast::TraitItem]), or TryParseFailure if a trait item can't be parsed.
    pub fn try_parse_trait_item(&mut self) -> TryParseResult<TraitItemGreen> {
        let maybe_attributes = self.try_parse_attribute_list(TRAIT_ITEM_DESCRIPTION);

        let (has_attrs, attributes) = match maybe_attributes {
            Ok(attributes) => (true, attributes),
            Err(_) => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalFunction => Ok(self.expect_trait_function(attributes).into()),
            _ => {
                if has_attrs {
                    Ok(self.create_and_report_missing::<TraitItem>(
                        ParserDiagnosticKindMissing::AttributesWithoutTraitItem,
                    ))
                } else {
                    Err(TryParseFailure::SkipToken)
                }
            }
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><SemiColon>`
    fn expect_trait_function(&mut self, attributes: AttributeListGreen) -> TraitItemFunctionGreen {
        self.subcontext("a trait function");
        let declaration = self.expect_function_declaration();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.parse_block().into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };
        self.pop_context();
        TraitItemFunction::new_green(self.db, attributes, declaration, body)
    }

    /// Assumes the current token is Impl.
    fn expect_item_impl(&mut self, attributes: AttributeListGreen) -> ItemGreen {
        match self.expect_impl_inner(attributes) {
            ImplItemOrAlias::Item(green) => green.into(),
            ImplItemOrAlias::Alias(green) => green.into(),
        }
    }

    /// Assumes the current token is Impl.
    fn expect_impl_item_impl(&mut self, attributes: AttributeListGreen) -> ImplItemGreen {
        match self.expect_impl_inner(attributes) {
            ImplItemOrAlias::Item(green) => green.into(),
            ImplItemOrAlias::Alias(green) => green.into(),
        }
    }

    /// Assumes the current token is Impl.
    fn expect_impl_inner(&mut self, attributes: AttributeListGreen) -> ImplItemOrAlias {
        self.subcontext("an impl");
        let impl_kw = self.take::<TerminalImpl>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();

        if self.peek().kind == SyntaxKind::TerminalEq {
            self.subcontext("an impl alias");
            let eq = self.take::<TerminalEq>();
            let impl_path = self.parse_type_path();
            let semicolon = self.parse_token::<TerminalSemicolon>();
            self.pop_context();

            return ImplItemOrAlias::Alias(ItemImplAlias::new_green(
                self.db,
                attributes,
                impl_kw,
                name,
                generic_params,
                eq,
                impl_path,
                semicolon,
            ));
        }

        let of_kw = self.parse_token::<TerminalOf>();
        let trait_path = self.parse_type_path();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.subcontext("an impl body");
            let lbrace = self.take::<TerminalLBrace>();
            let items = ImplItemList::new_green(
                self.db,
                self.parse_attributed_list(
                    Self::try_parse_impl_item,
                    is_of_kind!(rbrace),
                    IMPL_ITEM_DESCRIPTION,
                ),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            self.pop_context();
            ImplBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        self.pop_context();
        ImplItemOrAlias::Item(ItemImpl::new_green(
            self.db,
            attributes,
            impl_kw,
            name,
            generic_params,
            of_kw,
            trait_path,
            body,
        ))
    }

    /// Returns a GreenId of a node with a ImplItem.* kind (see
    /// [syntax::node::ast::ImplItem]), or TryParseFailure if an impl item can't be parsed.
    pub fn try_parse_impl_item(&mut self) -> TryParseResult<ImplItemGreen> {
        let maybe_attributes = self.try_parse_attribute_list(IMPL_ITEM_DESCRIPTION);

        let (has_attrs, attributes) = match maybe_attributes {
            Ok(attributes) => (true, attributes),
            Err(_) => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalFunction => {
                Ok(self.expect_function_with_body(attributes, "an impl function").into())
            }
            // These are not supported semantically.
            SyntaxKind::TerminalConst => Ok(self.expect_const(attributes).into()),
            SyntaxKind::TerminalModule => Ok(self.expect_module(attributes).into()),
            SyntaxKind::TerminalStruct => Ok(self.expect_struct(attributes).into()),
            SyntaxKind::TerminalEnum => Ok(self.expect_enum(attributes).into()),
            SyntaxKind::TerminalType => Ok(self.expect_type_alias(attributes).into()),
            SyntaxKind::TerminalExtern => Ok(self.expect_extern_impl_item(attributes)),
            SyntaxKind::TerminalUse => Ok(self.expect_use(attributes).into()),
            SyntaxKind::TerminalTrait => Ok(self.expect_trait(attributes).into()),
            SyntaxKind::TerminalImpl => Ok(self.expect_impl_item_impl(attributes)),
            _ => {
                if has_attrs {
                    Ok(self.create_and_report_missing::<ImplItem>(
                        ParserDiagnosticKindMissing::AttributesWithoutImplItem,
                    ))
                } else {
                    Err(TryParseFailure::SkipToken)
                }
            }
        }
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr])
    /// or TryParseFailure if an expression can't be parsed.
    fn try_parse_expr(&mut self) -> TryParseResult<ExprGreen> {
        self.try_parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Allow)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr])
    /// or a node with kind ExprMissing if an expression can't be parsed.
    pub fn parse_expr(&mut self) -> ExprGreen {
        match self.try_parse_expr() {
            Ok(green) => green,
            Err(_) => self
                .create_and_report_missing::<Expr>(ParserDiagnosticKindMissing::MissingExpression),
        }
    }

    /// Assumes the current token is a binary operator. Otherwise it might panic.
    ///
    /// Returns a GreenId of the operator.
    fn parse_binary_operator(&mut self) -> BinaryOperatorGreen {
        // Note that if this code is not reached you might need to add the operator to
        // `get_post_operator_precedence`.
        match self.peek().kind {
            SyntaxKind::TerminalDot => self.take::<TerminalDot>().into(),
            SyntaxKind::TerminalMul => self.take::<TerminalMul>().into(),
            SyntaxKind::TerminalMulEq => self.take::<TerminalMulEq>().into(),
            SyntaxKind::TerminalDiv => self.take::<TerminalDiv>().into(),
            SyntaxKind::TerminalDivEq => self.take::<TerminalDivEq>().into(),
            SyntaxKind::TerminalMod => self.take::<TerminalMod>().into(),
            SyntaxKind::TerminalModEq => self.take::<TerminalModEq>().into(),
            SyntaxKind::TerminalPlus => self.take::<TerminalPlus>().into(),
            SyntaxKind::TerminalPlusEq => self.take::<TerminalPlusEq>().into(),
            SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
            SyntaxKind::TerminalMinusEq => self.take::<TerminalMinusEq>().into(),
            SyntaxKind::TerminalEq => self.take::<TerminalEq>().into(),
            SyntaxKind::TerminalEqEq => self.take::<TerminalEqEq>().into(),
            SyntaxKind::TerminalNeq => self.take::<TerminalNeq>().into(),
            SyntaxKind::TerminalLT => self.take::<TerminalLT>().into(),
            SyntaxKind::TerminalGT => self.take::<TerminalGT>().into(),
            SyntaxKind::TerminalLE => self.take::<TerminalLE>().into(),
            SyntaxKind::TerminalGE => self.take::<TerminalGE>().into(),
            SyntaxKind::TerminalAnd => self.take::<TerminalAnd>().into(),
            SyntaxKind::TerminalAndAnd => self.take::<TerminalAndAnd>().into(),
            SyntaxKind::TerminalOrOr => self.take::<TerminalOrOr>().into(),
            SyntaxKind::TerminalOr => self.take::<TerminalOr>().into(),
            SyntaxKind::TerminalXor => self.take::<TerminalXor>().into(),
            _ => unreachable!(),
        }
    }

    /// Assumes the current token is a unary operator, and returns a GreenId of the operator.
    fn expect_unary_operator(&mut self) -> UnaryOperatorGreen {
        match self.peek().kind {
            SyntaxKind::TerminalAt => self.take::<TerminalAt>().into(),
            SyntaxKind::TerminalNot => self.take::<TerminalNot>().into(),
            SyntaxKind::TerminalBitNot => self.take::<TerminalBitNot>().into(),
            SyntaxKind::TerminalMinus => self.take::<TerminalMinus>().into(),
            SyntaxKind::TerminalMul => self.take::<TerminalMul>().into(),
            _ => unreachable!(),
        }
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr])
    /// or TryParseFailure if such an expression can't be parsed.
    ///
    /// Parsing will be limited by:
    /// `parent_precedence` - parsing of binary operators limited to this.
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_expr_limited(
        &mut self,
        parent_precedence: usize,
        lbrace_allowed: LbraceAllowed,
    ) -> TryParseResult<ExprGreen> {
        let mut expr = self.try_parse_atom_or_unary(lbrace_allowed)?;

        while let Some(precedence) = get_post_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Ok(expr);
            }
            expr = if self.peek().kind == SyntaxKind::TerminalQuestionMark {
                ExprErrorPropagate::new_green(self.db, expr, self.take::<TerminalQuestionMark>())
                    .into()
            } else if self.peek().kind == SyntaxKind::TerminalLBrack {
                self.subcontext("an index expression");
                let lbrack = self.take::<TerminalLBrack>();
                let index_expr = self.parse_expr();
                let rbrack = self.parse_token::<TerminalRBrack>();
                self.pop_context();
                ExprIndexed::new_green(self.db, expr, lbrack, index_expr, rbrack).into()
            } else {
                self.subcontext("a binary expression");
                let op = self.parse_binary_operator();
                let rhs = self.parse_expr_limited(precedence, lbrace_allowed);
                self.pop_context();
                ExprBinary::new_green(self.db, expr, op, rhs).into()
            };
        }
        Ok(expr)
    }

    /// Returns a GreenId of a node with ExprPath, ExprFunctionCall, ExprStructCtorCall,
    /// ExprParenthesized, ExprTuple or ExprUnary kind, or TryParseFailure if such an expression
    /// can't be parsed.
    ///
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_atom_or_unary(
        &mut self,
        lbrace_allowed: LbraceAllowed,
    ) -> TryParseResult<ExprGreen> {
        let Some(precedence) = get_unary_operator_precedence(self.peek().kind) else {
            return self.try_parse_atom(lbrace_allowed);
        };
        self.subcontext("an unary expression");
        let op = self.expect_unary_operator();
        let expr = self.parse_expr_limited(precedence, lbrace_allowed);
        self.pop_context();
        Ok(ExprUnary::new_green(self.db, op, expr).into())
    }

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr]),
    /// excluding ExprBlock, or ExprMissing if such an expression can't be parsed.
    ///
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn parse_expr_limited(
        &mut self,
        parent_precedence: usize,
        lbrace_allowed: LbraceAllowed,
    ) -> ExprGreen {
        match self.try_parse_expr_limited(parent_precedence, lbrace_allowed) {
            Ok(green) => green,
            Err(_) => self
                .create_and_report_missing::<Expr>(ParserDiagnosticKindMissing::MissingExpression),
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprPath|ExprFunctionCall|ExprStructCtorCall|ExprParenthesized|ExprTuple kind, or
    /// TryParseFailure if such an expression can't be parsed.
    ///
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_atom(&mut self, lbrace_allowed: LbraceAllowed) -> TryParseResult<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            SyntaxKind::TerminalIdentifier => {
                // Call parse_path() and not expect_path(), because it's cheap.
                let path = self.parse_path();
                match self.peek().kind {
                    SyntaxKind::TerminalLParen => Ok(self.expect_function_call(path).into()),
                    SyntaxKind::TerminalLBrace if lbrace_allowed == LbraceAllowed::Allow => {
                        Ok(self.expect_constructor_call(path).into())
                    }
                    SyntaxKind::TerminalNot => Ok(self.expect_macro_call(path).into()),
                    _ => Ok(path.into()),
                }
            }
            SyntaxKind::TerminalFalse => Ok(self.take::<TerminalFalse>().into()),
            SyntaxKind::TerminalTrue => Ok(self.take::<TerminalTrue>().into()),
            SyntaxKind::TerminalLiteralNumber => Ok(self.take::<TerminalLiteralNumber>().into()),
            SyntaxKind::TerminalShortString => Ok(self.take::<TerminalShortString>().into()),
            SyntaxKind::TerminalString => Ok(self.take::<TerminalString>().into()),
            SyntaxKind::TerminalLParen => {
                // Note that LBrace is allowed inside parenthesis, even if `lbrace_allowed` is
                // [LbraceAllowed::Forbid].
                Ok(self.expect_parenthesized_expr())
            }
            SyntaxKind::TerminalLBrace if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.parse_block().into())
            }
            SyntaxKind::TerminalMatch if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_match_expr().into())
            }
            SyntaxKind::TerminalIf if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_if_expr().into())
            }
            SyntaxKind::TerminalLoop if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_loop_expr().into())
            }

            _ => {
                // TODO(yuval): report to diagnostics.
                Err(TryParseFailure::SkipToken)
            }
        }
    }

    /// Returns a GreenId of a node with an ExprPath|ExprParenthesized|ExprTuple kind, or
    /// TryParseFailure if such an expression can't be parsed.
    fn try_parse_type_expr(&mut self) -> TryParseResult<ExprGreen> {
        // TODO(yuval): support paths starting with "::".
        match self.peek().kind {
            SyntaxKind::TerminalAt => {
                self.subcontext("a snapshot type expression");
                let op = self.take::<TerminalAt>().into();
                let expr = self.parse_type_expr();
                self.pop_context();
                Ok(ExprUnary::new_green(self.db, op, expr).into())
            }
            SyntaxKind::TerminalIdentifier => Ok(self.parse_type_path().into()),
            SyntaxKind::TerminalLParen => Ok(self.expect_type_tuple_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                Err(TryParseFailure::SkipToken)
            }
        }
    }

    /// Returns a GreenId of a node with an ExprPath|ExprParenthesized|ExprTuple kind, or
    /// ExprMissing if such an expression can't be parsed.
    fn parse_type_expr(&mut self) -> ExprGreen {
        match self.try_parse_type_expr() {
            Ok(expr) => expr,
            Err(_) => self.create_and_report_missing::<Expr>(
                ParserDiagnosticKindMissing::MissingTypeExpression,
            ),
        }
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `\{<StructArgList>\}`
    fn expect_struct_ctor_argument_list_braced(&mut self) -> StructArgListBracedGreen {
        self.subcontext("a braced constructor argument list");
        let lbrace = self.take::<TerminalLBrace>();
        let arg_list = StructArgList::new_green(
            self.db,
            self.parse_separated_list::<StructArg, TerminalComma, StructArgListElementOrSeparatorGreen>(
                Self::try_parse_struct_ctor_argument,
                is_of_kind!(rparen, block, rbrace, top_level),
                "constructor argument",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        self.pop_context();

        StructArgListBraced::new_green(self.db, lbrace, arg_list, rbrace)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `<ArgListParenthesized>`
    fn expect_function_call(&mut self, path: ExprPathGreen) -> ExprFunctionCallGreen {
        self.subcontext("a function call expression");
        let func_name = path;
        let parenthesized_args = self.expect_parenthesized_argument_list();
        self.pop_context();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is TerminalNot.
    /// Expected pattern: `!<WrappedArgList>`
    fn expect_macro_call(&mut self, path: ExprPathGreen) -> ExprInlineMacroGreen {
        self.subcontext("an inline macro call");
        let bang = self.take::<TerminalNot>();
        let macro_name = path;
        let wrapped_expr_list = self.parse_wrapped_arg_list();
        self.pop_context();
        ExprInlineMacro::new_green(self.db, macro_name, bang, wrapped_expr_list)
    }

    /// Returns a GreenId of a node with an ArgListParenthesized|ArgListBracketed|ArgListBraced kind
    /// or TryParseFailure if such an argument list can't be parsed.
    fn parse_wrapped_arg_list(&mut self) -> WrappedArgListGreen {
        match self.peek().kind {
            SyntaxKind::TerminalLParen => {
                self.subcontext("a parenthesized argument list");
                let list = self
                    .expect_wrapped_argument_list::<TerminalLParen, TerminalRParen, _, _>(
                        ArgListParenthesized::new_green,
                    )
                    .into();
                self.pop_context();
                list
            }
            SyntaxKind::TerminalLBrack => {
                self.subcontext("a bracketed argument list");
                let list = self
                    .expect_wrapped_argument_list::<TerminalLBrack, TerminalRBrack, _, _>(
                        ArgListBracketed::new_green,
                    )
                    .into();
                self.pop_context();
                list
            }
            SyntaxKind::TerminalLBrace => {
                self.subcontext("a braced argument list");
                let list = self
                    .expect_wrapped_argument_list::<TerminalLBrace, TerminalRBrace, _, _>(
                        ArgListBraced::new_green,
                    )
                    .into();
                self.pop_context();
                list
            }
            _ => self.create_and_report_missing::<WrappedArgList>(
                ParserDiagnosticKindMissing::MissingExpression,
            ),
        }
    }

    /// Assumes the current token is LTerminal.
    /// Expected pattern: `[LTerminal](<expr>,)*<expr>?[RTerminal]`
    /// Gets `new_green` a green id node builder for the list of the requested type, applies it to
    /// the parsed list and returns the result.
    fn expect_wrapped_argument_list<
        LTerminal: syntax::node::Terminal,
        RTerminal: syntax::node::Terminal,
        ListGreen,
        NewGreen: Fn(&dyn SyntaxGroup, LTerminal::Green, ArgListGreen, RTerminal::Green) -> ListGreen,
    >(
        &mut self,
        new_green: NewGreen,
    ) -> ListGreen {
        let l_term = self.take::<LTerminal>();
        let exprs: Vec<ArgListElementOrSeparatorGreen> = self
            .parse_separated_list::<Arg, TerminalComma, ArgListElementOrSeparatorGreen>(
                Self::try_parse_function_argument,
                is_of_kind!(rparen, rbrace, rbrack, block, top_level),
                "argument",
            );
        let r_term: <RTerminal as TypedSyntaxNode>::Green = self.parse_token::<RTerminal>();
        new_green(self.db, l_term, ArgList::new_green(self.db, exprs), r_term)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\(<ArgList>\)`
    fn expect_parenthesized_argument_list(&mut self) -> ArgListParenthesizedGreen {
        self.in_subcontext("a parenthesized argument list", |slf| {
            slf.expect_wrapped_argument_list::<TerminalLParen, TerminalRParen, _, _>(
                ArgListParenthesized::new_green,
            )
        })
    }
    // TODO(ygg): check all diffs and check if something needs to change.

    /// Tries to parse parenthesized argument list.
    /// Expected pattern: `\(<ArgList>\)`
    fn try_parse_parenthesized_argument_list(&mut self) -> OptionArgListParenthesizedGreen {
        if self.peek().kind == SyntaxKind::TerminalLParen {
            self.expect_parenthesized_argument_list().into()
        } else {
            OptionArgListParenthesizedEmpty::new_green(self.db).into()
        }
    }

    /// Parses a function call's argument, which contains possibly modifiers, and a argument clause.
    fn try_parse_function_argument(&mut self) -> TryParseResult<ArgGreen> {
        let modifiers_list = self.parse_modifier_list();
        let arg_clause = self.try_parse_argument_clause();
        match arg_clause {
            Ok(arg_clause) => {
                let modifiers = ModifierList::new_green(self.db, modifiers_list);
                Ok(Arg::new_green(self.db, modifiers, arg_clause))
            }
            Err(_) if !modifiers_list.is_empty() => {
                self.subcontext("a function argument");
                let modifiers = ModifierList::new_green(self.db, modifiers_list);
                let arg_clause = ArgClauseUnnamed::new_green(self.db, self.parse_expr()).into();
                self.pop_context();
                Ok(Arg::new_green(self.db, modifiers, arg_clause))
            }
            Err(err) => Err(err),
        }
    }

    /// Parses a function call's argument, which is an expression with or without the name
    /// of the argument.
    ///
    /// Possible patterns:
    /// * `<Expr>` (unnamed).
    /// * `<Identifier>: <Expr>` (named).
    /// * `:<Identifier>` (Field init shorthand - syntactic sugar for `a: a`).
    fn try_parse_argument_clause(&mut self) -> TryParseResult<ArgClauseGreen> {
        if self.peek().kind == SyntaxKind::TerminalColon {
            self.subcontext("a shorthand argument clause (`:<identifier>`)");
            let colon = self.take::<TerminalColon>();
            let name = self.parse_identifier();
            self.pop_context();
            return Ok(ArgClauseFieldInitShorthand::new_green(
                self.db,
                colon,
                ExprFieldInitShorthand::new_green(self.db, name),
            )
            .into());
        }

        // Read an expression.
        let value = self.try_parse_expr()?;

        // If the next token is `:` and the expression is an identifier, this is the argument's
        // name.
        if self.peek().kind == SyntaxKind::TerminalColon {
            if let Some(argname) = self.try_extract_identifier(value) {
                self.subcontext("a named argument clause (`<identifier>: <expr>`)");
                let colon = self.take::<TerminalColon>();
                let expr = self.parse_expr();
                self.pop_context();
                return Ok(ArgClauseNamed::new_green(self.db, argname, colon, expr).into());
            }
        }

        Ok(ArgClauseUnnamed::new_green(self.db, value).into())
    }

    /// If the given `expr` is a simple identifier, returns the corresponding green node.
    /// Otherwise, returns `TryParseFailure`.
    fn try_extract_identifier(&self, expr: ExprGreen) -> Option<TerminalIdentifierGreen> {
        // Check that `expr` is `ExprPath`.
        let GreenNode {
            kind: SyntaxKind::ExprPath,
            details: GreenNodeDetails::Node { children: children0, .. },
        } = &self.db.lookup_intern_green(expr.0)
        else {
            return None;
        };

        // Check that it has one child.
        let [path_segment] = children0[..] else {
            return None;
        };

        // Check that `path_segment` is `PathSegmentSimple`.
        let GreenNode {
            kind: SyntaxKind::PathSegmentSimple,
            details: GreenNodeDetails::Node { children: children1, .. },
        } = self.db.lookup_intern_green(path_segment)
        else {
            return None;
        };

        // Check that it has one child.
        let [ident] = children1[..] else {
            return None;
        };

        // Check that it is indeed `TerminalIdentifier`.
        let GreenNode { kind: SyntaxKind::TerminalIdentifier, .. } =
            self.db.lookup_intern_green(ident)
        else {
            return None;
        };

        Some(TerminalIdentifierGreen(ident))
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `<StructArgListBraced>`
    fn expect_constructor_call(&mut self, path: ExprPathGreen) -> ExprStructCtorCallGreen {
        self.subcontext("a constructor call");
        let ctor_name = path;
        let args = self.expect_struct_ctor_argument_list_braced();
        self.pop_context();
        ExprStructCtorCall::new_green(self.db, ctor_name, args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\((<expr>,)*<expr>?\)`
    /// Returns a GreenId of a node with kind ExprParenthesized|ExprTuple.
    fn expect_parenthesized_expr(&mut self) -> ExprGreen {
        self.subcontext("a parenthesized expression (single or a tuple)");
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                "expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();
        self.pop_context();

        if let [ExprListElementOrSeparatorGreen::Element(expr)] = &exprs[..] {
            // We have exactly one item and no separator --> This is not a tuple.
            ExprParenthesized::new_green(self.db, lparen, *expr, rparen).into()
        } else {
            ExprListParenthesized::new_green(
                self.db,
                lparen,
                ExprList::new_green(self.db, exprs),
                rparen,
            )
            .into()
        }
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\((<type_expr>,)*<type_expr>?\)`
    /// Returns a GreenId of a node with kind ExprTuple.
    fn expect_type_tuple_expr(&mut self) -> ExprGreen {
        self.subcontext("a type-tuple expression");
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_type_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                "type expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();
        self.pop_context();
        if let [ExprListElementOrSeparatorGreen::Element(_)] = &exprs[..] {
            self.diagnostics.add(ParserDiagnostic {
                file_id: self.file_id,
                kind: ParserDiagnosticKind::Missing {
                    kind: ParserDiagnosticKindMissing::MissingToken(SyntaxKind::TokenComma),
                    parsing_context: self.parsing_context.last().unwrap().into(),
                },
                span: TextSpan { start: self.offset, end: self.offset },
            });
        }
        ExprListParenthesized::new_green(
            self.db,
            lparen,
            ExprList::new_green(self.db, exprs),
            rparen,
        )
        .into()
    }

    /// Assumes the current token is DotDot.
    /// Expected pattern: `\.\.<Expr>`
    fn expect_struct_argument_tail(&mut self) -> StructArgTailGreen {
        self.subcontext("a constructor argument tail (`..<expr>`)");
        let dotdot = self.take::<TerminalDotDot>(); // ..
        // TODO(yuval): consider changing this to SimpleExpr once it exists.
        let expr = self.parse_expr();
        self.pop_context();
        StructArgTail::new_green(self.db, dotdot, expr)
    }

    // For the similar syntax in Rust, see
    // https://doc.rust-lang.org/book/ch05-01-defining-structs.html#creating-instances-from-other-instances-with-struct-update-syntax.
    /// Like parse_argument, but also allows a struct-arg-tail, e.g. 'let s2 = S{"s2", ..s1};'
    /// Returns a GreenId of a node with kind StructArgSingle|StructArgTail.
    fn try_parse_struct_ctor_argument(&mut self) -> TryParseResult<StructArgGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalDotDot => Ok(self.expect_struct_argument_tail().into()),
            _ => self.try_parse_argument_single().map(|arg| arg.into()),
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

    /// Returns a GreenId of a node with kind OptionExprClause or OptionExprClauseEmpty if an
    /// argument expression `("Expr")` can't be parsed.
    fn parse_option_expression_clause(&mut self) -> OptionExprClauseGreen {
        if self.peek().kind == SyntaxKind::TerminalSemicolon {
            OptionExprClauseEmpty::new_green(self.db).into()
        } else {
            let value = self.parse_expr();
            ExprClause::new_green(self.db, value).into()
        }
    }

    /// Returns a GreenId of a node with kind StructArgSingle.
    fn try_parse_argument_single(&mut self) -> TryParseResult<StructArgSingleGreen> {
        let identifier = self.try_parse_identifier()?;
        let struct_arg_expr =
            self.in_subcontext("a constructor argument", Self::parse_option_struct_arg_expression); // :<expr>
        Ok(StructArgSingle::new_green(self.db, identifier, struct_arg_expr))
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let skipped_tokens = self.skip_until(is_of_kind!(rbrace, lbrace, top_level, block));

        if let Err(SkippedError(span)) = skipped_tokens {
            self.diagnostics.add(ParserDiagnostic {
                file_id: self.file_id,
                kind: ParserDiagnosticKind::SkippedElement { element_name: "'{'".into() },
                span,
            });
        }

        if is_of_kind!(rbrace, top_level)(self.peek().kind) {
            return ExprBlock::new_green(
                self.db,
                self.create_and_report_missing_terminal::<TerminalLBrace>(),
                StatementList::new_green(self.db, vec![]),
                TerminalRBrace::missing(self.db),
            );
        }
        // Don't report diagnostic if one has already been reported.

        self.subcontext("a block");
        let lbrace = self.parse_token_ex::<TerminalLBrace>(skipped_tokens.is_ok());
        let statements = StatementList::new_green(
            self.db,
            self.parse_list(Self::try_parse_statement, is_of_kind!(rbrace, top_level), "statement"),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        self.pop_context();
        ExprBlock::new_green(self.db, lbrace, statements, rbrace)
    }

    /// Assumes the current token is `Match`.
    /// Expected pattern: `match <expr> \{<MatchArm>*\}`
    fn expect_match_expr(&mut self) -> ExprMatchGreen {
        self.subcontext("a match expression");
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
        self.pop_context();
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, arms, rbrace)
    }

    /// Assumes the current token is `If`.
    /// Expected pattern: `if <expr> <block> [else <block>]`.
    fn expect_if_expr(&mut self) -> ExprIfGreen {
        self.subcontext("an if expression");
        let if_kw = self.take::<TerminalIf>();
        let condition = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
        let if_block = self.parse_block();

        let else_clause: OptionElseClauseGreen = if self.peek().kind == SyntaxKind::TerminalElse {
            let else_kw = self.take::<TerminalElse>();
            let else_block_or_if = if self.peek().kind == SyntaxKind::TerminalIf {
                let else_if_expr = self.in_subcontext("an else-if clause", Self::expect_if_expr);
                BlockOrIfGreen::from(else_if_expr)
            } else {
                let else_expr = self.in_subcontext("an else clause", Self::parse_block);
                BlockOrIfGreen::from(else_expr)
            };
            ElseClause::new_green(self.db, else_kw, else_block_or_if).into()
        } else {
            OptionElseClauseEmpty::new_green(self.db).into()
        };

        self.pop_context();
        ExprIf::new_green(self.db, if_kw, condition, if_block, else_clause)
    }

    /// Assumes the current token is `Loop`.
    /// Expected pattern: `loop <block>`.
    fn expect_loop_expr(&mut self) -> ExprLoopGreen {
        self.subcontext("a loop expression");
        let loop_kw = self.take::<TerminalLoop>();
        let body = self.parse_block();
        self.pop_context();

        ExprLoop::new_green(self.db, loop_kw, body)
    }

    /// Returns a GreenId of a node with a MatchArm kind or TryParseFailure if a match arm can't be
    /// parsed.
    pub fn try_parse_match_arm(&mut self) -> TryParseResult<MatchArmGreen> {
        let pattern = self.try_parse_pattern()?;
        self.subcontext("a match arm");
        let arrow = self.parse_token::<TerminalMatchArrow>();
        let expr = self.parse_expr();
        self.pop_context();
        Ok(MatchArm::new_green(self.db, pattern, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see
    /// [syntax::node::ast::Pattern]) or TryParseFailure if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> TryParseResult<PatternGreen> {
        let modifier_list = self.parse_modifier_list();
        if !modifier_list.is_empty() {
            let modifiers = ModifierList::new_green(self.db, modifier_list);
            let name = self.in_subcontext("a pattern", Self::parse_identifier);
            return Ok(PatternIdentifier::new_green(self.db, modifiers, name).into());
        };

        // TODO(yuval): Support "Or" patterns.
        Ok(match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => self.take::<TerminalLiteralNumber>().into(),
            SyntaxKind::TerminalShortString => self.take::<TerminalShortString>().into(),
            SyntaxKind::TerminalUnderscore => self.take::<TerminalUnderscore>().into(),
            SyntaxKind::TerminalIdentifier => {
                self.subcontext("a pattern");
                // TODO(ilya): Consider parsing a single identifier as PatternIdentifier rather
                // then ExprPath.
                let path = self.parse_path();
                let pattern = match self.peek().kind {
                    SyntaxKind::TerminalLBrace => {
                        self.subcontext("a struct pattern");
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
                        let rbrace = self.parse_token::<TerminalRBrace>();
                        self.pop_context();
                        PatternStruct::new_green(self.db, path, lbrace, params, rbrace).into()
                    }
                    SyntaxKind::TerminalLParen => {
                        self.subcontext("an enum pattern");
                        // Enum pattern.
                        let lparen = self.take::<TerminalLParen>();
                        let pattern = self.parse_pattern();
                        let rparen = self.parse_token::<TerminalRParen>();
                        let inner_pattern =
                            PatternEnumInnerPattern::new_green(self.db, lparen, pattern, rparen);
                        self.pop_context();
                        PatternEnum::new_green(self.db, path, inner_pattern.into()).into()
                    }
                    _ => {
                        let children = match self.db.lookup_intern_green(path.0).details {
                            GreenNodeDetails::Node { children, width: _ } => children,
                            _ => return Err(TryParseFailure::SkipToken),
                        };
                        // If the path has more than 1 element assume it's a simplified Enum variant
                        // Eg. MyEnum::A(()) ~ MyEnum::A
                        // Multi-element path identifiers aren't allowed, for now this mechanism is
                        // sufficient.
                        match children.len() {
                            // 0 => return None, - unreachable
                            1 => path.into(),
                            _ => PatternEnum::new_green(
                                self.db,
                                path,
                                OptionPatternEnumInnerPatternEmpty::new_green(self.db).into(),
                            )
                            .into(),
                        }
                    }
                };
                self.pop_context();
                pattern
            }
            SyntaxKind::TerminalLParen => {
                self.subcontext("a tuple pattern");
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
                self.pop_context();
                PatternTuple::new_green(self.db, lparen, patterns, rparen).into()
            }
            _ => return Err(TryParseFailure::SkipToken),
        })
    }
    /// Returns a GreenId of a node with some Pattern kind (see
    /// [syntax::node::ast::Pattern]).
    fn parse_pattern(&mut self) -> PatternGreen {
        // If not found, return a missing underscore pattern.
        match self.try_parse_pattern() {
            Ok(pattern) => pattern,
            Err(_) => self.create_and_report_missing_terminal::<TerminalUnderscore>().into(),
        }
    }

    /// Returns a GreenId of a syntax in side a struct pattern. Example:
    /// `MyStruct { param0, param1: _, .. }`.
    fn try_parse_pattern_struct_param(&mut self) -> TryParseResult<PatternStructParamGreen> {
        Ok(match self.peek().kind {
            SyntaxKind::TerminalDotDot => self.take::<TerminalDotDot>().into(),
            _ => {
                let modifier_list = self.parse_modifier_list();
                let name = if modifier_list.is_empty() {
                    self.try_parse_identifier()?
                } else {
                    self.in_subcontext("a struct pattern parameter", Self::parse_identifier)
                };
                let modifiers = ModifierList::new_green(self.db, modifier_list);
                if self.peek().kind == SyntaxKind::TerminalColon {
                    self.subcontext("a struct pattern bound parameter (`<name>: <pattern>`)");
                    let colon = self.take::<TerminalColon>();
                    let pattern = self.parse_pattern();
                    self.pop_context();
                    PatternStructParamWithExpr::new_green(self.db, modifiers, name, colon, pattern)
                        .into()
                } else {
                    PatternIdentifier::new_green(self.db, modifiers, name).into()
                }
            }
        })
    }

    // ------------------------------- Statements -------------------------------

    /// Returns a GreenId of a node with a Statement.* kind (see
    /// [syntax::node::ast::Statement]) or TryParseFailure if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> TryParseResult<StatementGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalLet => {
                self.subcontext("a let statement");
                let let_kw = self.take::<TerminalLet>();
                let pattern = self.parse_pattern();
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token::<TerminalEq>();
                let rhs = self.parse_expr();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                self.pop_context();
                Ok(StatementLet::new_green(
                    self.db,
                    let_kw,
                    pattern,
                    type_clause,
                    eq,
                    rhs,
                    semicolon,
                )
                .into())
            }
            SyntaxKind::TerminalContinue => {
                self.subcontext("a continue statement");
                let continue_kw = self.take::<TerminalContinue>();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                self.pop_context();
                Ok(StatementContinue::new_green(self.db, continue_kw, semicolon).into())
            }
            SyntaxKind::TerminalReturn => {
                self.subcontext("a return statement");
                let return_kw = self.take::<TerminalReturn>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                self.pop_context();
                Ok(StatementReturn::new_green(self.db, return_kw, expr, semicolon).into())
            }
            SyntaxKind::TerminalBreak => {
                self.subcontext("a break statement");
                let break_kw = self.take::<TerminalBreak>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                self.pop_context();
                Ok(StatementBreak::new_green(self.db, break_kw, expr, semicolon).into())
            }
            _ => match self.try_parse_expr() {
                Ok(expr) => {
                    self.subcontext("an expression statement");
                    let optional_semicolon = if self.peek().kind == SyntaxKind::TerminalSemicolon {
                        self.take::<TerminalSemicolon>().into()
                    } else {
                        OptionTerminalSemicolonEmpty::new_green(self.db).into()
                    };
                    self.pop_context();
                    Ok(StatementExpr::new_green(self.db, expr, optional_semicolon).into())
                }
                Err(err) => Err(err),
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

    /// Parses a type clause of the form: `: <type>`.
    fn parse_type_clause(&mut self, error_recovery: ErrorRecovery) -> TypeClauseGreen {
        match self.try_parse_type_clause() {
            Some(green) => green,
            None => {
                let res = self.create_and_report_missing::<TypeClause>(
                    ParserDiagnosticKindMissing::MissingTypeClause,
                );
                self.skip_until(error_recovery.should_stop).ok();
                res
            }
        }
    }
    fn try_parse_type_clause(&mut self) -> Option<TypeClauseGreen> {
        if self.peek().kind == SyntaxKind::TerminalColon {
            self.subcontext("a type clause");
            let colon = self.take::<TerminalColon>();
            let ty = self.parse_type_expr();
            self.pop_context();
            Some(TypeClause::new_green(self.db, colon, ty))
        } else {
            None
        }
    }

    /// Returns a GreenId of a node with kind ReturnTypeClause or OptionReturnTypeClauseEmpty if a
    /// return type clause can't be parsed.
    fn parse_option_return_type_clause(&mut self) -> OptionReturnTypeClauseGreen {
        if self.peek().kind == SyntaxKind::TerminalArrow {
            self.subcontext("a return type clause");
            let arrow = self.take::<TerminalArrow>();
            let return_type = self.parse_type_expr();
            self.pop_context();
            ReturnTypeClause::new_green(self.db, arrow, return_type).into()
        } else {
            OptionReturnTypeClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ImplicitsClause or OptionImplicitsClauseEmpty if a
    /// implicits-clause can't be parsed.
    fn parse_option_implicits_clause(&mut self) -> OptionImplicitsClauseGreen {
        if self.peek().kind == SyntaxKind::TerminalImplicits {
            self.subcontext("an implicits clause");
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
            self.pop_context();
            ImplicitsClause::new_green(self.db, implicits_kw, lparen, implicits, rparen).into()
        } else {
            OptionImplicitsClauseEmpty::new_green(self.db).into()
        }
    }

    /// Returns a GreenId of a node with kind ParamList.
    fn parse_param_list(&mut self) -> ParamListGreen {
        self.subcontext("a parameter list");
        let param_list = ParamList::new_green(
            self.db,
            self.parse_separated_list::<Param, TerminalComma, ParamListElementOrSeparatorGreen>(
                Self::try_parse_param,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                "parameter",
            ),
        );
        self.pop_context();
        param_list
    }

    /// Returns a GreenId of a node with kind Modifier or TryParseFailure if a modifier can't be
    /// parsed.
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

    /// Returns a GreenId of a node with kind Param or TryParseFailure if a parameter can't be
    /// parsed.
    fn try_parse_param(&mut self) -> TryParseResult<ParamGreen> {
        let modifier_list = self.parse_modifier_list();
        let name = if modifier_list.is_empty() {
            let name = self.try_parse_identifier()?;
            self.subcontext("a parameter");
            name
        } else {
            // If we had modifiers then the identifier is not optional and can't be '_'.
            self.subcontext("a parameter");
            self.parse_identifier()
        };

        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(comma, rparen, top_level),
        });
        self.pop_context();
        Ok(Param::new_green(
            self.db,
            ModifierList::new_green(self.db, modifier_list),
            name,
            type_clause,
        ))
    }

    /// Returns a GreenId of a node with kind MemberList.
    fn parse_member_list(&mut self) -> MemberListGreen {
        MemberList::new_green(
            self.db,
            self.parse_separated_list::<Member, TerminalComma, MemberListElementOrSeparatorGreen>(
                Self::try_parse_member,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                "struct member",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Member or TryParseFailure if a struct member can't be
    /// parsed.
    fn try_parse_member(&mut self) -> TryParseResult<MemberGreen> {
        let attributes = self.try_parse_attribute_list("Struct member");
        let (name, attributes) = match attributes {
            Ok(attributes) => {
                self.subcontext("a struct member");
                (self.parse_identifier(), attributes)
            }
            Err(_) => {
                let ident = self.try_parse_identifier()?;
                self.subcontext("a struct member");
                (ident, AttributeList::new_green(self.db, vec![]))
            }
        };
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(comma, rbrace, top_level),
        });
        self.pop_context();
        Ok(Member::new_green(self.db, attributes, name, type_clause))
    }

    /// Returns a GreenId of a node with kind VariantList.
    fn parse_variant_list(&mut self) -> VariantListGreen {
        self.subcontext("a variant list");
        let variant_list = VariantList::new_green(
            self.db,
            self.parse_separated_list::<Variant, TerminalComma, VariantListElementOrSeparatorGreen>(
                Self::try_parse_variant,
                is_of_kind!(rparen, block, lbrace, rbrace, top_level),
                "variant",
            ),
        );
        self.pop_context();
        variant_list
    }

    /// Returns a GreenId of a node with kind Variant or TryParseFailure if an enum variant can't be
    /// parsed.
    fn try_parse_variant(&mut self) -> TryParseResult<VariantGreen> {
        let attributes = self.try_parse_attribute_list("Enum variant");
        let (name, attributes) = match attributes {
            Ok(attributes) => {
                self.subcontext("an enum variant");
                (self.parse_identifier(), attributes)
            }
            Err(_) => {
                let ident = self.try_parse_identifier()?;
                self.subcontext("an enum variant");
                (ident, AttributeList::new_green(self.db, vec![]))
            }
        };

        let type_clause = self.parse_option_type_clause();
        self.pop_context();
        Ok(Variant::new_green(self.db, attributes, name, type_clause))
    }

    /// Expected pattern: `<PathSegment>(::<PathSegment>)*`
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_path(&mut self) -> ExprPathGreen {
        self.subcontext("a path expression");
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
        self.pop_context();

        ExprPath::new_green(self.db, children)
    }
    /// Returns a GreenId of a node with kind ExprPath or TryParseFailure if a path can't be parsed.
    fn try_parse_path(&mut self) -> TryParseResult<ExprPathGreen> {
        if self.is_peek_identifier_like() {
            Ok(self.parse_path())
        } else {
            Err(TryParseFailure::SkipToken)
        }
    }

    /// Expected pattern: `(<PathSegment>::)*<PathSegment>(::){0,1}<GenericArgs>`.
    ///
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_type_path(&mut self) -> ExprPathGreen {
        self.subcontext("a type path");
        let mut children: Vec<ExprPathElementOrSeparatorGreen> = vec![];
        loop {
            let (segment, optional_separator) = self.parse_type_path_segment();
            children.push(segment.into());

            if let Some(separator) = optional_separator {
                children.push(separator.into());
                continue;
            }
            break;
        }
        self.pop_context();

        ExprPath::new_green(self.db, children)
    }

    /// Returns a PathSegment and an optional separator.
    fn parse_path_segment(&mut self) -> (PathSegmentGreen, Option<TerminalColonColonGreen>) {
        let identifier = match self.try_parse_identifier() {
            Ok(identifier) => identifier,
            Err(_) => {
                return (
                    self.create_and_report_missing::<PathSegment>(
                        ParserDiagnosticKindMissing::MissingPathSegment,
                    ),
                    // TODO(ilya, 10/10/2022): Should we continue parsing the path here?
                    None,
                );
            }
        };
        match self.try_parse_token::<TerminalColonColon>() {
            Ok(separator) if self.peek().kind == SyntaxKind::TerminalLT => (
                PathSegmentWithGenericArgs::new_green(
                    self.db,
                    identifier,
                    separator.into(),
                    self.expect_generic_args(),
                )
                .into(),
                self.try_parse_token::<TerminalColonColon>().ok(),
            ),
            optional_separator => {
                (PathSegmentSimple::new_green(self.db, identifier).into(), optional_separator.ok())
            }
        }
    }

    /// Returns a Typed PathSegment or a normal PathSegment.
    /// Additionally returns an optional separators.
    fn parse_type_path_segment(&mut self) -> (PathSegmentGreen, Option<TerminalColonColonGreen>) {
        let identifier = match self.try_parse_identifier() {
            Ok(identifier) => identifier,
            Err(_) => {
                return (
                    self.create_and_report_missing::<PathSegment>(
                        ParserDiagnosticKindMissing::MissingPathSegment,
                    ),
                    // TODO(ilya, 10/10/2022): Should we continue parsing the path here?
                    None,
                );
            }
        };
        match self.try_parse_token::<TerminalColonColon>() {
            Err(_) if self.peek().kind == SyntaxKind::TerminalLT => (
                PathSegmentWithGenericArgs::new_green(
                    self.db,
                    identifier,
                    OptionTerminalColonColonEmpty::new_green(self.db).into(),
                    self.expect_generic_args(),
                )
                .into(),
                None,
            ),
            // This is here to preserve backwards compatibility.
            // This allows Option::<T> to still work after this change.
            Ok(separator) if self.peek().kind == SyntaxKind::TerminalLT => (
                PathSegmentWithGenericArgs::new_green(
                    self.db,
                    identifier,
                    separator.into(),
                    self.expect_generic_args(),
                )
                .into(),
                self.try_parse_token::<TerminalColonColon>().ok(),
            ),
            optional_separator => {
                (PathSegmentSimple::new_green(self.db, identifier).into(), optional_separator.ok())
            }
        }
    }

    /// Returns a GreenId of a node with an
    /// ExprLiteral|ExprPath|ExprParenthesized|ExprTuple|ExprUnderscore kind, or TryParseFailure if
    /// such an expression can't be parsed.
    fn try_parse_generic_arg(&mut self) -> TryParseResult<GenericArgGreen> {
        if self.peek().kind == SyntaxKind::TerminalUnderscore {
            let underscore = self.take::<TerminalUnderscore>().into();
            return Ok(GenericArgUnnamed::new_green(self.db, underscore).into());
        }

        self.subcontext("a generic argument");
        let expr = match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => self.take::<TerminalLiteralNumber>().into(),
            SyntaxKind::TerminalMinus => {
                let op = self.take::<TerminalMinus>().into();
                let expr = self.parse_token::<TerminalLiteralNumber>().into();
                ExprUnary::new_green(self.db, op, expr).into()
            }
            SyntaxKind::TerminalShortString => self.take::<TerminalShortString>().into(),
            SyntaxKind::TerminalLBrace => self.parse_block().into(),
            _ => {
                // In this case we don't yet know we parse a generic argument. Pop the context and
                // set it back if we see a type expression.
                self.pop_context();
                let expr = self.try_parse_type_expr()?;
                self.subcontext("a generic argument");
                expr
            }
        };

        // If the next token is `:` and the expression is an identifier, this is the argument's
        // name.
        if self.peek().kind == SyntaxKind::TerminalColon {
            if let Some(argname) = self.try_extract_identifier(expr) {
                let colon = self.take::<TerminalColon>();
                let expr = if self.peek().kind == SyntaxKind::TerminalUnderscore {
                    self.take::<TerminalUnderscore>().into()
                } else {
                    let expr = self.try_parse_type_expr()?; // TODO(yg): fix, this should be parse.
                    GenericArgValueExpr::new_green(self.db, expr).into()
                };
                return Ok(GenericArgNamed::new_green(self.db, argname, colon, expr).into());
            }
        }
        self.pop_context();
        Ok(GenericArgUnnamed::new_green(
            self.db,
            GenericArgValueExpr::new_green(self.db, expr).into(),
        )
        .into())
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericArgList> \>`
    fn expect_generic_args(&mut self) -> GenericArgsGreen {
        self.subcontext("generic arguments");
        let langle = self.take::<TerminalLT>();
        let generic_args = GenericArgList::new_green(
            self.db,
            self.parse_separated_list::<GenericArg, TerminalComma, GenericArgListElementOrSeparatorGreen>(
                Self::try_parse_generic_arg,
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, top_level),
                "generic arg",
            ),
        );
        let rangle = self.parse_token::<TerminalGT>();
        self.pop_context();
        GenericArgs::new_green(self.db, langle, generic_args, rangle)
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericParamList> \>`
    fn expect_generic_params(&mut self) -> WrappedGenericParamListGreen {
        self.subcontext("generic parameters");
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
        self.pop_context();
        WrappedGenericParamList::new_green(self.db, langle, generic_params, rangle)
    }

    fn parse_optional_generic_params(&mut self) -> OptionWrappedGenericParamListGreen {
        if self.peek().kind != SyntaxKind::TerminalLT {
            return OptionWrappedGenericParamListEmpty::new_green(self.db).into();
        }
        self.expect_generic_params().into()
    }

    fn try_parse_generic_param(&mut self) -> TryParseResult<GenericParamGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalConst => {
                self.subcontext("a const generic parameter");
                let const_kw = self.take::<TerminalConst>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let ty = self.parse_type_expr();
                self.pop_context();
                Ok(GenericParamConst::new_green(self.db, const_kw, name, colon, ty).into())
            }
            SyntaxKind::TerminalImpl => {
                self.subcontext("an impl generic parameter");
                let impl_kw = self.take::<TerminalImpl>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let trait_path = self.parse_type_path();
                self.pop_context();
                Ok(GenericParamImpl::new_green(self.db, impl_kw, name, colon, trait_path).into())
            }
            _ => Ok(GenericParamType::new_green(self.db, self.try_parse_identifier()?).into()),
        }
    }

    // ------------------------------- Helpers -------------------------------

    /// Parses a list of elements (without separators), where the elements are parsed using
    /// `try_parse_list_element`.
    /// Returns the list of green ids of the elements.
    ///
    /// `should_stop` is a predicate to decide how to proceed in case an element can't be parsed,
    /// according to the current token. If it returns true, the parsing of the list stops. If it
    /// returns false, the current token is skipped and we try to parse an element again.
    ///
    /// `expected_element` is a description of the expected element.
    fn parse_list<ElementGreen>(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> TryParseResult<ElementGreen>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &str,
    ) -> Vec<ElementGreen> {
        let mut children: Vec<ElementGreen> = Vec::new();
        loop {
            let parse_result = try_parse_list_element(self);
            match parse_result {
                Ok(element_green) => {
                    children.push(element_green);
                }
                Err(err) => {
                    if should_stop(self.peek().kind) {
                        break;
                    }
                    if err == TryParseFailure::SkipToken {
                        self.skip_token(ParserDiagnosticKind::SkippedElement {
                            element_name: expected_element.into(),
                        });
                    }
                }
            }
        }
        children
    }

    /// Parses a list of elements (without separators) that can be prefixed with attributes
    /// (#[...]), where the elements are parsed using `try_parse_list_element`.
    /// Returns the list of green ids of the elements.
    ///
    /// `should_stop` is a predicate to decide how to proceed in case an element can't be parsed,
    /// according to the current token. If it returns true, the parsing of the list stops. If it
    /// returns false, the current token is skipped and we try to parse an element again.
    ///
    /// `expected_element` is a description of the expected element. Note: it should not include
    /// "attribute".
    fn parse_attributed_list<ElementGreen>(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> TryParseResult<ElementGreen>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &str,
    ) -> Vec<ElementGreen> {
        self.parse_list::<ElementGreen>(
            try_parse_list_element,
            should_stop,
            &format!("{expected_element} or an attribute"),
        )
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
        try_parse_list_element: fn(&mut Self) -> TryParseResult<Element::Green>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &'static str,
    ) -> Vec<ElementOrSeparatorGreen>
    where
        ElementOrSeparatorGreen: From<Separator::Green> + From<Element::Green>,
    {
        let mut children: Vec<ElementOrSeparatorGreen> = Vec::new();
        loop {
            match try_parse_list_element(self) {
                Err(_) if should_stop(self.peek().kind) => {
                    break;
                }
                Err(_) => {
                    self.skip_token(ParserDiagnosticKind::SkippedElement {
                        element_name: expected_element.into(),
                    });
                    continue;
                }
                Ok(element) => {
                    children.push(element.into());
                }
            };

            let separator = match self.try_parse_token::<Separator>() {
                Err(_) if should_stop(self.peek().kind) => {
                    break;
                }
                Err(_) => self.create_and_report_missing::<Separator>(
                    ParserDiagnosticKindMissing::MissingToken(Separator::KIND),
                ),
                Ok(separator) => separator,
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
        self.offset = self.offset.add_width(self.current_width);
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
        if self.peek().kind == SyntaxKind::TerminalEndOfFile {
            self.diagnostics.add(ParserDiagnostic {
                file_id: self.file_id,
                kind: diagnostic_kind,
                span: TextSpan { start: self.offset, end: self.offset },
            });
            return;
        }
        let terminal = self.take_raw();

        let diag_start = self.offset.add_width(
            terminal
                .leading_trivia
                .iter()
                .map(|trivium| trivium.0.width(self.db))
                .sum::<TextWidth>(),
        );
        let diag_end = diag_start.add_width(TextWidth::from_str(&terminal.text));

        // Add to pending trivia.
        self.pending_trivia.extend(terminal.leading_trivia);
        self.pending_trivia.push(TokenSkipped::new_green(self.db, terminal.text).into());
        self.pending_trivia.extend(terminal.trailing_trivia);
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: diagnostic_kind,
            span: TextSpan { start: diag_start, end: diag_end },
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

    /// Skips terminals until `should_stop` returns `true`.
    ///
    /// Returns the span of the skipped terminals, if any.
    fn skip_until(&mut self, should_stop: fn(SyntaxKind) -> bool) -> Result<(), SkippedError> {
        let mut diag_start = None;
        let mut diag_end = None;
        while !should_stop(self.peek().kind) {
            let terminal = self.take_raw();
            diag_start.get_or_insert(self.offset);
            diag_end = Some(self.offset.add_width(TextWidth::from_str(&terminal.text)));

            self.pending_trivia.extend(terminal.leading_trivia);
            self.pending_trivia.push(TokenSkipped::new_green(self.db, terminal.text).into());
            self.pending_trivia.extend(terminal.trailing_trivia);
        }
        if let (Some(diag_start), Some(diag_end)) = (diag_start, diag_end) {
            Err(SkippedError(TextSpan { start: diag_start, end: diag_end }))
        } else {
            Ok(())
        }
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
    /// TryParseFailure.
    /// Note that this function should not be called for 'TerminalIdentifier' -
    /// try_parse_identifier() should be used instead.
    fn try_parse_token<Terminal: syntax::node::Terminal>(
        &mut self,
    ) -> TryParseResult<Terminal::Green> {
        if Terminal::KIND == self.peek().kind {
            Ok(self.take::<Terminal>())
        } else {
            Err(TryParseFailure::SkipToken)
        }
    }

    /// If the current token is of kind `token_kind`, returns a GreenId of a node with this kind.
    /// Otherwise, returns Token::Missing.
    ///
    /// Note that this function should not be called for 'TerminalIdentifier' - parse_identifier()
    /// should be used instead.
    fn parse_token<Terminal: syntax::node::Terminal>(&mut self) -> Terminal::Green {
        self.parse_token_ex::<Terminal>(true)
    }

    /// Same as [Self::parse_token], except that the diagnostic may be omitted.
    fn parse_token_ex<Terminal: syntax::node::Terminal>(
        &mut self,
        report_diagnostic: bool,
    ) -> Terminal::Green {
        match self.try_parse_token::<Terminal>() {
            Ok(green) => green,
            Err(_) => {
                if report_diagnostic {
                    self.create_and_report_missing_terminal::<Terminal>()
                } else {
                    Terminal::missing(self.db)
                }
            }
        }
    }

    /// Enter a new parsing subcontext.
    fn subcontext(&mut self, context: &str) {
        self.parsing_context.push(context.into());
    }
    /// Pop the last parsing subcontext.
    fn pop_context(&mut self) {
        self.parsing_context.pop();
    }
    /// Run the given closure in the given context.
    fn in_subcontext<F, T>(&mut self, subcontext: &str, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.subcontext(subcontext);
        let res = f(self);
        self.pop_context();
        res
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

/// Indicates that [Parser::skip_until] skipped some terminals.
struct SkippedError(TextSpan);

/// Defines the parser behavior in the case of a parsing error.
struct ErrorRecovery {
    /// In the case of a parsing error, tokens will be skipped until `should_stop`
    /// returns `true`. For example, one can stop at tokens such as `,` and `}`.
    should_stop: fn(SyntaxKind) -> bool,
}

enum ExternItem {
    Function(ItemExternFunctionGreen),
    Type(ItemExternTypeGreen),
}

enum ImplItemOrAlias {
    Item(ItemImplGreen),
    Alias(ItemImplAliasGreen),
}
