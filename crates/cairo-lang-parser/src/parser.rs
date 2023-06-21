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

use crate::diagnostic::ParserDiagnosticKind;
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
        };
        let green = parser.parse_syntax_file();
        SyntaxFile::from_syntax_node(db, SyntaxNode::new_root(db, green))
    }

    /// Returns a GreenId of an ExprMissing and adds a diagnostic describing it.
    fn create_and_report_missing<T: TypedSyntaxNode>(
        &mut self,
        missing_kind: ParserDiagnosticKind,
    ) -> T::Green {
        let next_offset = self.offset.add_width(self.current_width - self.last_trivia_length);
        self.diagnostics.add(ParserDiagnostic {
            file_id: self.file_id,
            kind: missing_kind,
            span: TextSpan { start: next_offset, end: next_offset },
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

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::Item]), or none if
    /// a top-level item can't be parsed.
    pub fn try_parse_top_level_item(&mut self) -> Option<ItemGreen> {
        let maybe_attributes = self
            .try_parse_attribute_list(TOP_LEVEL_ITEM_DESCRIPTION, is_of_kind!(rbrace, top_level));

        let (has_attrs, attributes) = match maybe_attributes {
            Some(attributes) => (true, attributes),
            None => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalConst => Some(self.expect_const(attributes).into()),
            SyntaxKind::TerminalModule => Some(self.expect_module(attributes).into()),
            SyntaxKind::TerminalStruct => Some(self.expect_struct(attributes).into()),
            SyntaxKind::TerminalEnum => Some(self.expect_enum(attributes).into()),
            SyntaxKind::TerminalType => Some(self.expect_type_alias(attributes).into()),
            SyntaxKind::TerminalExtern => Some(self.expect_extern_item(attributes)),
            SyntaxKind::TerminalFunction => Some(self.expect_function_with_body(attributes).into()),
            SyntaxKind::TerminalUse => Some(self.expect_use(attributes).into()),
            SyntaxKind::TerminalTrait => Some(self.expect_trait(attributes).into()),
            SyntaxKind::TerminalImpl => Some(self.expect_item_impl(attributes)),
            _ => {
                if has_attrs {
                    Some(self.create_and_report_missing::<Item>(
                        ParserDiagnosticKind::AttributesWithoutItem,
                    ))
                } else {
                    None
                }
            }
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
                    self.parse_attributed_list(
                        Self::try_parse_top_level_item,
                        is_of_kind!(rbrace),
                        TOP_LEVEL_ITEM_DESCRIPTION,
                    ),
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

    /// Assumes the current token is type.
    /// Expected pattern: `type <Identifier>{<ParamList>} = <TypeExpression>`
    fn expect_type_alias(&mut self, attributes: AttributeListGreen) -> ItemTypeAliasGreen {
        let type_kw = self.take::<TerminalType>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let eq = self.parse_token::<TerminalEq>();
        let ty = self.parse_type_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();
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

    /// Assumes the current token is [TerminalConst].
    /// Expected pattern: `const <Identifier> = <Expr>;`
    fn expect_const(&mut self, attributes: AttributeListGreen) -> ItemConstantGreen {
        let const_kw = self.take::<TerminalConst>();
        let name = self.parse_identifier();
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(eq, semicolon, top_level),
        });
        let eq = self.parse_token::<TerminalEq>();
        let expr = self.parse_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();

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
        match self.expect_extern_item_inner(attributes) {
            ExternItem::Function(x) => x.into(),
            ExternItem::Type(x) => x.into(),
        }
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionDeclaration>|type<Identifier>);`
    fn expect_extern_impl_item(&mut self, attributes: AttributeListGreen) -> ImplItemGreen {
        match self.expect_extern_item_inner(attributes) {
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
                let type_kw = self.parse_token::<TerminalType>();

                let name = self.parse_identifier();
                let generic_params = self.parse_optional_generic_params();
                let semicolon = self.parse_token::<TerminalSemicolon>();
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
        let use_kw = self.take::<TerminalUse>();
        let use_path = self.parse_use_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemUse::new_green(self.db, attributes, use_kw, use_path, semicolon)
    }

    /// Returns a GreenId of a node with a UsePath kind or None if can't parse a UsePath.
    fn try_parse_use_path(&mut self) -> Option<UsePathGreen> {
        if !matches!(self.peek().kind, SyntaxKind::TerminalLBrace | SyntaxKind::TerminalIdentifier)
        {
            return None;
        }
        Some(self.parse_use_path())
    }

    /// Returns a GreenId of a node with a UsePath kind.
    fn parse_use_path(&mut self) -> UsePathGreen {
        if self.peek().kind == SyntaxKind::TerminalLBrace {
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
            UsePathMulti::new_green(self.db, lbrace, items, rbrace).into()
        } else if let Some(ident) = self.try_parse_identifier() {
            let ident = PathSegmentSimple::new_green(self.db, ident).into();
            match self.peek().kind {
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
            }
        } else {
            let missing = self.skip_token_and_return_missing::<TerminalIdentifier>(
                ParserDiagnosticKind::MissingPathSegment,
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

    /// Returns a GreenId of a node with an identifier kind or None if an identifier can't be
    /// parsed.
    /// Note that if the terminal is a keyword or an underscore, it is skipped, and
    /// Some(missing-identifier) is returned.
    fn try_parse_identifier(&mut self) -> Option<TerminalIdentifierGreen> {
        if self.peek().kind.is_keyword_terminal() {
            // TODO(spapini): don't skip every keyword. Instead, pass a recovery set.
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

    /// Returns a GreenId of a node with an attribute list kind or None if an attribute list can't
    /// be parsed.
    /// `expected_elements_str` are the expected elements that these attributes are parsed for.
    /// Note: it should not include "attribute".
    fn try_parse_attribute_list(
        &mut self,
        expected_elements_str: &str,
        should_stop: fn(SyntaxKind) -> bool,
    ) -> Option<AttributeListGreen> {
        if self.peek().kind == SyntaxKind::TerminalHash {
            Some(self.parse_attribute_list(expected_elements_str, should_stop))
        } else {
            None
        }
    }

    /// Parses an attribute list.
    /// `expected_elements_str` are the expected elements that these attributes are parsed for.
    /// Note: it should not include "attribute".
    fn parse_attribute_list(
        &mut self,
        expected_elements_str: &str,
        should_stop: fn(SyntaxKind) -> bool,
    ) -> AttributeListGreen {
        AttributeList::new_green(
            self.db,
            self.parse_list(
                Self::try_parse_attribute,
                should_stop,
                format!("{expected_elements_str} or an attribute").as_str(),
            ),
        )
    }

    /// Returns a GreenId of a node with an attribute kind or None if an attribute can't be parsed.
    fn try_parse_attribute(&mut self) -> Option<AttributeGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalHash => {
                let hash = self.take::<TerminalHash>();
                let lbrack = self.parse_token::<TerminalLBrack>();
                let attr = self.parse_path();
                let arguments = self.try_parse_parenthesized_argument_list();
                let rbrack = self.parse_token::<TerminalRBrack>();

                Some(Attribute::new_green(self.db, hash, lbrack, attr, arguments, rbrack))
            }
            _ => None,
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration>`
    fn expect_function_declaration(&mut self) -> FunctionDeclarationGreen {
        let function_kw = self.take::<TerminalFunction>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let signature = self.expect_function_signature();

        FunctionDeclaration::new_green(self.db, function_kw, name, generic_params, signature)
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><Block>`
    fn expect_function_with_body(
        &mut self,
        attributes: AttributeListGreen,
    ) -> FunctionWithBodyGreen {
        let declaration = self.expect_function_declaration();
        let function_body = self.parse_block();
        FunctionWithBody::new_green(self.db, attributes, declaration, function_body)
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
                self.parse_attributed_list(
                    Self::try_parse_trait_item,
                    is_of_kind!(rbrace, top_level),
                    TRAIT_ITEM_DESCRIPTION,
                ),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            TraitBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        ItemTrait::new_green(self.db, attributes, trait_kw, name, generic_params, body)
    }

    /// Returns a GreenId of a node with a TraitItem.* kind (see
    /// [syntax::node::ast::TraitItem]), or none if a trait item can't be parsed.
    pub fn try_parse_trait_item(&mut self) -> Option<TraitItemGreen> {
        let maybe_attributes =
            self.try_parse_attribute_list(TRAIT_ITEM_DESCRIPTION, is_of_kind!(rbrace, top_level));

        let (has_attrs, attributes) = match maybe_attributes {
            Some(attributes) => (true, attributes),
            None => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalFunction => Some(self.expect_trait_function(attributes).into()),
            _ => {
                if has_attrs {
                    Some(self.create_and_report_missing::<TraitItem>(
                        ParserDiagnosticKind::AttributesWithoutTraitItem,
                    ))
                } else {
                    None
                }
            }
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><SemiColon>`
    fn expect_trait_function(&mut self, attributes: AttributeListGreen) -> TraitItemFunctionGreen {
        let declaration = self.expect_function_declaration();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.parse_block().into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };
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
        let impl_kw = self.take::<TerminalImpl>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();

        if self.peek().kind == SyntaxKind::TerminalEq {
            let eq = self.take::<TerminalEq>();
            let impl_path = self.parse_type_path();
            let semicolon = self.parse_token::<TerminalSemicolon>();

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
            ImplBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

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
    /// [syntax::node::ast::ImplItem]), or none if an impl item can't be parsed.
    pub fn try_parse_impl_item(&mut self) -> Option<ImplItemGreen> {
        let maybe_attributes =
            self.try_parse_attribute_list(IMPL_ITEM_DESCRIPTION, is_of_kind!(rbrace, top_level));

        let (has_attrs, attributes) = match maybe_attributes {
            Some(attributes) => (true, attributes),
            None => (false, AttributeList::new_green(self.db, vec![])),
        };

        match self.peek().kind {
            SyntaxKind::TerminalFunction => Some(self.expect_function_with_body(attributes).into()),
            // These are not supported semantically.
            SyntaxKind::TerminalConst => Some(self.expect_const(attributes).into()),
            SyntaxKind::TerminalModule => Some(self.expect_module(attributes).into()),
            SyntaxKind::TerminalStruct => Some(self.expect_struct(attributes).into()),
            SyntaxKind::TerminalEnum => Some(self.expect_enum(attributes).into()),
            SyntaxKind::TerminalType => Some(self.expect_type_alias(attributes).into()),
            SyntaxKind::TerminalExtern => Some(self.expect_extern_impl_item(attributes)),
            SyntaxKind::TerminalUse => Some(self.expect_use(attributes).into()),
            SyntaxKind::TerminalTrait => Some(self.expect_trait(attributes).into()),
            SyntaxKind::TerminalImpl => Some(self.expect_impl_item_impl(attributes)),
            _ => {
                if has_attrs {
                    Some(self.create_and_report_missing::<ImplItem>(
                        ParserDiagnosticKind::AttributesWithoutImplItem,
                    ))
                } else {
                    None
                }
            }
        }
    }

    // ------------------------------- Expressions -------------------------------

    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr])
    /// or None if an expression can't be parsed.
    fn try_parse_expr(&mut self) -> Option<ExprGreen> {
        self.try_parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Allow)
    }
    /// Returns a GreenId of a node with an Expr.* kind (see [syntax::node::ast::Expr])
    /// or a node with kind ExprMissing if an expression can't be parsed.
    pub fn parse_expr(&mut self) -> ExprGreen {
        match self.try_parse_expr() {
            Some(green) => green,
            None => self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression),
        }
    }

    /// Assumes the current token is an operator (binary or unary).
    /// Returns a GreenId of the operator or None if the operator is a unary-only operator.
    fn try_parse_binary_operator(&mut self) -> Option<BinaryOperatorGreen> {
        // Note that if this code is not reached you might need to add the operator to
        // `get_post_operator_precedence`.
        if matches!(self.peek().kind, SyntaxKind::TerminalNot | SyntaxKind::TerminalAt) {
            None
        } else {
            Some(match self.peek().kind {
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
            })
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
    /// or None if such an expression can't be parsed.
    ///
    /// Parsing will be limited by:
    /// `parent_precedence` - parsing of boolean operators limited to this.
    /// `lbrace_allowed` - See [LbraceAllowed].
    fn try_parse_expr_limited(
        &mut self,
        parent_precedence: usize,
        lbrace_allowed: LbraceAllowed,
    ) -> Option<ExprGreen> {
        let mut expr = if self.peek().kind == SyntaxKind::TerminalMinus {
            let minus = self.take::<TerminalMinus>();
            if self.peek().kind == SyntaxKind::TerminalNumber {
                LiteralNumber::new_green(self.db, minus.into(), self.take::<TerminalNumber>())
                    .into()
            } else {
                let expr = self.parse_expr_limited(
                    get_unary_operator_precedence(SyntaxKind::TerminalMinus).unwrap(),
                    lbrace_allowed,
                );
                ExprUnary::new_green(self.db, minus.into(), expr).into()
            }
        } else if let Some(precedence) = get_unary_operator_precedence(self.peek().kind) {
            let op = self.expect_unary_operator();
            let expr = self.parse_expr_limited(precedence, lbrace_allowed);
            ExprUnary::new_green(self.db, op, expr).into()
        } else {
            self.try_parse_atom(lbrace_allowed)?
        };

        while let Some(precedence) = get_post_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Some(expr);
            }
            if self.peek().kind == SyntaxKind::TerminalQuestionMark {
                expr = ExprErrorPropagate::new_green(
                    self.db,
                    expr,
                    self.take::<TerminalQuestionMark>(),
                )
                .into();
            } else if self.peek().kind == SyntaxKind::TerminalLBrack {
                let lbrack = self.take::<TerminalLBrack>();
                let index_expr = self.parse_expr();
                let rbrack = self.parse_token::<TerminalRBrack>();
                expr = ExprIndexed::new_green(self.db, expr, lbrack, index_expr, rbrack).into();
            } else if let Some(op) = self.try_parse_binary_operator() {
                let rhs = self.parse_expr_limited(precedence, lbrace_allowed);
                expr = ExprBinary::new_green(self.db, expr, op, rhs).into();
            } else {
                return Some(expr);
            }
        }
        Some(expr)
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
                    SyntaxKind::TerminalNot => Some(self.expect_macro_call(path).into()),
                    _ => Some(path.into()),
                }
            }
            SyntaxKind::TerminalFalse => Some(self.take::<TerminalFalse>().into()),
            SyntaxKind::TerminalTrue => Some(self.take::<TerminalTrue>().into()),
            SyntaxKind::TerminalNumber => Some(
                LiteralNumber::new_green(
                    self.db,
                    OptionTerminalMinusEmpty::new_green(self.db).into(),
                    self.take::<TerminalNumber>(),
                )
                .into(),
            ),
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
            SyntaxKind::TerminalLoop if lbrace_allowed == LbraceAllowed::Allow => {
                Some(self.expect_loop_expr().into())
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
            SyntaxKind::TerminalAt => {
                let op = self.take::<TerminalAt>().into();
                let expr = self.try_parse_type_expr().unwrap_or_else(|| {
                    self.create_and_report_missing::<Expr>(
                        ParserDiagnosticKind::MissingTypeExpression,
                    )
                });
                Some(ExprUnary::new_green(self.db, op, expr).into())
            }
            SyntaxKind::TerminalIdentifier => Some(self.parse_type_path().into()),
            SyntaxKind::TerminalLParen => Some(self.expect_type_tuple_expr()),
            _ => {
                // TODO(yuval): report to diagnostics.
                None
            }
        }
    }

    /// Returns a GreenId of a node with an ExprPath|ExprParenthesized|ExprTuple kind, or
    /// ExprMissing if such an expression can't be parsed.
    fn parse_type_expr(&mut self) -> ExprGreen {
        self.try_parse_type_expr().unwrap_or_else(|| {
            self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingTypeExpression)
        })
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
    /// Expected pattern: `<ArgListParenthesized>`
    fn expect_function_call(&mut self, path: ExprPathGreen) -> ExprFunctionCallGreen {
        let func_name = path;
        let parenthesized_args = self.expect_parenthesized_argument_list();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `<ArgListParenthesized>`
    fn expect_macro_call(&mut self, path: ExprPathGreen) -> ExprInlineMacroGreen {
        let bang = self.take::<TerminalNot>();
        let macro_name = path;
        let parenthesized_args = self.expect_parenthesized_argument_list();
        ExprInlineMacro::new_green(self.db, macro_name, bang, parenthesized_args)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\(<ArgList>\)`
    fn expect_parenthesized_argument_list(&mut self) -> ArgListParenthesizedGreen {
        let lparen = self.take::<TerminalLParen>();
        let arg_list = ArgList::new_green(
            self.db,
            self.parse_separated_list::<Arg, TerminalComma, ArgListElementOrSeparatorGreen>(
                Self::try_parse_function_argument,
                is_of_kind!(rparen, block, rbrace, top_level),
                "argument",
            ),
        );
        let rparen = self.parse_token::<TerminalRParen>();
        ArgListParenthesized::new_green(self.db, lparen, arg_list, rparen)
    }

    /// Tries to parse parenthesized function call arguments.
    /// Expected pattern: `\(<ArgList>\)`
    fn try_parse_parenthesized_argument_list(&mut self) -> OptionArgListParenthesizedGreen {
        let Some(lparen) = self.try_parse_token::<TerminalLParen>() else {
            return OptionArgListParenthesizedEmpty::new_green(self.db).into();
        };
        let arg_list = ArgList::new_green(
            self.db,
            self.parse_separated_list::<Arg, TerminalComma, ArgListElementOrSeparatorGreen>(
                Self::try_parse_function_argument,
                is_of_kind!(rparen, block, rbrace, top_level),
                "argument",
            ),
        );
        let rparen = self.parse_token::<TerminalRParen>();
        ArgListParenthesized::new_green(self.db, lparen, arg_list, rparen).into()
    }

    /// Parses a function call's argument, which contains possibly modifiers, and a argument clause.
    fn try_parse_function_argument(&mut self) -> Option<ArgGreen> {
        let modifiers_list = self.parse_modifier_list();
        let arg_clause = self.try_parse_argument_clause();
        if !modifiers_list.is_empty() && arg_clause.is_none() {
            let modifiers = ModifierList::new_green(self.db, modifiers_list);
            let arg_clause = ArgClauseUnnamed::new_green(self.db, self.parse_expr()).into();
            return Some(Arg::new_green(self.db, modifiers, arg_clause));
        }
        let modifiers = ModifierList::new_green(self.db, modifiers_list);
        Some(Arg::new_green(self.db, modifiers, arg_clause?))
    }

    /// Parses a function call's argument, which is an expression with or without the name
    /// of the argument.
    ///
    /// Possible patterns:
    /// * `<Expr>` (unnamed).
    /// * `<Identifier>: <Expr>` (named).
    /// * `:<Identifier>` (Field init shorthand - syntactic sugar for `a: a`).
    fn try_parse_argument_clause(&mut self) -> Option<ArgClauseGreen> {
        if self.peek().kind == SyntaxKind::TerminalColon {
            let colon = self.take::<TerminalColon>();
            let argname = self.parse_identifier();
            return Some(
                ArgClauseFieldInitShorthand::new_green(
                    self.db,
                    colon,
                    ExprFieldInitShorthand::new_green(self.db, argname),
                )
                .into(),
            );
        }

        // Read an expression.
        let expr_or_argname = self.try_parse_expr()?;

        // If the next token is `:` and the expression is an identifier, this is the argument's
        // name.
        if self.peek().kind == SyntaxKind::TerminalColon {
            if let Some(argname) = self.try_extract_identifier(expr_or_argname) {
                let colon = self.take::<TerminalColon>();
                let expr = self.parse_expr();
                return Some(ArgClauseNamed::new_green(self.db, argname, colon, expr).into());
            }
        }

        Some(ArgClauseUnnamed::new_green(self.db, expr_or_argname).into())
    }

    /// If the given `expr` is a simple identifier, returns the corresponding green node.
    /// Otherwise, returns `None`.
    fn try_extract_identifier(&self, expr: ExprGreen) -> Option<TerminalIdentifierGreen> {
        // Check that `expr` is `ExprPath`.
        let GreenNode {
            kind: SyntaxKind::ExprPath,
            details: GreenNodeDetails::Node { children: children0, .. },
        } = &self.db.lookup_intern_green(expr.0) else {return None;};

        // Check that it has one child.
        let [path_segment] = children0[..] else {return None;};

        // Check that `path_segment` is `PathSegmentSimple`.
        let GreenNode {
            kind: SyntaxKind::PathSegmentSimple,
            details: GreenNodeDetails::Node { children: children1, .. },
        } = self.db.lookup_intern_green(path_segment) else {return None;};

        // Check that it has one child.
        let [ident] = children1[..] else {return None;};

        // Check that it is indeed `TerminalIdentifier`.
        let GreenNode {
            kind: SyntaxKind::TerminalIdentifier,
            ..
        } = self.db.lookup_intern_green(ident) else {return None;};

        Some(TerminalIdentifierGreen(ident))
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

    /// Assumes the current token is LParen.
    /// Expected pattern: `\((<type_expr>,)*<type_expr>?\)`
    /// Returns a GreenId of a node with kind ExprTuple.
    fn expect_type_tuple_expr(&mut self) -> ExprGreen {
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_type_expr,
                is_of_kind!(rparen, block, rbrace, top_level),
                "type expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();
        if let [ExprListElementOrSeparatorGreen::Element(_)] = &exprs[..] {
            self.diagnostics.add(ParserDiagnostic {
                file_id: self.file_id,
                kind: ParserDiagnosticKind::MissingToken(SyntaxKind::TokenComma),
                span: TextSpan { start: self.offset, end: self.offset },
            });
        }
        ExprTuple::new_green(self.db, lparen, ExprList::new_green(self.db, exprs), rparen).into()
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
    fn try_parse_argument_single(&mut self) -> Option<StructArgSingleGreen> {
        let identifier = self.try_parse_identifier()?;
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        Some(StructArgSingle::new_green(self.db, identifier, struct_arg_expr))
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let skipped_tokens = self.skip_until(is_of_kind!(lbrace, top_level, block));

        if let Err(SkippedError(span)) = skipped_tokens {
            self.diagnostics.add(ParserDiagnostic {
                file_id: self.file_id,
                kind: ParserDiagnosticKind::SkippedElement { element_name: "'{'".into() },
                span,
            });
        }

        // Don't report diagnostic if one has already been reported.
        let lbrace = self.parse_token_ex::<TerminalLBrace>(skipped_tokens.is_ok());
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

    /// Assumes the current token is `Loop`.
    /// Expected pattern: `loop <block>`.
    fn expect_loop_expr(&mut self) -> ExprLoopGreen {
        let loop_kw = self.take::<TerminalLoop>();
        let body = self.parse_block();

        ExprLoop::new_green(self.db, loop_kw, body)
    }

    /// Returns a GreenId of a node with a MatchArm kind or None if a match arm can't be parsed.
    pub fn try_parse_match_arm(&mut self) -> Option<MatchArmGreen> {
        let pattern = self.try_parse_pattern()?;
        let arrow = self.parse_token::<TerminalMatchArrow>();
        let expr = self.parse_expr();
        Some(MatchArm::new_green(self.db, pattern, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see
    /// [syntax::node::ast::Pattern]) or None if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> Option<PatternGreen> {
        let modifier_list = self.parse_modifier_list();
        if !modifier_list.is_empty() {
            let modifiers = ModifierList::new_green(self.db, modifier_list);
            let name = self.parse_identifier();
            return Some(PatternIdentifier::new_green(self.db, modifiers, name).into());
        };

        // TODO(yuval): Support "Or" patterns.
        Some(match self.peek().kind {
            SyntaxKind::TerminalNumber => LiteralNumber::new_green(
                self.db,
                OptionTerminalMinusEmpty::new_green(self.db).into(),
                self.take::<TerminalNumber>(),
            )
            .into(),
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
                        let rbrace = self.parse_token::<TerminalRBrace>();
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
    /// Returns a GreenId of a node with some Pattern kind (see
    /// [syntax::node::ast::Pattern]).
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
                let modifier_list = self.parse_modifier_list();
                let name = if modifier_list.is_empty() {
                    self.try_parse_identifier()?
                } else {
                    self.parse_identifier()
                };
                let modifiers = ModifierList::new_green(self.db, modifier_list);
                if self.peek().kind == SyntaxKind::TerminalColon {
                    let colon = self.take::<TerminalColon>();
                    let pattern = self.parse_pattern();
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
    /// [syntax::node::ast::Statement]) or None if a statement can't be parsed.
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
            SyntaxKind::TerminalContinue => {
                let continue_kw = self.take::<TerminalContinue>();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(StatementContinue::new_green(self.db, continue_kw, semicolon).into())
            }
            SyntaxKind::TerminalReturn => {
                let return_kw = self.take::<TerminalReturn>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(StatementReturn::new_green(self.db, return_kw, expr, semicolon).into())
            }
            SyntaxKind::TerminalBreak => {
                let break_kw = self.take::<TerminalBreak>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Some(StatementBreak::new_green(self.db, break_kw, expr, semicolon).into())
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

    /// Parses a type clause of the form: `: <type>`.
    fn parse_type_clause(&mut self, error_recovery: ErrorRecovery) -> TypeClauseGreen {
        match self.try_parse_type_clause() {
            Some(green) => green,
            None => {
                let res = self.create_and_report_missing::<TypeClause>(
                    ParserDiagnosticKind::MissingTypeClause,
                );
                self.skip_until(error_recovery.should_stop).ok();
                res
            }
        }
    }
    fn try_parse_type_clause(&mut self) -> Option<TypeClauseGreen> {
        if self.peek().kind == SyntaxKind::TerminalColon {
            let colon = self.take::<TerminalColon>();
            let ty = self.parse_type_expr();
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
            let return_type = self.parse_type_expr();
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
            self.try_parse_identifier()?
        } else {
            // If we had modifiers then the identifier is not optional and can't be '_'.
            self.parse_identifier()
        };

        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(comma, rparen, top_level),
        });
        Some(Param::new_green(
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
                "member or variant",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Member or None if a struct member/enum variant can't
    /// be parsed.
    fn try_parse_member(&mut self) -> Option<MemberGreen> {
        let attributes =
            self.try_parse_attribute_list("Struct member", |x| x != SyntaxKind::TerminalHash);
        let name = if attributes.is_some() {
            self.parse_identifier()
        } else {
            self.try_parse_identifier()?
        };
        let attributes = attributes.unwrap_or_else(|| AttributeList::new_green(self.db, vec![]));

        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(comma, rbrace, top_level),
        });
        Some(Member::new_green(self.db, attributes, name, type_clause))
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

    /// Expected pattern: `(<PathSegment>::)*<PathSegment>(::){0,1}<GenericArgs>`.
    ///
    /// Returns a GreenId of a node with kind ExprPath.
    fn parse_type_path(&mut self) -> ExprPathGreen {
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

        ExprPath::new_green(self.db, children)
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
                    separator.into(),
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

    /// Returns a Typed PathSegment or a normal PathSegment.
    /// Additionally returns an optional separators.
    fn parse_type_path_segment(&mut self) -> (PathSegmentGreen, Option<TerminalColonColonGreen>) {
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
            None if self.peek().kind == SyntaxKind::TerminalLT => (
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
            Some(separator) if self.peek().kind == SyntaxKind::TerminalLT => (
                PathSegmentWithGenericArgs::new_green(
                    self.db,
                    identifier,
                    separator.into(),
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

    /// Returns a GreenId of a node with an
    /// ExprLiteral|ExprPath|ExprParenthesized|ExprTuple|ExprUnderscore kind, or None if such an
    /// expression can't be parsed.
    fn try_parse_generic_arg(&mut self) -> Option<GenericArgGreen> {
        if self.peek().kind == SyntaxKind::TerminalUnderscore {
            return Some(self.take::<TerminalUnderscore>().into());
        }

        let expr = match self.peek().kind {
            SyntaxKind::TerminalNumber => LiteralNumber::new_green(
                self.db,
                OptionTerminalMinusEmpty::new_green(self.db).into(),
                self.take::<TerminalNumber>(),
            )
            .into(),
            SyntaxKind::TerminalMinus => LiteralNumber::new_green(
                self.db,
                self.take::<TerminalMinus>().into(),
                self.take::<TerminalNumber>(),
            )
            .into(),
            SyntaxKind::TerminalShortString => self.take::<TerminalShortString>().into(),
            SyntaxKind::TerminalLBrace => self.parse_block().into(),
            _ => self.try_parse_type_expr()?,
        };

        Some(GenericArgExpr::new_green(self.db, expr).into())
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericArgList> \>`
    fn expect_generic_args(&mut self) -> GenericArgsGreen {
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
        match self.peek().kind {
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let ty = self.parse_type_expr();
                Some(GenericParamConst::new_green(self.db, const_kw, name, colon, ty).into())
            }
            SyntaxKind::TerminalImpl => {
                let impl_kw = self.take::<TerminalImpl>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let trait_path = self.parse_type_path();
                Some(GenericParamImpl::new_green(self.db, impl_kw, name, colon, trait_path).into())
            }
            _ => Some(GenericParamType::new_green(self.db, self.try_parse_identifier()?).into()),
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
        try_parse_list_element: fn(&mut Self) -> Option<ElementGreen>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &str,
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
                    element_name: expected_element.into(),
                });
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
        try_parse_list_element: fn(&mut Self) -> Option<ElementGreen>,
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
                        element_name: expected_element.into(),
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
        self.parse_token_ex::<Terminal>(true)
    }

    /// Same as [Self::parse_token], except that the diagnostic may be omitted.
    fn parse_token_ex<Terminal: syntax::node::Terminal>(
        &mut self,
        report_diagnostic: bool,
    ) -> Terminal::Green {
        match self.try_parse_token::<Terminal>() {
            Some(green) => green,
            None => {
                if report_diagnostic {
                    self.create_and_report_missing_terminal::<Terminal>()
                } else {
                    Terminal::missing(self.db)
                }
            }
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
