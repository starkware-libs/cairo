use std::mem;

use cairo_lang_diagnostics::DiagnosticsBuilder;
use cairo_lang_filesystem::ids::FileId;
use cairo_lang_filesystem::span::{TextOffset, TextSpan, TextWidth};
use cairo_lang_primitive_token::{PrimitiveToken, ToPrimitiveTokenStream};
use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::ast::*;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, Token, TypedSyntaxNode};
use cairo_lang_utils::{LookupIntern, extract_matches, require};
use syntax::node::green::{GreenNode, GreenNodeDetails};
use syntax::node::ids::GreenId;

use crate::ParserDiagnostic;
use crate::diagnostic::ParserDiagnosticKind;
use crate::lexer::{Lexer, LexerTerminal};
use crate::operators::{get_post_operator_precedence, get_unary_operator_precedence};
use crate::recovery::is_of_kind;
use crate::utils::primitive_token_stream_content_and_offset;
use crate::validation::{validate_literal_number, validate_short_string, validate_string};

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
    /// The width of the current terminal being handled (excluding the trivia length of this
    /// terminal).
    current_width: TextWidth,
    /// The length of the trailing trivia following the last read token.
    last_trivia_length: TextWidth,
    diagnostics: &'a mut DiagnosticsBuilder<ParserDiagnostic>,
    /// An accumulating vector of pending skipped tokens diagnostics.
    pending_skipped_token_diagnostics: Vec<PendingParserDiagnostic>,
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

// ====================================== Naming of items ======================================
// To avoid confusion, there is a naming convention for the language items.
// An item is called <item_scope>Item<item_kind>, where item_scope is in {Module, Trait, Impl}, and
// item_kind is in {Const, Enum, ExternFunction, ExternType, Function, Impl, InlineMacro, Module,
// Struct, Trait, Type, TypeAlias, Use} (note not all combinations are supported).
// For example, ModuleItemFunction is a function item in a module, TraitItemConst is a const item in
// a trait.

// ================================ Naming of parsing functions ================================
// try_parse_<something>: returns a TryParseElementResult. A `Ok` with green ID with a kind
// that represents 'something' or a `Err` if 'something' can't be parsed.
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
const MODULE_ITEM_DESCRIPTION: &str = "Const/Enum/ExternFunction/ExternType/Function/Impl/\
                                       InlineMacro/Module/Struct/Trait/TypeAlias/Use";
const TRAIT_ITEM_DESCRIPTION: &str = "Const/Function/Impl/Type";
const IMPL_ITEM_DESCRIPTION: &str = "Const/Function/Impl/Type";

// A macro adding "or an attribute" to the end of a string.
macro_rules! or_an_attribute {
    ($string:expr) => {
        format!("{} or an attribute", $string)
    };
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    fn new(
        db: &'a dyn SyntaxGroup,
        file_id: FileId,
        text: &'a str,
        diagnostics: &'a mut DiagnosticsBuilder<ParserDiagnostic>,
    ) -> Self {
        let mut lexer = Lexer::from_text(db, text);
        let next_terminal = lexer.next().unwrap();
        Parser {
            db,
            file_id,
            lexer,
            next_terminal,
            pending_trivia: Vec::new(),
            offset: Default::default(),
            current_width: Default::default(),
            last_trivia_length: Default::default(),
            diagnostics,
            pending_skipped_token_diagnostics: Vec::new(),
        }
    }

    /// Adds a diagnostic to the parser diagnostics collection.
    fn add_diagnostic(&mut self, kind: ParserDiagnosticKind, span: TextSpan) {
        self.diagnostics.add(ParserDiagnostic { file_id: self.file_id, kind, span });
    }

    /// Parses a file.
    pub fn parse_file(
        db: &'a dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
        file_id: FileId,
        text: &'a str,
    ) -> SyntaxFile {
        let parser = Parser::new(db, file_id, text, diagnostics);
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
        let mut parser = Parser::new(db, file_id, text, diagnostics);
        let green = parser.parse_expr();
        if let Err(SkippedError(span)) = parser.skip_until(is_of_kind!()) {
            parser.add_diagnostic(
                ParserDiagnosticKind::SkippedElement { element_name: "end of expr".into() },
                span,
            );
        }
        Expr::from_syntax_node(db, SyntaxNode::new_root(db, file_id, green.0))
    }

    /// Parses a token stream.
    pub fn parse_token_stream(
        db: &'a dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
        file_id: FileId,
        token_stream: &'a dyn ToPrimitiveTokenStream<Iter = impl Iterator<Item = PrimitiveToken>>,
    ) -> SyntaxFile {
        let (content, offset) = primitive_token_stream_content_and_offset(token_stream);
        let parser = Parser::new(db, file_id, &content, diagnostics);
        let green = parser.parse_syntax_file();
        SyntaxFile::from_syntax_node(
            db,
            SyntaxNode::new_root_with_offset(db, file_id, green.0, offset),
        )
    }

    /// Parses a token stream expression.
    pub fn parse_token_stream_expr(
        db: &'a dyn SyntaxGroup,
        diagnostics: &mut DiagnosticsBuilder<ParserDiagnostic>,
        file_id: FileId,
        token_stream: &'a dyn ToPrimitiveTokenStream<Iter = impl Iterator<Item = PrimitiveToken>>,
    ) -> Expr {
        let (content, offset) = primitive_token_stream_content_and_offset(token_stream);
        let mut parser = Parser::new(db, file_id, &content, diagnostics);
        let green = parser.parse_expr();
        if let Err(SkippedError(span)) = parser.skip_until(is_of_kind!()) {
            parser.diagnostics.add(ParserDiagnostic {
                file_id: parser.file_id,
                kind: ParserDiagnosticKind::SkippedElement { element_name: "end of expr".into() },
                span,
            });
        }
        Expr::from_syntax_node(db, SyntaxNode::new_root_with_offset(db, file_id, green.0, offset))
    }

    /// Returns a GreenId of an ExprMissing and adds a diagnostic describing it.
    fn create_and_report_missing<T: TypedSyntaxNode>(
        &mut self,
        missing_kind: ParserDiagnosticKind,
    ) -> T::Green {
        let next_offset = self.offset.add_width(self.current_width - self.last_trivia_length);
        self.add_diagnostic(missing_kind, TextSpan { start: next_offset, end: next_offset });
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
        let mut module_items = vec![];
        if let Some(doc_item) = self.take_doc() {
            module_items.push(doc_item.into());
        }
        module_items.extend(self.parse_attributed_list(
            Self::try_parse_module_item,
            is_of_kind!(),
            MODULE_ITEM_DESCRIPTION,
        ));
        // Create a new vec with the doc item as the children.
        let items = ModuleItemList::new_green(self.db, module_items);
        // This will not panic since the above parsing only stops when reaches EOF.
        assert_eq!(self.peek().kind, SyntaxKind::TerminalEndOfFile);

        // Fix offset in case there are skipped tokens before EOF. This is usually done in
        // self.take_raw() but here we don't call self.take_raw as it tries to read the next
        // token, which doesn't exist.
        self.offset = self.offset.add_width(self.current_width);

        let eof = self.add_trivia_to_terminal::<TerminalEndOfFile>(self.next_terminal.clone());
        SyntaxFile::new_green(self.db, items, eof)
    }

    // ------------------------------- Module items -------------------------------

    /// Returns a GreenId of a node with an Item.* kind (see [syntax::node::ast::ModuleItem]), or
    /// TryParseFailure if a module item can't be parsed.
    /// In case of an identifier not followed by a `!`, it is skipped inside the function and thus a
    /// TryParseFailure::DoNothing is returned.
    pub fn try_parse_module_item(&mut self) -> TryParseResult<ModuleItemGreen> {
        let maybe_attributes = self.try_parse_attribute_list(MODULE_ITEM_DESCRIPTION);
        let (has_attrs, attributes) = match maybe_attributes {
            Ok(attributes) => (true, attributes),
            Err(_) => (false, AttributeList::new_green(self.db, vec![])),
        };
        let post_attributes_offset = self.offset.add_width(self.current_width);

        let visibility_pub = self.try_parse_visibility_pub();
        let visibility = match visibility_pub {
            Some(visibility) => visibility.into(),
            None => VisibilityDefault::new_green(self.db).into(),
        };
        let post_visibility_offset = self.offset.add_width(self.current_width);

        match self.peek().kind {
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                Ok(if self.peek().kind == SyntaxKind::TerminalFunction {
                    self.expect_item_function_with_body(attributes, visibility, const_kw.into())
                        .into()
                } else {
                    self.expect_item_const(attributes, visibility, const_kw).into()
                })
            }
            SyntaxKind::TerminalModule => {
                Ok(self.expect_item_module(attributes, visibility).into())
            }
            SyntaxKind::TerminalStruct => {
                Ok(self.expect_item_struct(attributes, visibility).into())
            }
            SyntaxKind::TerminalEnum => Ok(self.expect_item_enum(attributes, visibility).into()),
            SyntaxKind::TerminalType => {
                Ok(self.expect_item_type_alias(attributes, visibility).into())
            }
            SyntaxKind::TerminalExtern => Ok(self.expect_item_extern(attributes, visibility)),
            SyntaxKind::TerminalFunction => Ok(self
                .expect_item_function_with_body(
                    attributes,
                    visibility,
                    OptionTerminalConstEmpty::new_green(self.db).into(),
                )
                .into()),
            SyntaxKind::TerminalUse => Ok(self.expect_item_use(attributes, visibility).into()),
            SyntaxKind::TerminalTrait => Ok(self.expect_item_trait(attributes, visibility).into()),
            SyntaxKind::TerminalImpl => Ok(self.expect_module_item_impl(attributes, visibility)),
            SyntaxKind::TerminalIdentifier => {
                // We take the identifier to check if the next token is a `!`. If it is, we assume
                // that a macro is following and handle it similarly to any other module item. If
                // not we skip the identifier. 'take_raw' is used here since it is not yet known if
                // the identifier would be taken as a part of a macro, or skipped.
                //
                // TODO(Gil): Consider adding a lookahead capability to the lexer to avoid this.
                let ident = self.take_raw();
                match self.peek().kind {
                    SyntaxKind::TerminalNot => {
                        // Complete the `take`ing of the identifier.
                        let macro_name = self.add_trivia_to_terminal::<TerminalIdentifier>(ident);
                        Ok(self.expect_item_inline_macro(attributes, macro_name).into())
                    }
                    SyntaxKind::TerminalLParen
                    | SyntaxKind::TerminalLBrace
                    | SyntaxKind::TerminalLBrack => {
                        // This case is treated as an item inline macro with a missing bang ('!').
                        self.add_diagnostic(
                            ParserDiagnosticKind::ItemInlineMacroWithoutBang {
                                identifier: ident.clone().text,
                                bracket_type: self.peek().kind,
                            },
                            TextSpan {
                                start: self.offset,
                                end: self.offset.add_width(self.current_width),
                            },
                        );
                        let macro_name = self.add_trivia_to_terminal::<TerminalIdentifier>(ident);
                        Ok(self
                            .parse_item_inline_macro_given_bang(
                                attributes,
                                macro_name,
                                TerminalNot::missing(self.db),
                            )
                            .into())
                    }
                    _ => {
                        if has_attrs {
                            self.skip_taken_node_with_offset(
                                attributes,
                                ParserDiagnosticKind::SkippedElement {
                                    element_name: or_an_attribute!(MODULE_ITEM_DESCRIPTION).into(),
                                },
                                post_attributes_offset,
                            );
                        }
                        if let Some(visibility_pub) = visibility_pub {
                            self.skip_taken_node_with_offset(
                                visibility_pub,
                                ParserDiagnosticKind::SkippedElement {
                                    element_name: or_an_attribute!(MODULE_ITEM_DESCRIPTION).into(),
                                },
                                post_visibility_offset,
                            );
                        }
                        // Complete the `skip`ping of the identifier.
                        self.append_skipped_token_to_pending_trivia(
                            ident,
                            ParserDiagnosticKind::SkippedElement {
                                element_name: or_an_attribute!(MODULE_ITEM_DESCRIPTION).into(),
                            },
                        );
                        // The token is already skipped, so it should not be skipped in the caller.
                        Err(TryParseFailure::DoNothing)
                    }
                }
            }
            _ => {
                let mut result = Err(TryParseFailure::SkipToken);
                if has_attrs {
                    self.skip_taken_node_with_offset(
                        attributes,
                        ParserDiagnosticKind::AttributesWithoutItem,
                        post_attributes_offset,
                    );
                    result = Ok(ModuleItem::missing(self.db));
                }
                if let Some(visibility_pub) = visibility_pub {
                    self.skip_taken_node_with_offset(
                        visibility_pub,
                        ParserDiagnosticKind::VisibilityWithoutItem,
                        post_visibility_offset,
                    );
                    result = Ok(ModuleItem::missing(self.db));
                }
                result
            }
        }
    }

    /// Assumes the current token is Module.
    /// Expected pattern: `mod <Identifier> \{<ItemList>\}` or `mod <Identifier>;`.
    fn expect_item_module(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemModuleGreen {
        let module_kw = self.take::<TerminalModule>();
        let name = self.parse_identifier();

        let body = match self.peek().kind {
            SyntaxKind::TerminalLBrace => {
                let lbrace = self.take::<TerminalLBrace>();
                let mut module_items = vec![];
                if let Some(doc_item) = self.take_doc() {
                    module_items.push(doc_item.into());
                }
                module_items.extend(self.parse_attributed_list(
                    Self::try_parse_module_item,
                    is_of_kind!(rbrace),
                    MODULE_ITEM_DESCRIPTION,
                ));
                let items = ModuleItemList::new_green(self.db, module_items);
                let rbrace = self.parse_token::<TerminalRBrace>();
                ModuleBody::new_green(self.db, lbrace, items, rbrace).into()
            }
            // TODO: Improve diagnostic to indicate semicolon or a body were expected.
            _ => self.parse_token::<TerminalSemicolon>().into(),
        };

        ItemModule::new_green(self.db, attributes, visibility, module_kw, name, body)
    }

    /// Assumes the current token is Struct.
    /// Expected pattern: `struct<Identifier>{<ParamList>}`
    fn expect_item_struct(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemStructGreen {
        let struct_kw = self.take::<TerminalStruct>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let members = self.parse_member_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        ItemStruct::new_green(
            self.db,
            attributes,
            visibility,
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
    fn expect_item_enum(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemEnumGreen {
        let enum_kw = self.take::<TerminalEnum>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let lbrace = self.parse_token::<TerminalLBrace>();
        let variants = self.parse_variant_list();
        let rbrace = self.parse_token::<TerminalRBrace>();
        ItemEnum::new_green(
            self.db,
            attributes,
            visibility,
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
    fn expect_item_type_alias(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemTypeAliasGreen {
        let type_kw = self.take::<TerminalType>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let eq = self.parse_token::<TerminalEq>();
        let ty = self.parse_type_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemTypeAlias::new_green(
            self.db,
            attributes,
            visibility,
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
    fn expect_item_const(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
        const_kw: TerminalConstGreen,
    ) -> ItemConstantGreen {
        let name = self.parse_identifier();
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(eq, semicolon, module_item_kw),
        });
        let eq = self.parse_token::<TerminalEq>();
        let expr = self.parse_expr();
        let semicolon = self.parse_token::<TerminalSemicolon>();

        ItemConstant::new_green(
            self.db,
            attributes,
            visibility,
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
    fn expect_item_extern<T: From<ItemExternFunctionGreen> + From<ItemExternTypeGreen>>(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> T {
        match self.expect_item_extern_inner(attributes, visibility) {
            ExternItem::Function(x) => x.into(),
            ExternItem::Type(x) => x.into(),
        }
    }

    /// Assumes the current token is Extern.
    /// Expected pattern: `extern(<FunctionDeclaration>|type<Identifier>);`
    fn expect_item_extern_inner(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ExternItem {
        let extern_kw = self.take::<TerminalExtern>();
        match self.peek().kind {
            SyntaxKind::TerminalFunction | SyntaxKind::TerminalConst => {
                let (optional_const, function_kw) = if self.peek().kind == SyntaxKind::TerminalConst
                {
                    (self.take::<TerminalConst>().into(), self.parse_token::<TerminalFunction>())
                } else {
                    (
                        OptionTerminalConstEmpty::new_green(self.db).into(),
                        self.take::<TerminalFunction>(),
                    )
                };
                let declaration = self.expect_function_declaration_ex(optional_const, function_kw);
                let semicolon = self.parse_token::<TerminalSemicolon>();
                ExternItem::Function(ItemExternFunction::new_green(
                    self.db,
                    attributes,
                    visibility,
                    extern_kw,
                    declaration,
                    semicolon,
                ))
            }
            _ => {
                // TODO(spapini): Don't return ItemExternType if we don't see a type.
                let type_kw = self.parse_token::<TerminalType>();

                let name = self.parse_identifier();
                let generic_params = self.parse_optional_generic_params();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                // If the next token is not type, assume it is missing.
                ExternItem::Type(ItemExternType::new_green(
                    self.db,
                    attributes,
                    visibility,
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
    fn expect_item_use(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemUseGreen {
        let use_kw = self.take::<TerminalUse>();
        let use_path = self.parse_use_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemUse::new_green(self.db, attributes, visibility, use_kw, use_path, semicolon)
    }

    /// Returns a GreenId of a node with a UsePath kind or TryParseFailure if can't parse a UsePath.
    fn try_parse_use_path(&mut self) -> TryParseResult<UsePathGreen> {
        if !matches!(
            self.peek().kind,
            SyntaxKind::TerminalLBrace | SyntaxKind::TerminalIdentifier | SyntaxKind::TerminalMul
        ) {
            return Err(TryParseFailure::SkipToken);
        }
        Ok(self.parse_use_path())
    }

    /// Returns a GreenId of a node with a UsePath kind.
    fn parse_use_path(&mut self) -> UsePathGreen {
        match self.peek().kind {
            SyntaxKind::TerminalLBrace => {
                let lbrace = self.parse_token::<TerminalLBrace>();
                let items = UsePathList::new_green(self.db,
                    self.parse_separated_list::<
                        UsePath, TerminalComma, UsePathListElementOrSeparatorGreen
                    >(
                        Self::try_parse_use_path,
                        is_of_kind!(rbrace, module_item_kw),
                        "path segment",
                    ));
                let rbrace = self.parse_token::<TerminalRBrace>();
                UsePathMulti::new_green(self.db, lbrace, items, rbrace).into()
            }
            SyntaxKind::TerminalMul => {
                let star = self.parse_token::<TerminalMul>();
                UsePathStar::new_green(self.db, star).into()
            }
            _ => {
                if let Ok(ident) = self.try_parse_identifier() {
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
                    let missing = self.create_and_report_missing::<TerminalIdentifier>(
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

    /// Returns a GreenId of node visibility.
    fn parse_visibility(&mut self) -> VisibilityGreen {
        match self.try_parse_visibility_pub() {
            Some(visibility) => visibility.into(),
            None => VisibilityDefault::new_green(self.db).into(),
        }
    }

    /// Returns a GreenId of node with pub visibility or None if not starting with "pub".
    fn try_parse_visibility_pub(&mut self) -> Option<VisibilityPubGreen> {
        require(self.peek().kind == SyntaxKind::TerminalPub)?;
        let pub_kw = self.take::<TerminalPub>();
        let argument_clause = if self.peek().kind != SyntaxKind::TerminalLParen {
            OptionVisibilityPubArgumentClauseEmpty::new_green(self.db).into()
        } else {
            let lparen = self.parse_token::<TerminalLParen>();
            let argument = self.parse_token::<TerminalIdentifier>();
            let rparen = self.parse_token::<TerminalRParen>();
            VisibilityPubArgumentClause::new_green(self.db, lparen, argument, rparen).into()
        };
        Some(VisibilityPub::new_green(self.db, pub_kw, argument_clause))
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
        AttributeList::new_green(
            self.db,
            self.parse_list(
                Self::try_parse_attribute,
                |x| x != SyntaxKind::TerminalHash,
                &or_an_attribute!(expected_elements_str),
            ),
        )
    }

    /// Returns a GreenId of a node with an attribute kind or TryParseFailure if an attribute can't
    /// be parsed.
    fn try_parse_attribute(&mut self) -> TryParseResult<AttributeGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalHash => {
                let hash = self.take::<TerminalHash>();
                let lbrack = self.parse_token::<TerminalLBrack>();
                let attr = self.parse_path();
                let arguments = self.try_parse_parenthesized_argument_list();
                let rbrack = self.parse_token::<TerminalRBrack>();

                Ok(Attribute::new_green(self.db, hash, lbrack, attr, arguments, rbrack))
            }
            _ => Err(TryParseFailure::SkipToken),
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration>`
    fn expect_function_declaration(
        &mut self,
        optional_const: OptionTerminalConstGreen,
    ) -> FunctionDeclarationGreen {
        let function_kw = self.take::<TerminalFunction>();
        self.expect_function_declaration_ex(optional_const, function_kw)
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration>`
    fn expect_function_declaration_ex(
        &mut self,
        optional_const: OptionTerminalConstGreen,
        function_kw: TerminalFunctionGreen,
    ) -> FunctionDeclarationGreen {
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let signature = self.expect_function_signature();

        FunctionDeclaration::new_green(
            self.db,
            optional_const,
            function_kw,
            name,
            generic_params,
            signature,
        )
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><Block>`
    fn expect_item_function_with_body(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
        optional_const: OptionTerminalConstGreen,
    ) -> FunctionWithBodyGreen {
        let declaration = self.expect_function_declaration(optional_const);
        let function_body = self.parse_block();
        FunctionWithBody::new_green(self.db, attributes, visibility, declaration, function_body)
    }

    /// Assumes the current token is Trait.
    fn expect_item_trait(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemTraitGreen {
        let trait_kw = self.take::<TerminalTrait>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            let lbrace = self.take::<TerminalLBrace>();
            let items = TraitItemList::new_green(
                self.db,
                self.parse_attributed_list(
                    Self::try_parse_trait_item,
                    is_of_kind!(rbrace, module_item_kw),
                    TRAIT_ITEM_DESCRIPTION,
                ),
            );
            let rbrace = self.parse_token::<TerminalRBrace>();
            TraitBody::new_green(self.db, lbrace, items, rbrace).into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };

        ItemTrait::new_green(self.db, attributes, visibility, trait_kw, name, generic_params, body)
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
            SyntaxKind::TerminalFunction => Ok(self
                .expect_trait_item_function(
                    attributes,
                    OptionTerminalConstEmpty::new_green(self.db).into(),
                )
                .into()),
            SyntaxKind::TerminalType => Ok(self.expect_trait_item_type(attributes).into()),
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                Ok(if self.peek().kind == SyntaxKind::TerminalFunction {
                    self.expect_trait_item_function(attributes, const_kw.into()).into()
                } else {
                    self.expect_trait_item_const(attributes, const_kw).into()
                })
            }
            SyntaxKind::TerminalImpl => Ok(self.expect_trait_item_impl(attributes).into()),
            _ => {
                if has_attrs {
                    Ok(self.skip_taken_node_and_return_missing::<TraitItem>(
                        attributes,
                        ParserDiagnosticKind::AttributesWithoutTraitItem,
                    ))
                } else {
                    Err(TryParseFailure::SkipToken)
                }
            }
        }
    }

    /// Assumes the current token is Function.
    /// Expected pattern: `<FunctionDeclaration><SemiColon>`
    fn expect_trait_item_function(
        &mut self,
        attributes: AttributeListGreen,
        optional_const: OptionTerminalConstGreen,
    ) -> TraitItemFunctionGreen {
        let declaration = self.expect_function_declaration(optional_const);
        let body = if self.peek().kind == SyntaxKind::TerminalLBrace {
            self.parse_block().into()
        } else {
            self.parse_token::<TerminalSemicolon>().into()
        };
        TraitItemFunction::new_green(self.db, attributes, declaration, body)
    }

    /// Assumes the current token is Type.
    /// Expected pattern: `type <name>;`
    fn expect_trait_item_type(&mut self, attributes: AttributeListGreen) -> TraitItemTypeGreen {
        let type_kw = self.take::<TerminalType>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        TraitItemType::new_green(self.db, attributes, type_kw, name, generic_params, semicolon)
    }

    /// Assumes the current token is Const.
    /// Expected pattern: `const <name>: <type>;`
    fn expect_trait_item_const(
        &mut self,
        attributes: AttributeListGreen,
        const_kw: TerminalConstGreen,
    ) -> TraitItemConstantGreen {
        let name = self.parse_identifier();
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(eq, semicolon, module_item_kw),
        });
        let semicolon = self.parse_token::<TerminalSemicolon>();

        TraitItemConstant::new_green(self.db, attributes, const_kw, name, type_clause, semicolon)
    }

    /// Assumes the current token is Impl.
    /// Expected pattern: `impl <name>: <trait_path>;`
    fn expect_trait_item_impl(&mut self, attributes: AttributeListGreen) -> TraitItemImplGreen {
        let impl_kw = self.take::<TerminalImpl>();
        let name = self.parse_identifier();
        let colon = self.parse_token::<TerminalColon>();
        let trait_path = self.parse_type_path();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        TraitItemImpl::new_green(self.db, attributes, impl_kw, name, colon, trait_path, semicolon)
    }

    /// Assumes the current token is Impl.
    fn expect_module_item_impl(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ModuleItemGreen {
        match self.expect_item_impl_inner(attributes, visibility, false) {
            ImplItemOrAlias::Item(green) => green.into(),
            ImplItemOrAlias::Alias(green) => green.into(),
        }
    }

    /// Assumes the current token is Impl.
    /// Expects an impl impl-item (impl alias syntax): `impl <name> = <path>;`.
    fn expect_impl_item_impl(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
    ) -> ItemImplAliasGreen {
        extract_matches!(
            self.expect_item_impl_inner(attributes, visibility, true),
            ImplItemOrAlias::Alias
        )
    }

    /// Assumes the current token is Impl.
    /// Expects either an impl item (`impl <name> of <trait_path> {<impl_body>}`) or and impl alias
    /// `impl <name> = <path>;`.
    /// If `only_allow_alias` is true, always returns a ImplItemOrAlias::Alias.
    fn expect_item_impl_inner(
        &mut self,
        attributes: AttributeListGreen,
        visibility: VisibilityGreen,
        only_allow_alias: bool,
    ) -> ImplItemOrAlias {
        let impl_kw = self.take::<TerminalImpl>();
        let name = self.parse_identifier();
        let generic_params = self.parse_optional_generic_params();

        if self.peek().kind == SyntaxKind::TerminalEq || only_allow_alias {
            let eq = self.parse_token::<TerminalEq>();
            let impl_path = self.parse_type_path();
            let semicolon = self.parse_token::<TerminalSemicolon>();

            return ImplItemOrAlias::Alias(ItemImplAlias::new_green(
                self.db,
                attributes,
                visibility,
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
            visibility,
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

        // No visibility in impls, as these just implements the options of a trait, which is always
        // pub.
        let visibility = VisibilityDefault::new_green(self.db).into();

        match self.peek().kind {
            SyntaxKind::TerminalFunction => Ok(self
                .expect_item_function_with_body(
                    attributes,
                    visibility,
                    OptionTerminalConstEmpty::new_green(self.db).into(),
                )
                .into()),
            SyntaxKind::TerminalType => {
                Ok(self.expect_item_type_alias(attributes, visibility).into())
            }
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                Ok(if self.peek().kind == SyntaxKind::TerminalFunction {
                    self.expect_item_function_with_body(attributes, visibility, const_kw.into())
                        .into()
                } else {
                    self.expect_item_const(attributes, visibility, const_kw).into()
                })
            }
            SyntaxKind::TerminalImpl => {
                Ok(self.expect_impl_item_impl(attributes, visibility).into())
            }
            // These are not supported semantically.
            SyntaxKind::TerminalModule => {
                Ok(self.expect_item_module(attributes, visibility).into())
            }
            SyntaxKind::TerminalStruct => {
                Ok(self.expect_item_struct(attributes, visibility).into())
            }
            SyntaxKind::TerminalEnum => Ok(self.expect_item_enum(attributes, visibility).into()),
            SyntaxKind::TerminalExtern => Ok(self.expect_item_extern(attributes, visibility)),
            SyntaxKind::TerminalUse => Ok(self.expect_item_use(attributes, visibility).into()),
            SyntaxKind::TerminalTrait => Ok(self.expect_item_trait(attributes, visibility).into()),
            _ => {
                if has_attrs {
                    Ok(self.skip_taken_node_and_return_missing::<ImplItem>(
                        attributes,
                        ParserDiagnosticKind::AttributesWithoutImplItem,
                    ))
                } else {
                    Err(TryParseFailure::SkipToken)
                }
            }
        }
    }

    /// Assumes the current token is TerminalNot.
    fn expect_item_inline_macro(
        &mut self,
        attributes: AttributeListGreen,
        name: TerminalIdentifierGreen,
    ) -> ItemInlineMacroGreen {
        let bang = self.parse_token::<TerminalNot>();
        self.parse_item_inline_macro_given_bang(attributes, name, bang)
    }

    /// Returns a GreenId of a node with an ItemInlineMacro kind, given the bang ('!') token.
    fn parse_item_inline_macro_given_bang(
        &mut self,
        attributes: AttributeListGreen,
        name: TerminalIdentifierGreen,
        bang: TerminalNotGreen,
    ) -> ItemInlineMacroGreen {
        let arguments = self.parse_wrapped_arg_list();
        let semicolon = self.parse_token::<TerminalSemicolon>();
        ItemInlineMacro::new_green(self.db, attributes, name, bang, arguments, semicolon)
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
            Err(_) => {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression)
            }
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
            SyntaxKind::TerminalDotDot => self.take::<TerminalDotDot>().into(),
            SyntaxKind::TerminalDotDotEq => self.take::<TerminalDotDotEq>().into(),
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

    /// Checks if the current token is a relational or equality operator (`<`, `>`, `<=`, `>=`,
    /// `==`, or `!=`).
    ///
    /// This function is used to determine if the given `SyntaxKind` represents a relational or
    /// equality operator, which is commonly used in binary expressions.
    ///
    /// # Parameters:
    /// - `kind`: The `SyntaxKind` of the current token.
    ///
    /// # Returns:
    /// `true` if the token is a relational or equality operator, otherwise `false`.
    fn is_comparison_operator(&self, kind: SyntaxKind) -> bool {
        matches!(
            kind,
            SyntaxKind::TerminalLT
                | SyntaxKind::TerminalGT
                | SyntaxKind::TerminalLE
                | SyntaxKind::TerminalGE
                | SyntaxKind::TerminalEqEq
                | SyntaxKind::TerminalNeq
        )
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
        let mut child_op: Option<SyntaxKind> = None;
        while let Some(precedence) = get_post_operator_precedence(self.peek().kind) {
            if precedence >= parent_precedence {
                return Ok(expr);
            }
            expr = if self.peek().kind == SyntaxKind::TerminalQuestionMark {
                ExprErrorPropagate::new_green(self.db, expr, self.take::<TerminalQuestionMark>())
                    .into()
            } else if self.peek().kind == SyntaxKind::TerminalLBrack {
                let lbrack = self.take::<TerminalLBrack>();
                let index_expr = self.parse_expr();
                let rbrack = self.parse_token::<TerminalRBrack>();
                ExprIndexed::new_green(self.db, expr, lbrack, index_expr, rbrack).into()
            } else {
                let current_op = self.peek().kind;
                if let Some(child_op_kind) = child_op {
                    if self.is_comparison_operator(child_op_kind)
                        && self.is_comparison_operator(current_op)
                    {
                        self.add_diagnostic(
                            ParserDiagnosticKind::ConsecutiveMathOperators {
                                first_op: child_op_kind,
                                second_op: current_op,
                            },
                            TextSpan { start: self.offset, end: self.offset },
                        );
                    }
                }
                child_op = Some(current_op);
                let op = self.parse_binary_operator();
                let rhs = self.parse_expr_limited(precedence, lbrace_allowed);
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
        let op = self.expect_unary_operator();
        let expr = self.parse_expr_limited(precedence, lbrace_allowed);
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
            Err(_) => {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingExpression)
            }
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
            SyntaxKind::TerminalLiteralNumber => Ok(self.take_terminal_literal_number().into()),
            SyntaxKind::TerminalShortString => Ok(self.take_terminal_short_string().into()),
            SyntaxKind::TerminalString => Ok(self.take_terminal_string().into()),
            SyntaxKind::TerminalLParen => {
                // Note that LBrace is allowed inside parenthesis, even if `lbrace_allowed` is
                // [LbraceAllowed::Forbid].
                Ok(self.expect_parenthesized_expr())
            }
            SyntaxKind::TerminalLBrack => Ok(self.expect_fixed_size_array_expr().into()),
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
            SyntaxKind::TerminalWhile if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_while_expr().into())
            }
            SyntaxKind::TerminalFor if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_for_expr().into())
            }
            SyntaxKind::TerminalOr if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_closure_expr_nary().into())
            }
            SyntaxKind::TerminalOrOr if lbrace_allowed == LbraceAllowed::Allow => {
                Ok(self.expect_closure_expr_nullary().into())
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
                let op = self.take::<TerminalAt>().into();
                let expr = self.parse_type_expr();
                Ok(ExprUnary::new_green(self.db, op, expr).into())
            }
            SyntaxKind::TerminalIdentifier => Ok(self.parse_type_path().into()),
            SyntaxKind::TerminalLParen => Ok(self.expect_type_tuple_expr()),
            SyntaxKind::TerminalLBrack => Ok(self.expect_type_fixed_size_array_expr()),
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
            Err(_) => {
                self.create_and_report_missing::<Expr>(ParserDiagnosticKind::MissingTypeExpression)
            }
        }
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `\{<StructArgList>\}`
    fn expect_struct_ctor_argument_list_braced(&mut self) -> StructArgListBracedGreen {
        let lbrace = self.take::<TerminalLBrace>();
        let arg_list = StructArgList::new_green(
            self.db,
            self.parse_separated_list::<StructArg, TerminalComma, StructArgListElementOrSeparatorGreen>(
                Self::try_parse_struct_ctor_argument,
                is_of_kind!(rparen, block, rbrace, module_item_kw),
                "struct constructor argument",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();

        StructArgListBraced::new_green(self.db, lbrace, arg_list, rbrace)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `<ArgListParenthesized>`
    fn expect_function_call(&mut self, path: ExprPathGreen) -> ExprFunctionCallGreen {
        let func_name = path;
        let parenthesized_args = self.expect_parenthesized_argument_list();
        ExprFunctionCall::new_green(self.db, func_name, parenthesized_args)
    }

    /// Assumes the current token is TerminalNot.
    /// Expected pattern: `!<WrappedArgList>`
    fn expect_macro_call(&mut self, path: ExprPathGreen) -> ExprInlineMacroGreen {
        let bang = self.take::<TerminalNot>();
        let macro_name = path;
        let wrapped_expr_list = self.parse_wrapped_arg_list();
        ExprInlineMacro::new_green(self.db, macro_name, bang, wrapped_expr_list)
    }

    /// Returns a GreenId of a node with an ArgListParenthesized|ArgListBracketed|ArgListBraced kind
    /// or TryParseFailure if such an argument list can't be parsed.
    fn parse_wrapped_arg_list(&mut self) -> WrappedArgListGreen {
        let current_token = self.peek().kind;
        match current_token {
            SyntaxKind::TerminalLParen => self
                .expect_wrapped_argument_list::<TerminalLParen, TerminalRParen, _, _>(
                    ArgListParenthesized::new_green,
                )
                .into(),
            SyntaxKind::TerminalLBrack => self
                .expect_wrapped_argument_list::<TerminalLBrack, TerminalRBrack, _, _>(
                    ArgListBracketed::new_green,
                )
                .into(),
            SyntaxKind::TerminalLBrace => self
                .expect_wrapped_argument_list::<TerminalLBrace, TerminalRBrace, _, _>(
                    ArgListBraced::new_green,
                )
                .into(),
            _ => self.create_and_report_missing::<WrappedArgList>(
                ParserDiagnosticKind::MissingWrappedArgList,
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
                is_of_kind!(rparen, rbrace, rbrack, block, module_item_kw),
                "argument",
            );
        let r_term: <RTerminal as TypedSyntaxNode>::Green = self.parse_token::<RTerminal>();
        new_green(self.db, l_term, ArgList::new_green(self.db, exprs), r_term)
    }

    /// Assumes the current token is LParen.
    /// Expected pattern: `\(<ArgList>\)`
    fn expect_parenthesized_argument_list(&mut self) -> ArgListParenthesizedGreen {
        self.expect_wrapped_argument_list::<TerminalLParen, TerminalRParen, _, _>(
            ArgListParenthesized::new_green,
        )
    }

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
                let modifiers = ModifierList::new_green(self.db, modifiers_list);
                let arg_clause = ArgClauseUnnamed::new_green(self.db, self.parse_expr()).into();
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
            let colon = self.take::<TerminalColon>();
            let name = self.parse_identifier();
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
                let colon = self.take::<TerminalColon>();
                let expr = self.parse_expr();
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
        } = &*expr.0.lookup_intern(self.db)
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
        } = &*path_segment.lookup_intern(self.db)
        else {
            return None;
        };

        // Check that it has one child.
        let [ident] = children1[..] else {
            return None;
        };

        // Check that it is indeed `TerminalIdentifier`.
        let GreenNode { kind: SyntaxKind::TerminalIdentifier, .. } =
            ident.lookup_intern(self.db).as_ref()
        else {
            return None;
        };

        Some(TerminalIdentifierGreen(ident))
    }

    /// Assumes the current token is LBrace.
    /// Expected pattern: `<StructArgListBraced>`
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
                is_of_kind!(rparen, block, rbrace, module_item_kw),
                "expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();

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
        let lparen = self.take::<TerminalLParen>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_type_expr,
                is_of_kind!(rparen, block, rbrace, module_item_kw),
                "type expression",
            );
        let rparen = self.parse_token::<TerminalRParen>();
        if let [ExprListElementOrSeparatorGreen::Element(_)] = &exprs[..] {
            self.add_diagnostic(
                ParserDiagnosticKind::MissingToken(SyntaxKind::TokenComma),
                TextSpan { start: self.offset, end: self.offset },
            );
        }
        ExprListParenthesized::new_green(
            self.db,
            lparen,
            ExprList::new_green(self.db, exprs),
            rparen,
        )
        .into()
    }

    /// Assumes the current token is LBrack.
    /// Expected pattern: `\[<type_expr>; <expr>\]`.
    /// Returns a GreenId of a node with kind ExprFixedSizeArray.
    fn expect_type_fixed_size_array_expr(&mut self) -> ExprGreen {
        let lbrack = self.take::<TerminalLBrack>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_type_expr,
                is_of_kind!(rbrack, semicolon),
                "type expression",
            );
        let semicolon = self.parse_token::<TerminalSemicolon>();
        let size_expr = self.parse_expr();
        let fixed_size_array_size =
            FixedSizeArraySize::new_green(self.db, semicolon, size_expr).into();
        let rbrack = self.parse_token::<TerminalRBrack>();
        ExprFixedSizeArray::new_green(
            self.db,
            lbrack,
            ExprList::new_green(self.db, exprs),
            fixed_size_array_size,
            rbrack,
        )
        .into()
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
        let struct_arg_expr = self.parse_option_struct_arg_expression(); // :<expr>
        Ok(StructArgSingle::new_green(self.db, identifier, struct_arg_expr))
    }

    /// Returns a GreenId of a node with kind ExprBlock.
    fn parse_block(&mut self) -> ExprBlockGreen {
        let skipped_tokens = self.skip_until(is_of_kind!(rbrace, lbrace, module_item_kw, block));

        if let Err(SkippedError(span)) = skipped_tokens {
            self.add_diagnostic(
                ParserDiagnosticKind::SkippedElement { element_name: "'{'".into() },
                span,
            );
        }

        let is_rbrace_or_top_level = is_of_kind!(rbrace, module_item_kw);
        if is_rbrace_or_top_level(self.peek().kind) {
            return ExprBlock::new_green(
                self.db,
                self.create_and_report_missing_terminal::<TerminalLBrace>(),
                StatementList::new_green(self.db, vec![]),
                TerminalRBrace::missing(self.db),
            );
        }
        // Don't report diagnostic if one has already been reported.
        let lbrace = self.parse_token_ex::<TerminalLBrace>(skipped_tokens.is_ok());
        let statements = StatementList::new_green(
            self.db,
            self.parse_list(
                Self::try_parse_statement,
                is_of_kind!(rbrace, module_item_kw),
                "statement",
            ),
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
                is_of_kind!(block, rbrace, module_item_kw),
                "match arm",
            ),
        );
        let rbrace = self.parse_token::<TerminalRBrace>();
        ExprMatch::new_green(self.db, match_kw, expr, lbrace, arms, rbrace)
    }

    /// Assumes the current token is `If`.
    fn expect_if_expr(&mut self) -> ExprIfGreen {
        let if_kw = self.take::<TerminalIf>();

        let condition = self.parse_condition_expr();
        let if_block = self.parse_block();
        let else_clause = if self.peek().kind == SyntaxKind::TerminalElse {
            let else_kw = self.take::<TerminalElse>();
            let else_block_or_if = if self.peek().kind == SyntaxKind::TerminalIf {
                self.expect_if_expr().into()
            } else {
                self.parse_block().into()
            };
            ElseClause::new_green(self.db, else_kw, else_block_or_if).into()
        } else {
            OptionElseClauseEmpty::new_green(self.db).into()
        };
        ExprIf::new_green(self.db, if_kw, condition, if_block, else_clause)
    }

    /// Parses condition exprs of the form `<expr>` or `let <pattern> = <expr>`.
    fn parse_condition_expr(&mut self) -> ConditionGreen {
        if self.peek().kind == SyntaxKind::TerminalLet {
            let let_kw = self.take::<TerminalLet>();
            let pattern_list = self
            .parse_separated_list_inner::<Pattern, TerminalOr, PatternListOrElementOrSeparatorGreen>(
                Self::try_parse_pattern,
                is_of_kind!(eq),
                "pattern",
                Some(ParserDiagnosticKind::DisallowedTrailingSeparatorOr),
            );

            let pattern_list_green = if pattern_list.is_empty() {
                self.create_and_report_missing::<PatternListOr>(
                    ParserDiagnosticKind::MissingPattern,
                )
            } else {
                PatternListOr::new_green(self.db, pattern_list)
            };
            let eq = self.parse_token::<TerminalEq>();
            let expr: ExprGreen = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
            ConditionLet::new_green(self.db, let_kw, pattern_list_green, eq, expr).into()
        } else {
            let condition = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
            ConditionExpr::new_green(self.db, condition).into()
        }
    }

    /// Assumes the current token is `Loop`.
    /// Expected pattern: `loop <block>`.
    fn expect_loop_expr(&mut self) -> ExprLoopGreen {
        let loop_kw = self.take::<TerminalLoop>();
        let body = self.parse_block();

        ExprLoop::new_green(self.db, loop_kw, body)
    }

    /// Assumes the current token is `While`.
    /// Expected pattern: `while <condition> <block>`.
    fn expect_while_expr(&mut self) -> ExprWhileGreen {
        let while_kw = self.take::<TerminalWhile>();
        let condition = self.parse_condition_expr();
        let body = self.parse_block();

        ExprWhile::new_green(self.db, while_kw, condition, body)
    }

    /// Assumes the current token is `For`.
    /// Expected pattern: `for <pattern> <identifier> <expression> <block>`.
    /// Identifier will be checked to be 'in' in semantics.
    fn expect_for_expr(&mut self) -> ExprForGreen {
        let for_kw = self.take::<TerminalFor>();
        let pattern = self.parse_pattern();
        let ident = self.take_raw();
        let in_identifier: TerminalIdentifierGreen = match ident.text.as_str() {
            "in" => self.add_trivia_to_terminal::<TerminalIdentifier>(ident),
            _ => {
                self.append_skipped_token_to_pending_trivia(
                    ident,
                    ParserDiagnosticKind::SkippedElement { element_name: "'in'".into() },
                );
                TerminalIdentifier::missing(self.db)
            }
        };
        let expression = self.parse_expr_limited(MAX_PRECEDENCE, LbraceAllowed::Forbid);
        let body = self.parse_block();
        ExprFor::new_green(self.db, for_kw, pattern, in_identifier, expression, body)
    }

    /// Assumes the current token is `|`.
    /// Expected pattern: `| <params> | <ReturnTypeClause> <expression>`.
    fn expect_closure_expr_nary(&mut self) -> ExprClosureGreen {
        let leftor = self.take::<TerminalOr>();
        let params = self.parse_closure_param_list();
        let rightor = self.parse_token::<TerminalOr>();

        self.parse_closure_expr_body(
            ClosureParamWrapperNAry::new_green(self.db, leftor, params, rightor).into(),
        )
    }
    /// Assumes the current token is `||`.
    /// Expected pattern: `|| <ReturnTypeClause> <expression> `.
    fn expect_closure_expr_nullary(&mut self) -> ExprClosureGreen {
        let wrapper = self.take::<TerminalOrOr>().into();
        self.parse_closure_expr_body(wrapper)
    }
    fn parse_closure_expr_body(&mut self, wrapper: ClosureParamWrapperGreen) -> ExprClosureGreen {
        let mut block_required = self.peek().kind == SyntaxKind::TerminalArrow;

        let return_type_clause = self.parse_option_return_type_clause();
        let optional_no_panic = if self.peek().kind == SyntaxKind::TerminalNoPanic {
            block_required = true;
            self.take::<TerminalNoPanic>().into()
        } else {
            OptionTerminalNoPanicEmpty::new_green(self.db).into()
        };
        let expr = if block_required { self.parse_block().into() } else { self.parse_expr() };

        ExprClosure::new_green(self.db, wrapper, return_type_clause, optional_no_panic, expr)
    }

    /// Assumes the current token is LBrack.
    /// Expected pattern: `\[<expr>; <expr>\]`.
    fn expect_fixed_size_array_expr(&mut self) -> ExprFixedSizeArrayGreen {
        let lbrack = self.take::<TerminalLBrack>();
        let exprs: Vec<ExprListElementOrSeparatorGreen> = self
            .parse_separated_list::<Expr, TerminalComma, ExprListElementOrSeparatorGreen>(
                Self::try_parse_expr,
                is_of_kind!(rbrack, semicolon),
                "expression",
            );
        let size_green = if self.peek().kind == SyntaxKind::TerminalSemicolon {
            let semicolon = self.take::<TerminalSemicolon>();
            let size = self.parse_expr();
            FixedSizeArraySize::new_green(self.db, semicolon, size).into()
        } else {
            OptionFixedSizeArraySizeEmpty::new_green(self.db).into()
        };
        let rbrack = self.parse_token::<TerminalRBrack>();
        ExprFixedSizeArray::new_green(
            self.db,
            lbrack,
            ExprList::new_green(self.db, exprs),
            size_green,
            rbrack,
        )
    }

    /// Returns a GreenId of a node with a MatchArm kind or TryParseFailure if a match arm can't be
    /// parsed.
    pub fn try_parse_match_arm(&mut self) -> TryParseResult<MatchArmGreen> {
        let pattern_list = self
            .parse_separated_list_inner::<Pattern, TerminalOr, PatternListOrElementOrSeparatorGreen>(
                Self::try_parse_pattern,
                is_of_kind!(match_arrow, rparen, block, rbrace, module_item_kw),
                "pattern",
                Some(ParserDiagnosticKind::DisallowedTrailingSeparatorOr),
            );
        if pattern_list.is_empty() {
            return Err(TryParseFailure::SkipToken);
        }

        let pattern_list_green = PatternListOr::new_green(self.db, pattern_list);

        let arrow = self.parse_token::<TerminalMatchArrow>();
        let expr = self.parse_expr();
        Ok(MatchArm::new_green(self.db, pattern_list_green, arrow, expr))
    }

    /// Returns a GreenId of a node with some Pattern kind (see
    /// [syntax::node::ast::Pattern]) or TryParseFailure if a pattern can't be parsed.
    fn try_parse_pattern(&mut self) -> TryParseResult<PatternGreen> {
        let modifier_list = self.parse_modifier_list();
        if !modifier_list.is_empty() {
            let modifiers = ModifierList::new_green(self.db, modifier_list);
            let name = self.parse_identifier();
            return Ok(PatternIdentifier::new_green(self.db, modifiers, name).into());
        };

        // TODO(yuval): Support "Or" patterns.
        Ok(match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => self.take_terminal_literal_number().into(),
            SyntaxKind::TerminalShortString => self.take_terminal_short_string().into(),
            SyntaxKind::TerminalTrue => self.take::<TerminalTrue>().into(),
            SyntaxKind::TerminalFalse => self.take::<TerminalFalse>().into(),
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
                                is_of_kind!(rparen, block, rbrace, module_item_kw),
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
                        let inner_pattern =
                            PatternEnumInnerPattern::new_green(self.db, lparen, pattern, rparen);
                        PatternEnum::new_green(self.db, path, inner_pattern.into()).into()
                    }
                    _ => {
                        let green_node = path.0.lookup_intern(self.db);
                        let children = match &green_node.details {
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
                    is_of_kind!(rparen, block, rbrace, module_item_kw),
                    "pattern",
                ));
                let rparen = self.parse_token::<TerminalRParen>();
                PatternTuple::new_green(self.db, lparen, patterns, rparen).into()
            }
            SyntaxKind::TerminalLBrack => {
                let lbrack = self.take::<TerminalLBrack>();
                let patterns = PatternList::new_green(self.db,  self.parse_separated_list::<
                    Pattern,
                    TerminalComma,
                    PatternListElementOrSeparatorGreen>
                (
                    Self::try_parse_pattern,
                    is_of_kind!(rbrack, block, rbrace, module_item_kw),
                    "pattern",
                ));
                let rbrack = self.parse_token::<TerminalRBrack>();
                PatternFixedSizeArray::new_green(self.db, lbrack, patterns, rbrack).into()
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
    /// [syntax::node::ast::Statement]) or TryParseFailure if a statement can't be parsed.
    pub fn try_parse_statement(&mut self) -> TryParseResult<StatementGreen> {
        let maybe_attributes = self.try_parse_attribute_list("Statement");
        let (has_attrs, attributes) = match maybe_attributes {
            Ok(attributes) => (true, attributes),
            Err(_) => (false, AttributeList::new_green(self.db, vec![])),
        };
        match self.peek().kind {
            SyntaxKind::TerminalLet => {
                let let_kw = self.take::<TerminalLet>();
                let pattern = self.parse_pattern();
                let type_clause = self.parse_option_type_clause();
                let eq = self.parse_token::<TerminalEq>();
                let rhs = self.parse_expr();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Ok(StatementLet::new_green(
                    self.db,
                    attributes,
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
                let continue_kw = self.take::<TerminalContinue>();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Ok(StatementContinue::new_green(self.db, attributes, continue_kw, semicolon).into())
            }
            SyntaxKind::TerminalReturn => {
                let return_kw = self.take::<TerminalReturn>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Ok(StatementReturn::new_green(self.db, attributes, return_kw, expr, semicolon)
                    .into())
            }
            SyntaxKind::TerminalBreak => {
                let break_kw = self.take::<TerminalBreak>();
                let expr = self.parse_option_expression_clause();
                let semicolon = self.parse_token::<TerminalSemicolon>();
                Ok(StatementBreak::new_green(self.db, attributes, break_kw, expr, semicolon).into())
            }
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                Ok(StatementItem::new_green(
                    self.db,
                    self.expect_item_const(
                        attributes,
                        VisibilityDefault::new_green(self.db).into(),
                        const_kw,
                    )
                    .into(),
                )
                .into())
            }
            SyntaxKind::TerminalUse => Ok(StatementItem::new_green(
                self.db,
                self.expect_item_use(attributes, VisibilityDefault::new_green(self.db).into())
                    .into(),
            )
            .into()),
            SyntaxKind::TerminalType => Ok(StatementItem::new_green(
                self.db,
                self.expect_item_type_alias(
                    attributes,
                    VisibilityDefault::new_green(self.db).into(),
                )
                .into(),
            )
            .into()),
            _ => match self.try_parse_expr() {
                Ok(expr) => {
                    let optional_semicolon = if self.peek().kind == SyntaxKind::TerminalSemicolon {
                        self.take::<TerminalSemicolon>().into()
                    } else {
                        OptionTerminalSemicolonEmpty::new_green(self.db).into()
                    };
                    Ok(StatementExpr::new_green(self.db, attributes, expr, optional_semicolon)
                        .into())
                }
                Err(_) if has_attrs => Ok(self.skip_taken_node_and_return_missing::<Statement>(
                    attributes,
                    ParserDiagnosticKind::AttributesWithoutStatement,
                )),
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
                is_of_kind!(rparen, block, lbrace, rbrace, module_item_kw),
                "parameter",
            ),
        )
    }

    /// Returns a GreenId of a node with kind ClosureParamList.
    fn parse_closure_param_list(&mut self) -> ParamListGreen {
        ParamList::new_green(
            self.db,
            self.parse_separated_list::<Param, TerminalComma, ParamListElementOrSeparatorGreen>(
                Self::try_parse_closure_param,
                is_of_kind!(or, block, lbrace, rbrace, module_item_kw),
                "parameter",
            ),
        )
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
            self.try_parse_identifier()?
        } else {
            // If we had modifiers then the identifier is not optional and can't be '_'.
            self.parse_identifier()
        };

        let type_clause = self
            .parse_type_clause(ErrorRecovery {
                should_stop: is_of_kind!(comma, rparen, module_item_kw),
            })
            .into();
        Ok(Param::new_green(
            self.db,
            ModifierList::new_green(self.db, modifier_list),
            name,
            type_clause,
        ))
    }

    /// Returns a GreenId of a node with kind Param or TryParseFailure if a parameter can't
    /// be parsed.
    fn try_parse_closure_param(&mut self) -> TryParseResult<ParamGreen> {
        let modifier_list = self.parse_modifier_list();
        let name = if modifier_list.is_empty() {
            self.try_parse_identifier()?
        } else {
            // If we had modifiers then the identifier is not optional and can't be '_'.
            self.parse_identifier()
        };

        let type_clause = self.parse_option_type_clause();
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
                is_of_kind!(rparen, block, lbrace, rbrace, module_item_kw),
                "member or variant",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Member or TryParseFailure if a struct member can't be
    /// parsed.
    fn try_parse_member(&mut self) -> TryParseResult<MemberGreen> {
        let attributes = self.try_parse_attribute_list("Struct member");
        let visibility = self.parse_visibility();
        let (name, attributes) = match attributes {
            Ok(attributes) => (self.parse_identifier(), attributes),
            Err(_) => (self.try_parse_identifier()?, AttributeList::new_green(self.db, vec![])),
        };
        let type_clause = self.parse_type_clause(ErrorRecovery {
            should_stop: is_of_kind!(comma, rbrace, module_item_kw),
        });
        Ok(Member::new_green(self.db, attributes, visibility, name, type_clause))
    }

    /// Returns a GreenId of a node with kind VariantList.
    fn parse_variant_list(&mut self) -> VariantListGreen {
        VariantList::new_green(
            self.db,
            self.parse_separated_list::<Variant, TerminalComma, VariantListElementOrSeparatorGreen>(
                Self::try_parse_variant,
                is_of_kind!(rparen, block, lbrace, rbrace, module_item_kw),
                "variant",
            ),
        )
    }

    /// Returns a GreenId of a node with kind Variant or TryParseFailure if an enum variant can't be
    /// parsed.
    fn try_parse_variant(&mut self) -> TryParseResult<VariantGreen> {
        let attributes = self.try_parse_attribute_list("Enum variant");
        let (name, attributes) = match attributes {
            Ok(attributes) => (self.parse_identifier(), attributes),
            Err(_) => (self.try_parse_identifier()?, AttributeList::new_green(self.db, vec![])),
        };

        let type_clause = self.parse_option_type_clause();
        Ok(Variant::new_green(self.db, attributes, name, type_clause))
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
            Ok(identifier) => identifier,
            Err(_) => {
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
                        ParserDiagnosticKind::MissingPathSegment,
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

    /// Takes and validates a TerminalLiteralNumber token.
    fn take_terminal_literal_number(&mut self) -> TerminalLiteralNumberGreen {
        let text = self.peek().text.clone();
        let green = self.take::<TerminalLiteralNumber>();
        let span = TextSpan { start: self.offset, end: self.offset.add_width(self.current_width) };

        validate_literal_number(self.diagnostics, text, span, self.file_id);
        green
    }

    /// Takes and validates a TerminalShortString token.
    fn take_terminal_short_string(&mut self) -> TerminalShortStringGreen {
        let text = self.peek().text.clone();
        let green = self.take::<TerminalShortString>();
        let span = TextSpan { start: self.offset, end: self.offset.add_width(self.current_width) };

        validate_short_string(self.diagnostics, text, span, self.file_id);
        green
    }

    /// Takes and validates a TerminalString token.
    fn take_terminal_string(&mut self) -> TerminalStringGreen {
        let text = self.peek().text.clone();
        let green = self.take::<TerminalString>();
        let span = TextSpan { start: self.offset, end: self.offset.add_width(self.current_width) };

        validate_string(self.diagnostics, text, span, self.file_id);
        green
    }

    /// Returns a GreenId of a node with an
    /// ExprLiteral|ExprPath|ExprParenthesized|ExprTuple|ExprUnderscore kind, or TryParseFailure if
    /// such an expression can't be parsed.
    fn try_parse_generic_arg(&mut self) -> TryParseResult<GenericArgGreen> {
        if self.peek().kind == SyntaxKind::TerminalUnderscore {
            let underscore = self.take::<TerminalUnderscore>().into();
            return Ok(GenericArgUnnamed::new_green(self.db, underscore).into());
        }

        let expr = match self.peek().kind {
            SyntaxKind::TerminalLiteralNumber => self.take_terminal_literal_number().into(),
            SyntaxKind::TerminalMinus => {
                let op = self.take::<TerminalMinus>().into();
                let expr = self.parse_token::<TerminalLiteralNumber>().into();
                ExprUnary::new_green(self.db, op, expr).into()
            }
            SyntaxKind::TerminalShortString => self.take_terminal_short_string().into(),
            SyntaxKind::TerminalTrue => self.take::<TerminalTrue>().into(),
            SyntaxKind::TerminalFalse => self.take::<TerminalFalse>().into(),
            SyntaxKind::TerminalLBrace => self.parse_block().into(),
            _ => self.try_parse_type_expr()?,
        };

        // If the next token is `:` and the expression is an identifier, this is the argument's
        // name.
        if self.peek().kind == SyntaxKind::TerminalColon {
            if let Some(argname) = self.try_extract_identifier(expr) {
                let colon = self.take::<TerminalColon>();
                let expr = if self.peek().kind == SyntaxKind::TerminalUnderscore {
                    self.take::<TerminalUnderscore>().into()
                } else {
                    let expr = self.parse_type_expr();
                    GenericArgValueExpr::new_green(self.db, expr).into()
                };
                return Ok(GenericArgNamed::new_green(self.db, argname, colon, expr).into());
            }
        }
        Ok(GenericArgUnnamed::new_green(
            self.db,
            GenericArgValueExpr::new_green(self.db, expr).into(),
        )
        .into())
    }

    /// Assumes the current token is LT.
    /// Expected pattern: `\< <GenericArgList> \>`
    fn expect_generic_args(&mut self) -> GenericArgsGreen {
        let langle = self.take::<TerminalLT>();
        let generic_args = GenericArgList::new_green(
            self.db,
            self.parse_separated_list::<GenericArg, TerminalComma, GenericArgListElementOrSeparatorGreen>(
                Self::try_parse_generic_arg,
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, module_item_kw),
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
                is_of_kind!(rangle, rparen, block, lbrace, rbrace, module_item_kw),
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

    fn try_parse_generic_param(&mut self) -> TryParseResult<GenericParamGreen> {
        match self.peek().kind {
            SyntaxKind::TerminalConst => {
                let const_kw = self.take::<TerminalConst>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let ty = self.parse_type_expr();
                Ok(GenericParamConst::new_green(self.db, const_kw, name, colon, ty).into())
            }
            SyntaxKind::TerminalImpl => {
                let impl_kw = self.take::<TerminalImpl>();
                let name = self.parse_identifier();
                let colon = self.parse_token::<TerminalColon>();
                let trait_path = self.parse_type_path();
                let associated_item_constraints = self.parse_optional_associated_item_constraints();
                Ok(GenericParamImplNamed::new_green(
                    self.db,
                    impl_kw,
                    name,
                    colon,
                    trait_path,
                    associated_item_constraints,
                )
                .into())
            }
            SyntaxKind::TerminalPlus => {
                let plus = self.take::<TerminalPlus>();
                let trait_path = self.parse_type_path();
                let associated_item_constraints = self.parse_optional_associated_item_constraints();
                Ok(GenericParamImplAnonymous::new_green(
                    self.db,
                    plus,
                    trait_path,
                    associated_item_constraints,
                )
                .into())
            }
            SyntaxKind::TerminalMinus => {
                let minus = self.take::<TerminalMinus>();
                let trait_path = self.parse_type_path();
                Ok(GenericParamNegativeImpl::new_green(self.db, minus, trait_path).into())
            }
            _ => Ok(GenericParamType::new_green(self.db, self.try_parse_identifier()?).into()),
        }
    }

    /// Assumes the current token is LBrack.
    /// Expected pattern: `[ <associated_item_constraints_list> ]>`
    fn expect_associated_item_constraints(&mut self) -> AssociatedItemConstraintsGreen {
        let lbrack = self.take::<TerminalLBrack>();
        let associated_item_constraints_list = AssociatedItemConstraintList::new_green(
            self.db,
            self.parse_separated_list::<AssociatedItemConstraint, TerminalComma, AssociatedItemConstraintListElementOrSeparatorGreen>(
                Self::try_parse_associated_item_constraint,
                is_of_kind!(rbrack,rangle, rparen, block, lbrace, rbrace, module_item_kw),
                "associated type argument",
            ),
        );
        let rangle = self.parse_token::<TerminalRBrack>();
        AssociatedItemConstraints::new_green(
            self.db,
            lbrack,
            associated_item_constraints_list,
            rangle,
        )
    }

    fn parse_optional_associated_item_constraints(
        &mut self,
    ) -> OptionAssociatedItemConstraintsGreen {
        if self.peek().kind != SyntaxKind::TerminalLBrack {
            return OptionAssociatedItemConstraintsEmpty::new_green(self.db).into();
        }
        self.expect_associated_item_constraints().into()
    }

    /// Returns a GreenId of a node with kind AssociatedTypeArg or TryParseFailure if an associated
    /// type argument can't be parsed.
    fn try_parse_associated_item_constraint(
        &mut self,
    ) -> TryParseResult<AssociatedItemConstraintGreen> {
        let ident = self.try_parse_identifier()?;
        let colon = self.parse_token::<TerminalColon>();
        let ty = self.parse_type_expr();
        Ok(AssociatedItemConstraint::new_green(self.db, ident, colon, ty))
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
            &or_an_attribute!(expected_element),
        )
    }

    /// Parses a list of elements with `separator`s, where the elements are parsed using
    /// `try_parse_list_element`. Depending on the value of
    /// `forbid_trailing_separator` the separator may or may not appear in
    /// the end of the list. Returns the list of elements and separators. This list contains
    /// alternating children: [element, separator, element, separator, ...]. Separators may be
    /// missing. The length of the list is either 2 * #elements - 1 or 2 * #elements (a
    /// separator for each element or for each element but the last one).
    ///
    /// `should_stop` is a predicate to decide how to proceed in case an element or a separator
    /// can't be parsed, according to the current token.
    /// When parsing an element:
    /// If it returns true, the parsing of the list stops. If it returns false, the current token
    /// is skipped and we try to parse an element again.
    /// When parsing a separator:
    /// If it returns true, the parsing of the list stops. If it returns false, a missing separator
    /// is added and we continue to try to parse another element (with the same token).
    fn parse_separated_list_inner<
        Element: TypedSyntaxNode,
        Separator: syntax::node::Terminal,
        ElementOrSeparatorGreen,
    >(
        &mut self,
        try_parse_list_element: fn(&mut Self) -> TryParseResult<Element::Green>,
        should_stop: fn(SyntaxKind) -> bool,
        expected_element: &'static str,
        forbid_trailing_separator: Option<ParserDiagnosticKind>,
    ) -> Vec<ElementOrSeparatorGreen>
    where
        ElementOrSeparatorGreen: From<Separator::Green> + From<Element::Green>,
    {
        let mut children: Vec<ElementOrSeparatorGreen> = Vec::new();
        loop {
            match try_parse_list_element(self) {
                Err(_) if should_stop(self.peek().kind) => {
                    if let (Some(diagnostic_kind), true) =
                        (forbid_trailing_separator, !children.is_empty())
                    {
                        self.add_diagnostic(
                            diagnostic_kind,
                            TextSpan { start: self.offset, end: self.offset },
                        );
                    }
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
                    ParserDiagnosticKind::MissingToken(Separator::KIND),
                ),
                Ok(separator) => separator,
            };
            children.push(separator.into());
        }
        children
    }
    /// Calls parse_separated_list_inner with trailing separator enabled.
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
        self.parse_separated_list_inner::<Element, Separator, ElementOrSeparatorGreen>(
            try_parse_list_element,
            should_stop,
            expected_element,
            None,
        )
    }

    /// Peeks at the next terminal from the Lexer without taking it.
    fn peek(&self) -> &LexerTerminal {
        &self.next_terminal
    }

    /// Takes a terminal from the Lexer and places it in self.next_terminal.
    fn take_raw(&mut self) -> LexerTerminal {
        self.offset = self.offset.add_width(self.current_width);
        self.current_width = self.next_terminal.width(self.db);
        self.last_trivia_length = trivia_total_width(self.db, &self.next_terminal.trailing_trivia);

        let next_terminal = self.lexer.next().unwrap();
        std::mem::replace(&mut self.next_terminal, next_terminal)
    }

    /// Skips the next, non-taken, token. A skipped token is a token which is not expected where it
    /// is found. Skipping this token means reporting an error, appending the token to the
    /// current trivia as skipped, and continuing the compilation as if it wasn't there.
    fn skip_token(&mut self, diagnostic_kind: ParserDiagnosticKind) {
        if self.peek().kind == SyntaxKind::TerminalEndOfFile {
            self.add_diagnostic(diagnostic_kind, TextSpan { start: self.offset, end: self.offset });
            return;
        }
        let terminal = self.take_raw();
        self.append_skipped_token_to_pending_trivia(terminal, diagnostic_kind);
    }

    /// Appends the given terminal to the pending trivia and reports a diagnostic. Used for skipping
    /// a taken ('take_raw') token.
    fn append_skipped_token_to_pending_trivia(
        &mut self,
        terminal: LexerTerminal,
        diagnostic_kind: ParserDiagnosticKind,
    ) {
        let orig_offset = self.offset;
        let diag_start =
            self.offset.add_width(trivia_total_width(self.db, &terminal.leading_trivia));
        let diag_end = diag_start.add_width(TextWidth::from_str(&terminal.text));

        // Add to pending trivia.
        self.pending_trivia.extend(terminal.leading_trivia.clone());
        self.pending_trivia.push(TokenSkipped::new_green(self.db, terminal.text).into());
        self.pending_trivia.extend(terminal.trailing_trivia.clone());
        self.pending_skipped_token_diagnostics.push(PendingParserDiagnostic {
            kind: diagnostic_kind,
            span: TextSpan { start: diag_start, end: diag_end },
            leading_trivia_start: orig_offset,
            trailing_trivia_end: diag_end
                .add_width(trivia_total_width(self.db, &terminal.trailing_trivia)),
        });
    }

    /// A wrapper for `skip_taken_node_with_offset` to report the skipped node diagnostic relative
    /// to the current offset. Use this when the skipped node is the last node taken.
    fn skip_taken_node_from_current_offset(
        &mut self,
        node_to_skip: impl Into<SkippedNodeGreen>,
        diagnostic_kind: ParserDiagnosticKind,
    ) {
        self.skip_taken_node_with_offset(
            node_to_skip,
            diagnostic_kind,
            self.offset.add_width(self.current_width),
        )
    }

    /// Skips the given node which is a variant of `SkippedNode` and is already taken. A skipped
    /// node is a node which is not expected where it is found. Skipping this node means
    /// reporting the given error (pointing to right after the node), appending the node to the
    /// current trivia as skipped, and continuing the compilation as if it wasn't there.
    /// `end_of_node_offset` is the offset of the end of the skipped node.
    fn skip_taken_node_with_offset(
        &mut self,
        node_to_skip: impl Into<SkippedNodeGreen>,
        diagnostic_kind: ParserDiagnosticKind,
        end_of_node_offset: TextOffset,
    ) {
        let trivium_green = TriviumSkippedNode::new_green(self.db, node_to_skip.into()).into();

        // Add to pending trivia.
        self.pending_trivia.push(trivium_green);

        let start_of_node_offset = end_of_node_offset.sub_width(trivium_green.0.width(self.db));
        let diag_pos = end_of_node_offset
            .sub_width(trailing_trivia_width(self.db, trivium_green.0).unwrap_or_default());

        self.pending_skipped_token_diagnostics.push(PendingParserDiagnostic {
            kind: diagnostic_kind,
            span: TextSpan { start: diag_pos, end: diag_pos },
            leading_trivia_start: start_of_node_offset,
            trailing_trivia_end: end_of_node_offset,
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

    /// Skips a given SkippedNode, reports the given diagnostic (pointing to right after the node)
    /// and returns missing kind of the expected node.
    fn skip_taken_node_and_return_missing<ExpectedNode: TypedSyntaxNode>(
        &mut self,
        node_to_skip: impl Into<SkippedNodeGreen>,
        diagnostic_kind: ParserDiagnosticKind,
    ) -> ExpectedNode::Green {
        self.skip_taken_node_from_current_offset(node_to_skip, diagnostic_kind);
        ExpectedNode::missing(self.db)
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

        self.consume_pending_skipped_diagnostics();

        new_leading_trivia.extend(leading_trivia);
        Terminal::new_green(
            self.db,
            Trivia::new_green(self.db, new_leading_trivia),
            token,
            Trivia::new_green(self.db, trailing_trivia),
        )
    }

    /// Adds the pending skipped-tokens diagnostics, merging consecutive similar ones, and reset
    /// self.pending_skipped_token_diagnostics.
    fn consume_pending_skipped_diagnostics(&mut self) {
        let mut pending_skipped = self.pending_skipped_token_diagnostics.drain(..);
        let Some(first) = pending_skipped.next() else {
            return;
        };

        let mut current_diag = first;

        for diag in pending_skipped {
            if diag.kind == current_diag.kind
                && current_diag.trailing_trivia_end == diag.leading_trivia_start
            {
                // Aggregate this diagnostic with the previous ones.
                current_diag = PendingParserDiagnostic {
                    span: TextSpan { start: current_diag.span.start, end: diag.span.end },
                    kind: diag.kind,
                    leading_trivia_start: current_diag.leading_trivia_start,
                    trailing_trivia_end: diag.trailing_trivia_end,
                };
            } else {
                // Produce a diagnostic from the aggregated ones, and start aggregating a new
                // diagnostic.
                self.diagnostics.add(ParserDiagnostic {
                    file_id: self.file_id,
                    span: current_diag.span,
                    kind: current_diag.kind,
                });
                current_diag = diag;
            }
        }
        // Produce a diagnostic from the aggregated ones at the end.
        self.add_diagnostic(current_diag.kind, current_diag.span);
    }

    /// Takes a token from the Lexer and place it in self.current. If tokens were skipped, glue them
    /// to this token as leading trivia.
    fn take<Terminal: syntax::node::Terminal>(&mut self) -> Terminal::Green {
        let token = self.take_raw();
        assert_eq!(token.kind, Terminal::KIND);
        self.add_trivia_to_terminal::<Terminal>(token)
    }

    /// If the current leading trivia start with non-doc comments, creates a new `ItemHeaderDoc` and
    /// appends the trivia to it. The purpose of this item is to prevent non-doc comments moving
    /// from the top of the file formatter.
    fn take_doc(&mut self) -> Option<ItemHeaderDocGreen> {
        // Take all the trivia from `self.next_terminal`, until a doc-comment is found. If the
        // result does not contain a non-doc comment (i.e. regular comment or inner
        // comment), return None and do not change the next terminal leading trivia.
        let mut has_header_doc = false;
        let mut split_index = 0;
        for trivium in self.next_terminal.leading_trivia.iter() {
            match trivium.0.lookup_intern(self.db).kind {
                SyntaxKind::TokenSingleLineComment | SyntaxKind::TokenSingleLineInnerComment => {
                    has_header_doc = true;
                }
                SyntaxKind::TokenSingleLineDocComment => {
                    break;
                }
                _ => {}
            }
            split_index += 1;
        }
        if !has_header_doc {
            return None;
        }
        // Split the trivia into header doc and the rest.
        let leading_trivia = self.next_terminal.leading_trivia.clone();
        let (header_doc, rest) = leading_trivia.split_at(split_index);
        self.next_terminal.leading_trivia = rest.to_vec();
        let empty_lexer_terminal = LexerTerminal {
            text: "".into(),
            kind: SyntaxKind::TerminalEmpty,
            leading_trivia: header_doc.to_vec(),
            trailing_trivia: vec![],
        };
        self.offset = self.offset.add_width(empty_lexer_terminal.width(self.db));

        let empty_terminal = self.add_trivia_to_terminal::<TerminalEmpty>(empty_lexer_terminal);
        Some(ItemHeaderDoc::new_green(self.db, empty_terminal))
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

#[derive(Debug)]
enum ImplItemOrAlias {
    Item(ItemImplGreen),
    Alias(ItemImplAliasGreen),
}

/// A parser diagnostic that is not yet reported as it is accumulated with similar consecutive
/// diagnostics.
pub struct PendingParserDiagnostic {
    pub span: TextSpan,
    pub kind: ParserDiagnosticKind,
    pub leading_trivia_start: TextOffset,
    pub trailing_trivia_end: TextOffset,
}

/// Returns the total width of the given trivia list.
fn trivia_total_width(db: &dyn SyntaxGroup, trivia: &[TriviumGreen]) -> TextWidth {
    trivia.iter().map(|trivium| trivium.0.width(db)).sum::<TextWidth>()
}

/// The width of the trailing trivia, traversing the tree to the bottom right node.
fn trailing_trivia_width(db: &dyn SyntaxGroup, green_id: GreenId) -> Option<TextWidth> {
    let node = green_id.lookup_intern(db);
    if node.kind == SyntaxKind::Trivia {
        return Some(node.width());
    }
    match &node.details {
        GreenNodeDetails::Token(_) => Some(TextWidth::default()),
        GreenNodeDetails::Node { children, .. } => {
            for child in children.iter().rev() {
                if let Some(width) = trailing_trivia_width(db, *child) {
                    return Some(width);
                }
            }
            None
        }
    }
}
