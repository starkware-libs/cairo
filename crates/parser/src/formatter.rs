use regex::Regex;
use syntax::node::db::SyntaxGroup;
use syntax::node::kind::SyntaxKind;
use syntax::node::SyntaxNode;
use syntax::token::{self, TokenKind};

#[cfg(test)]
#[path = "formatter_test.rs"]
mod test;

pub struct FormatterConfig {
    tab_size: u32,
    _max_line_length: u32,
}

impl FormatterConfig {
    pub fn new() -> Self {
        Self { tab_size: 4, _max_line_length: 100 }
    }
}

impl Default for FormatterConfig {
    fn default() -> Self {
        Self::new()
    }
}

struct FormattedLineBuffer {
    code_line: String,
    indent_change: i32,
}

struct NodePath {
    kind_path: Vec<SyntaxKind>,
    is_leading_trivia: bool,
}

impl NodePath {
    pub fn new() -> Self {
        Self { kind_path: vec![], is_leading_trivia: false }
    }

    pub fn push(&mut self, kind: SyntaxKind) {
        self.kind_path.push(kind);
    }

    pub fn pop(&mut self) {
        self.kind_path.pop();
    }

    pub fn is_nth_ancestor(&self, n: u32, kind: SyntaxKind) -> bool {
        if n as usize >= self.kind_path.len() {
            false
        } else {
            self.kind_path[self.kind_path.len() - (n as usize) - 1] == kind
        }
    }
}
struct Formatter<'a> {
    db: &'a dyn SyntaxGroup,
    result: String,
    indent_counter: u32,
    config: FormatterConfig,
    line_buffer: FormattedLineBuffer,
    node_path: NodePath,
}

impl<'a> Formatter<'a> {
    pub fn new(db: &'a dyn SyntaxGroup, config: FormatterConfig) -> Self {
        Self {
            db,
            result: String::new(),
            indent_counter: 0,
            config,
            line_buffer: FormattedLineBuffer { code_line: String::new(), indent_change: 0 },
            node_path: NodePath::new(),
        }
    }

    fn format_tree(&mut self, syntax_node: &SyntaxNode) {
        match syntax_node.details(self.db) {
            syntax::node::SyntaxNodeDetails::Token(token) => {
                self.format_token(&token);
            }
            syntax::node::SyntaxNodeDetails::Syntax(kind) => {
                self.format_internal(syntax_node, kind);
            }
        }
    }

    // Should only be called with a syntax_kind syntax_node (i.e. non-token)
    fn format_internal(&mut self, syntax_node: &SyntaxNode, node_kind: SyntaxKind) {
        if syntax_node.ignore(self.db, &self.node_path) {
            return;
        }
        self.node_path.push(node_kind);
        if syntax_node.indent_increasing(self.db, &self.node_path) {
            self.line_buffer.indent_change += 1;
        }
        if syntax_node.whitespace_trimming(self.db, &self.node_path) {
            self.trim_result_end();
        }
        if syntax_node.space_before(self.db, &self.node_path) {
            self.print_space();
        }
        for (i, child) in syntax_node.children(self.db).enumerate() {
            if node_kind == SyntaxKind::Terminal {
                self.node_path.is_leading_trivia = i == 0; // First child of a terminal node
                // is a leading trivia
            }
            self.format_tree(&child);
        }
        if syntax_node.space_after(self.db, &self.node_path) {
            self.print_space();
        }
        if syntax_node.indent_decreasing(self.db, &self.node_path) {
            self.line_buffer.indent_change -= 1;
        }
        if syntax_node.line_breaking(self.db, &self.node_path, false) {
            self.finalize_line();
        }
        self.node_path.pop();
    }

    fn format_token(&mut self, token: &token::Token) {
        if token.ignore(self.db, &self.node_path) {
            return;
        }
        if token.indent_increasing(self.db, &self.node_path) {
            self.line_buffer.indent_change += 1;
        }
        if token.indent_decreasing(self.db, &self.node_path) {
            self.line_buffer.indent_change -= 1;
        }
        if token.whitespace_trimming(self.db, &self.node_path) {
            self.trim_result_end();
        }
        if token.space_before(self.db, &self.node_path) {
            self.print_space();
        }
        self.print_token(token);
        if token.space_after(self.db, &self.node_path) {
            self.print_space();
        }
        if token.line_breaking(self.db, &self.node_path, false) {
            self.finalize_line();
        }
    }

    fn print_token(&mut self, token: &token::Token) {
        self.line_buffer.code_line.push_str(token.text.as_str());
    }

    fn trim_result_end(&mut self) {
        self.line_buffer.code_line = self.line_buffer.code_line.trim_end().to_string();
    }

    fn print_space(&mut self) {
        self.line_buffer.code_line.push(' ');
    }

    fn print_indentation(&mut self) {
        for _ in 0..self.indent_counter {
            self.print_tab();
        }
    }

    fn print_tab(&mut self) {
        self.result.push_str(" ".repeat(self.config.tab_size as usize).as_str());
    }

    fn print_newline(&mut self) {
        self.result.push('\n');
    }

    fn finalize_line(&mut self) {
        if self.line_buffer.code_line.is_empty() {
            return;
        }
        if self.line_buffer.indent_change < 0 {
            if self.line_buffer.indent_change.unsigned_abs() > self.indent_counter {
                panic!("Illegal indentation. Something is wrong with the indentation definitions.");
            } else {
                self.indent_counter -= self.line_buffer.indent_change.unsigned_abs();
            }
        }
        self.print_indentation();
        let re = Regex::new(r"\s+").unwrap();
        let clean_line =
            re.replace_all(self.line_buffer.code_line.as_str(), " ").trim().to_string();
        self.result.push_str(clean_line.as_str());
        self.print_newline();
        if self.line_buffer.indent_change > 0 {
            self.indent_counter += self.line_buffer.indent_change as u32;
        }
        self.line_buffer = FormattedLineBuffer { code_line: String::new(), indent_change: 0 };
    }
}

trait SyntaxNodeFormat {
    fn space_before(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn space_after(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn line_breaking(&self, db: &dyn SyntaxGroup, node_path: &NodePath, hard_break: bool) -> bool;
    fn ignore(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn indent_increasing(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn indent_decreasing(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
    fn whitespace_trimming(&self, db: &dyn SyntaxGroup, node_path: &NodePath) -> bool;
}

// Auto-generated code
impl SyntaxNodeFormat for SyntaxNode {
    fn space_before(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        false
    }
    fn space_after(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(_db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => {
                matches!(kind, SyntaxKind::FunctionSignature)
            }
            syntax::node::SyntaxNodeDetails::Token(_) => {
                panic!("Shouldn't happen")
            }
        }
    }
    fn line_breaking(
        &self,
        _db: &dyn SyntaxGroup,
        _node_path: &NodePath,
        _hard_break: bool,
    ) -> bool {
        match self.details(_db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => matches!(
                kind,
                SyntaxKind::ExprBlock
                    | SyntaxKind::StatementLet
                    | SyntaxKind::StatementExpr
                    | SyntaxKind::StatementReturn
                    | SyntaxKind::ItemList
                    | SyntaxKind::ItemModule
                    | SyntaxKind::ItemFreeFunction
                    | SyntaxKind::ItemTrait
                    | SyntaxKind::ItemImpl
                    | SyntaxKind::ItemStruct
                    | SyntaxKind::ItemEnum
                    | SyntaxKind::ItemUse
            ),
            syntax::node::SyntaxNodeDetails::Token(_) => {
                panic!("Shouldn't happen")
            }
        }
    }
    fn whitespace_trimming(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        false
    }
    fn indent_increasing(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(_db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => matches!(
                kind,
                SyntaxKind::ExprBlock
                    | SyntaxKind::ItemTrait
                    | SyntaxKind::ItemImpl
                    | SyntaxKind::ItemStruct
                    | SyntaxKind::ItemEnum
            ),
            syntax::node::SyntaxNodeDetails::Token(_) => {
                panic!("Shouldn't happen")
            }
        }
    }
    fn indent_decreasing(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(_db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => matches!(
                kind,
                SyntaxKind::ExprBlock
                    | SyntaxKind::ItemTrait
                    | SyntaxKind::ItemImpl
                    | SyntaxKind::ItemStruct
                    | SyntaxKind::ItemEnum
            ),
            syntax::node::SyntaxNodeDetails::Token(_) => {
                panic!("Shouldn't happen")
            }
        }
    }
    fn ignore(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.details(_db) {
            syntax::node::SyntaxNodeDetails::Syntax(kind) => {
                matches!(kind, SyntaxKind::TriviumSkippedTerminal)
            }
            syntax::node::SyntaxNodeDetails::Token(_) => {
                panic!("Shouldn't happen")
            }
        }
    }
}
impl SyntaxNodeFormat for token::Token {
    fn space_before(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::False
            | TokenKind::True
            | TokenKind::And
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Not
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Eq
            | TokenKind::RBrack
            | TokenKind::Arrow => true,
            TokenKind::LBrace => !(_node_path.is_nth_ancestor(1, SyntaxKind::ExprBlock)),
            TokenKind::RBrace => !(_node_path.is_nth_ancestor(1, SyntaxKind::ExprBlock)),
            TokenKind::SingleLineComment => !(_node_path.is_leading_trivia),
            _ => false,
        }
    }
    fn space_after(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        match self.kind {
            TokenKind::False
            | TokenKind::True
            | TokenKind::Function
            | TokenKind::Module
            | TokenKind::Struct
            | TokenKind::Let
            | TokenKind::Return
            | TokenKind::AndAnd
            | TokenKind::OrOr
            | TokenKind::EqEq
            | TokenKind::Neq
            | TokenKind::GE
            | TokenKind::GT
            | TokenKind::LE
            | TokenKind::LT
            | TokenKind::Plus
            | TokenKind::Mul
            | TokenKind::Div
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::Eq
            | TokenKind::LBrace
            | TokenKind::RBrace
            | TokenKind::LBrack
            | TokenKind::Arrow => true,
            TokenKind::Minus => !(_node_path.is_nth_ancestor(1, SyntaxKind::ExprUnary)),
            _ => false,
        }
    }
    fn line_breaking(
        &self,
        _db: &dyn SyntaxGroup,
        _node_path: &NodePath,
        _hard_break: bool,
    ) -> bool {
        match self.kind {
            TokenKind::LBrace => vec![
                _node_path.is_nth_ancestor(1, SyntaxKind::ExprBlock),
                _node_path.is_nth_ancestor(2, SyntaxKind::ItemModule),
                _node_path.is_nth_ancestor(2, SyntaxKind::ItemStruct),
            ]
            .iter()
            .any(|&x| x),
            TokenKind::SingleLineComment => _node_path.is_leading_trivia,
            _ => false,
        }
    }
    fn whitespace_trimming(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        matches!(self.kind, TokenKind::Semicolon)
    }
    fn indent_increasing(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        false
    }
    fn indent_decreasing(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        false
    }
    fn ignore(&self, _db: &dyn SyntaxGroup, _node_path: &NodePath) -> bool {
        matches!(
            self.kind,
            TokenKind::Whitespace
                | TokenKind::Newline
                | TokenKind::Missing
                | TokenKind::BadCharacters
        )
    }
} // End of auto-generated code

pub fn get_formatted_file(
    db: &dyn SyntaxGroup,
    syntax_root: &SyntaxNode,
    config: FormatterConfig,
) -> String {
    let mut formatter = Formatter::new(db, config);
    formatter.format_tree(syntax_root);
    formatter.result
}
