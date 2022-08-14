use colored::Colorize;
use syntax::node::db::GreenInterner;
use syntax::node::green::GreenNode;
use syntax::node::ids::GreenId;
use syntax::node::kind::SyntaxKind;
use syntax::token::TokenKind;
use syntax_codegen::cairo_spec::get_spec;
use syntax_codegen::spec::{Node, NodeKind};
use termion::{color, style};

struct Printer<'a> {
    db: &'a dyn GreenInterner,
    spec: Vec<Node>,
}
impl<'a> Printer<'a> {
    fn print(&self, green_node: GreenId, print_missing: bool) {
        match self.db.lookup_intern_green(green_node) {
            GreenNode::Token(token) => {
                let color = get_color(token.kind);
                if print_missing && token.kind == TokenKind::Missing {
                    print!("{}<m> {}", color, style::Reset);
                }
                print!("{}{}{}", color, token.text, style::Reset);
            }
            GreenNode::Internal(internal) => {
                for child in internal.children {
                    self.print(child, print_missing);
                }
            }
        }
    }

    fn print_tree(
        &self,
        field_description: &str,
        green_node: GreenId,
        indent: &str,
        is_last: bool,
        print_trivia: bool,
    ) {
        let green_node = self.db.lookup_intern_green(green_node);
        let extra_head_indent = if is_last { "└── " } else { "├── " };
        match green_node {
            GreenNode::Token(token) => {
                let text = if token.kind == TokenKind::Missing {
                    format!("{}: {}", field_description.blue(), "Missing".red())
                } else {
                    let token_text = match token.kind {
                        TokenKind::Whitespace | TokenKind::Newline => ".".to_string(),
                        _ => format!(": '{}'", token.text.green().bold()),
                    };
                    format!("{} (kind: {:?}){}", field_description.blue(), token.kind, token_text)
                };
                println!("{}{}{}", indent, extra_head_indent, text);
            }
            GreenNode::Internal(internal) => {
                if internal.kind() == SyntaxKind::Terminal && !print_trivia {
                    self.print_tree(
                        field_description,
                        internal.child_at(1),
                        indent,
                        is_last,
                        print_trivia,
                    );
                    return;
                }

                let extra_info = if is_missing_kind(internal.kind()) {
                    format!(": {}", "Missing".red())
                } else {
                    format!(" (kind: {:?})", internal.kind())
                };

                let children = internal.children;
                let num_children = children.len();
                let no_children_str = if num_children == 0 {
                    " []".bright_purple().to_string()
                } else {
                    "".to_string()
                };

                println!(
                    "{}{}{}{}{}",
                    indent,
                    extra_head_indent,
                    field_description.cyan(),
                    extra_info,
                    no_children_str
                );

                let extra_indent = if is_last { "    " } else { "│   " };
                let kind = self.get_node_kind(internal.kind().to_string());
                match kind {
                    NodeKind::Struct { members } => {
                        assert_eq!(
                            members.len(),
                            num_children,
                            "Expected {} fields for kind {:?}, Actual fields: {}",
                            members.len(),
                            internal.kind(),
                            num_children,
                        );

                        for (i, child) in children.into_iter().enumerate() {
                            self.print_tree(
                                &members[i].name,
                                child,
                                (String::from(indent) + extra_indent).as_str(),
                                i == num_children - 1,
                                print_trivia,
                            );
                        }
                    }
                    NodeKind::List { element_type: _ } => {
                        for (i, child) in children.into_iter().enumerate() {
                            self.print_tree(
                                format!("child #{}", i).as_str(),
                                child,
                                (String::from(indent) + extra_indent).as_str(),
                                i == num_children - 1,
                                print_trivia,
                            );
                        }
                    }
                    NodeKind::SeparatedList { element_type: _ } => {
                        for (i, child) in children.into_iter().enumerate() {
                            let description = if i % 2 == 0 { "item" } else { "separator" };
                            self.print_tree(
                                format!("{} #{}", description, i / 2).as_str(),
                                child,
                                (String::from(indent) + extra_indent).as_str(),
                                i == num_children - 1,
                                print_trivia,
                            );
                        }
                    }
                    _ => panic!("This should never happen"),
                }
            }
        }
    }

    fn get_node_kind(&self, name: String) -> &NodeKind {
        if let Some(node) = self.spec.iter().find(|x| x.name == name) {
            &node.kind
        } else {
            panic!("Could not find spec for {}", name)
        }
    }
}

// TODO(yuval): autogenerate.
fn is_missing_kind(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::ExprMissing | SyntaxKind::StatementMissing)
}

fn get_color(kind: TokenKind) -> String {
    match kind {
        TokenKind::Identifier => color::Rgb(255, 255, 100).fg_string(),
        TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div | TokenKind::Dot => {
            color::LightMagenta.fg_str().into()
        }
        TokenKind::LiteralNumber | TokenKind::False | TokenKind::True => {
            color::LightCyan.fg_str().into()
        }
        TokenKind::Function | TokenKind::Module | TokenKind::Struct => {
            color::LightBlue.fg_str().into()
        }
        TokenKind::Let | TokenKind::Return => color::LightBlue.fg_str().into(),
        TokenKind::Arrow
        | TokenKind::Colon
        | TokenKind::ColonColon
        | TokenKind::DotDot
        | TokenKind::Semi
        | TokenKind::Underscore
        | TokenKind::And
        | TokenKind::Not => color::Rgb(255, 180, 255).fg_string(),
        TokenKind::Eq
        | TokenKind::EqEq
        | TokenKind::GE
        | TokenKind::GT
        | TokenKind::LE
        | TokenKind::LT
        | TokenKind::Neq => {
            color::Rgb(255, 165, 0).fg_string() // Orange
        }
        TokenKind::AndAnd | TokenKind::OrOr => color::Rgb(255, 165, 0).fg_string(), // Orange
        TokenKind::LBrace
        | TokenKind::RBrace
        | TokenKind::LBrack
        | TokenKind::RBrack
        | TokenKind::LParen
        | TokenKind::RParen
        | TokenKind::Comma => color::White.fg_str().into(),
        TokenKind::EndOfFile => color::White.fg_str().into(),
        TokenKind::BadCharacters => color::Red.fg_str().into(),
        TokenKind::Missing => color::Red.fg_str().into(),
        TokenKind::SingleLineComment | TokenKind::Whitespace | TokenKind::Newline => {
            color::Green.fg_str().into()
        }
    }
}

pub fn print_colored(root: GreenId, db: &dyn GreenInterner) {
    let printer = Printer { db, spec: get_spec() };
    printer.print(root, false);
}
pub fn print_colored_with_missing(root: GreenId, db: &dyn GreenInterner) {
    let printer = Printer { db, spec: get_spec() };
    printer.print(root, true);
}
pub fn print_tree(root: GreenId, db: &dyn GreenInterner, print_trivia: bool) {
    let printer = Printer { db, spec: get_spec() };
    printer.print_tree("root", root, "", true, print_trivia);
}
