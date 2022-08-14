use colored::{ColoredString, Colorize};
use syntax::node::db::GreenInterner;
use syntax::node::green::{GreenNode, GreenNodeInternal};
use syntax::node::ids::GreenId;
use syntax::node::kind::SyntaxKind;
use syntax::token::{self, TokenKind};
use syntax_codegen::cairo_spec::get_spec;
use syntax_codegen::spec::{Member, Node, NodeKind};

struct Printer<'a> {
    db: &'a dyn GreenInterner,
    spec: Vec<Node>,
    print_colors: bool,
    print_trivia: bool,
    result: String,
}
impl<'a> Printer<'a> {
    // Color helpers.
    fn bold(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.bold() } else { text }
    }
    fn green(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.green() } else { text }
    }
    fn red(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.red() } else { text }
    }
    fn cyan(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.cyan() } else { text }
    }
    fn blue(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.blue() } else { text }
    }
    fn bright_purple(&self, text: ColoredString) -> ColoredString {
        if self.print_colors { text.bright_purple() } else { text }
    }

    fn print_token_node(
        &mut self,
        field_description: &str,
        indent: &str,
        extra_head_indent: &str,
        token: token::Token,
    ) {
        let text = if token.kind == TokenKind::Missing {
            format!("{}: {}", self.blue(field_description.into()), self.red("Missing".into()))
        } else {
            let token_text = match token.kind {
                TokenKind::Whitespace | TokenKind::Newline => ".".to_string(),
                _ => format!(": '{}'", self.green(self.bold(token.text.as_str().into()))),
            };
            format!(
                "{} (kind: {:?}){}",
                self.blue(field_description.into()),
                token.kind,
                token_text
            )
        };
        self.result.push_str(format!("{}{}{}\n", indent, extra_head_indent, text).as_str());
    }

    fn print_internal_struct(
        &mut self,
        children: &Vec<GreenId>,
        expected_children: &Vec<Member>,
        kind: SyntaxKind,
        indent: &str,
    ) {
        let num_children = children.len();
        assert_eq!(
            expected_children.len(),
            num_children,
            "Expected {} fields for kind {:?}, Actual fields: {}",
            expected_children.len(),
            kind,
            num_children,
        );

        for (i, child) in children.into_iter().enumerate() {
            self.print_tree(&expected_children[i].name, *child, indent, i == num_children - 1);
        }
    }

    fn print_internal_node(
        &mut self,
        field_description: &str,
        indent: &str,
        extra_head_indent: &str,
        is_last: bool,
        internal_node: GreenNodeInternal,
    ) {
        if internal_node.kind == SyntaxKind::Terminal && !self.print_trivia {
            self.print_tree(field_description, internal_node.children[1], indent, is_last);
            return;
        }

        let extra_info = if is_missing_kind(internal_node.kind) {
            format!(": {}", self.red("Missing".into()))
        } else {
            format!(" (kind: {:?})", internal_node.kind)
        };

        let children = internal_node.children;
        let num_children = children.len();
        let no_children_str = if num_children == 0 {
            self.bright_purple(" []".into()).to_string()
        } else {
            "".to_string()
        };

        self.result.push_str(
            format!(
                "{}{}{}{}{}\n",
                indent,
                extra_head_indent,
                self.cyan(field_description.into()),
                extra_info,
                no_children_str
            )
            .as_str(),
        );

        let extra_indent = if is_last { "    " } else { "│   " };
        let indent = String::from(indent) + extra_indent;
        let kind = self.get_node_kind(internal_node.kind.to_string());
        match kind {
            NodeKind::Struct { members: expected_children } => {
                self.print_internal_struct(
                    &children,
                    &expected_children,
                    internal_node.kind,
                    indent.as_str(),
                );
            }
            NodeKind::List { element_type: _ } => {
                for (i, child) in children.into_iter().enumerate() {
                    self.print_tree(
                        format!("child #{}", i).as_str(),
                        child,
                        indent.as_str(),
                        i == num_children - 1,
                    );
                }
            }
            NodeKind::SeparatedList { element_type: _ } => {
                for (i, child) in children.into_iter().enumerate() {
                    let description = if i % 2 == 0 { "item" } else { "separator" };
                    self.print_tree(
                        format!("{} #{}", description, i / 2).as_str(),
                        child,
                        indent.as_str(),
                        i == num_children - 1,
                    );
                }
            }
            _ => panic!("This should never happen"),
        }
    }

    fn print_tree(
        &mut self,
        field_description: &str,
        green_node: GreenId,
        indent: &str,
        is_last: bool,
    ) {
        let green_node = self.db.lookup_intern_green(green_node);
        let extra_head_indent = if is_last { "└── " } else { "├── " };
        match green_node {
            GreenNode::Token(token) => {
                self.print_token_node(field_description, indent, extra_head_indent, token);
            }
            GreenNode::Internal(internal) => {
                self.print_internal_node(
                    field_description,
                    indent,
                    extra_head_indent,
                    is_last,
                    internal,
                );
            }
        }
    }

    fn get_node_kind(&self, name: String) -> NodeKind {
        if let Some(node) = self.spec.iter().find(|x| x.name == name) {
            node.kind.clone()
        } else {
            panic!("Could not find spec for {}", name)
        }
    }
}

// TODO(yuval): autogenerate.
fn is_missing_kind(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::ExprMissing | SyntaxKind::StatementMissing)
}

pub fn print_tree(
    root: GreenId,
    db: &dyn GreenInterner,
    print_colors: bool,
    print_trivia: bool,
) -> String {
    let mut printer =
        Printer { db, spec: get_spec(), print_colors, print_trivia, result: "".to_string() };
    printer.print_tree("root", root, "", true);
    printer.result
}
