use cairo_lang_syntax as syntax;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax_codegen::cairo_spec::get_spec;
use cairo_lang_syntax_codegen::spec::{Member, Node, NodeKind};
use colored::{ColoredString, Colorize};
use itertools::zip_eq;
use salsa::Database;

pub fn print_tree(
    db: &dyn Database,
    syntax_root: &SyntaxNode<'_>,
    print_colors: bool,
    print_trivia: bool,
) -> String {
    let mut printer = Printer::new(db, print_colors, print_trivia);
    printer.print_tree("root", syntax_root, "", true, true);
    printer.result
}

pub fn print_partial_tree(
    db: &dyn Database,
    syntax_root: &SyntaxNode<'_>,
    top_level_kind: &str,
    ignored_kinds: Vec<&str>,
    print_trivia: bool,
) -> String {
    let mut printer = Printer::new_partial(db, top_level_kind, ignored_kinds, print_trivia);
    let under_top_level = printer.top_level_kind.is_none();
    printer.print_tree("root", syntax_root, "", true, under_top_level);
    printer.result
}

struct Printer<'a> {
    db: &'a dyn Database,
    spec: Vec<Node>,
    print_colors: bool,
    print_trivia: bool,
    /// The highest SyntaxKind that is interesting. All other kinds, if not under it, are ignored.
    /// If None, the whole tree is printed.
    top_level_kind: Option<String>,
    /// Syntax kinds to ignore when printing. In this context, "ignore" means printing the nodes
    /// themselves, but not their children.
    ignored_kinds: Vec<String>,
    result: String,
}
impl<'a> Printer<'a> {
    fn new(db: &'a dyn Database, print_colors: bool, print_trivia: bool) -> Self {
        Self {
            db,
            spec: get_spec(),
            print_colors,
            print_trivia,
            top_level_kind: None,
            ignored_kinds: Vec::new(),
            result: String::new(),
        }
    }

    /// Create a new printer that is capable of partial printing of the syntax tree.
    fn new_partial(
        db: &'a dyn Database,
        top_level_kind: &str,
        ignored_kinds: Vec<&str>,
        print_trivia: bool,
    ) -> Self {
        Self {
            db,
            spec: get_spec(),
            print_colors: false,
            print_trivia,
            top_level_kind: if top_level_kind.trim().is_empty() {
                None
            } else {
                Some(top_level_kind.to_string())
            },
            ignored_kinds: ignored_kinds.into_iter().map(|x| x.to_string()).collect(),
            result: String::new(),
        }
    }

    /// `under_top_level`: whether we are in a subtree of the top-level kind.
    fn print_tree(
        &mut self,
        field_description: &str,
        syntax_node: &SyntaxNode<'_>,
        indent: &str,
        is_last: bool,
        under_top_level: bool,
    ) {
        let extra_head_indent = if is_last { "└── " } else { "├── " };
        let green_node = syntax_node.green_node(self.db);
        match &green_node.details {
            syntax::node::green::GreenNodeDetails::Token(text) => {
                if under_top_level {
                    self.print_token_node(
                        field_description,
                        indent,
                        extra_head_indent,
                        text.long(self.db),
                        green_node.kind,
                    )
                }
            }
            syntax::node::green::GreenNodeDetails::Node { .. } => {
                self.print_internal_node(
                    field_description,
                    indent,
                    extra_head_indent,
                    is_last,
                    syntax_node,
                    green_node.kind,
                    under_top_level,
                );
            }
        }
    }

    fn print_token_node(
        &mut self,
        field_description: &str,
        indent: &str,
        extra_head_indent: &str,
        text: &str,
        kind: SyntaxKind,
    ) {
        let text = if kind == SyntaxKind::TokenMissing {
            format!("{}: {}", self.blue(field_description.into()), self.red("Missing".into()))
        } else {
            let token_text = match kind {
                SyntaxKind::TokenWhitespace
                | SyntaxKind::TokenNewline
                | SyntaxKind::TokenEndOfFile => ".".to_string(),
                _ => format!(": '{}'", self.green(self.bold(text.into()))),
            };
            format!("{} (kind: {:?}){token_text}", self.blue(field_description.into()), kind)
        };
        self.result.push_str(format!("{indent}{extra_head_indent}{text}\n").as_str());
    }

    /// `under_top_level`: whether we are in a subtree of the top-level kind.
    #[allow(clippy::too_many_arguments)]
    fn print_internal_node(
        &mut self,
        field_description: &str,
        indent: &str,
        extra_head_indent: &str,
        is_last: bool,
        syntax_node: &SyntaxNode<'_>,
        kind: SyntaxKind,
        under_top_level: bool,
    ) {
        let current_is_top_level =
            !under_top_level && self.top_level_kind == Some(format!("{kind:?}"));
        // Update under_top_level and indent as needed.
        let (under_top_level, indent) =
            if current_is_top_level { (true, "") } else { (under_top_level, indent) };

        if !self.print_trivia
            && let Some(token_node) = syntax_node.get_terminal_token(self.db)
        {
            self.print_tree(field_description, &token_node, indent, is_last, under_top_level);
            return;
        }

        let extra_info = if is_missing_kind(kind) {
            format!(": {}", self.red("Missing".into()))
        } else {
            format!(" (kind: {kind:?})")
        };

        let children = syntax_node.get_children(self.db);
        let num_children = children.len();
        let suffix = if self.ignored_kinds.contains(&format!("{kind:?}")) {
            " <ignored>".to_string()
        } else if num_children == 0 {
            self.bright_purple(" []".into()).to_string()
        } else {
            String::new()
        };

        // Append to string only if we are under the top level kind.
        if under_top_level {
            if current_is_top_level {
                self.result.push_str(format!("└── Top level kind: {kind:?}{suffix}\n").as_str());
            } else {
                self.result.push_str(
                    format!(
                        "{indent}{extra_head_indent}{}{extra_info}{suffix}\n",
                        self.cyan(field_description.into())
                    )
                    .as_str(),
                );
            }
        }

        if under_top_level && self.ignored_kinds.contains(&format!("{kind:?}")) {
            return;
        }

        if num_children == 0 {
            return;
        }

        let extra_indent = if is_last || current_is_top_level { "    " } else { "│   " };
        let indent = String::from(indent) + extra_indent;
        let node_kind = self.get_node_kind(kind.to_string());
        match node_kind {
            NodeKind::Struct { members: expected_children }
            | NodeKind::Terminal { members: expected_children, .. } => {
                self.print_internal_struct(
                    children,
                    &expected_children,
                    indent.as_str(),
                    under_top_level,
                );
            }
            NodeKind::List { .. } => {
                for (i, child) in children.iter().enumerate() {
                    self.print_tree(
                        format!("child #{i}").as_str(),
                        child,
                        indent.as_str(),
                        i == num_children - 1,
                        under_top_level,
                    );
                }
            }
            NodeKind::SeparatedList { .. } => {
                for (i, child) in children.iter().enumerate() {
                    let description = if i.is_multiple_of(2) { "item" } else { "separator" };
                    self.print_tree(
                        format!("{description} #{}", i / 2).as_str(),
                        child,
                        indent.as_str(),
                        i == num_children - 1,
                        under_top_level,
                    );
                }
            }
            _ => panic!("This should never happen"),
        }
    }

    /// Assumes children and expected children are non-empty of the same length.
    /// `under_top_level`: whether we are in a subtree of the top-level kind.
    fn print_internal_struct(
        &mut self,
        children: &[SyntaxNode<'_>],
        expected_children: &[Member],
        indent: &str,
        under_top_level: bool,
    ) {
        let (last_child, non_last_children) = children.split_last().unwrap();
        let (last_expected_child, non_last_expected_children) =
            expected_children.split_last().unwrap();
        for (child, expected_child) in zip_eq(non_last_children, non_last_expected_children) {
            self.print_tree(&expected_child.name, child, indent, false, under_top_level);
        }
        self.print_tree(&last_expected_child.name, last_child, indent, true, under_top_level);
    }

    fn get_node_kind(&self, name: String) -> NodeKind {
        if let Some(node) = self.spec.iter().find(|x| x.name == name) {
            node.kind.clone()
        } else {
            panic!("Could not find spec for {name}")
        }
    }

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
}

// TODO(yuval): autogenerate.
fn is_missing_kind(kind: SyntaxKind) -> bool {
    matches!(
        kind,
        SyntaxKind::ExprMissing
            | SyntaxKind::WrappedArgListMissing
            | SyntaxKind::StatementMissing
            | SyntaxKind::ModuleItemMissing
            | SyntaxKind::TraitItemMissing
            | SyntaxKind::ImplItemMissing
    )
}
