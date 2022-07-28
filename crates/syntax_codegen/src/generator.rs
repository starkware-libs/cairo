use std::fs;
use std::path::PathBuf;

use genco::prelude::*;
use xshell::{cmd, Shell};

use crate::cairo_spec::get_spec;
use crate::spec::{Member, MemberKind, Node, NodeKind};

pub fn project_root() -> PathBuf {
    // This is the directory of Cargo.toml of the syntax_codegen crate.
    let dir = env!("CARGO_MANIFEST_DIR");
    // Pop the "/crates/syntax_codegen" suffix.
    let res = PathBuf::from(dir).parent().unwrap().parent().unwrap().to_owned();
    assert!(res.join("Cargo.toml").exists(), "Could not find project root directory.");
    res
}

pub fn ensure_file_content(filename: PathBuf, content: String) {
    if let Ok(old_contents) = fs::read_to_string(&filename) {
        if old_contents == content {
            return;
        }
    }

    fs::write(&filename, content).unwrap();
}

pub fn get_codes() -> Vec<(String, String)> {
    vec![
        (
            "crates/syntax/src/node/ast.rs".into(),
            reformat_rust_code(generate_ast_code().to_string().unwrap()),
        ),
        (
            "crates/syntax/src/node/kind.rs".into(),
            reformat_rust_code(generate_kinds_code().to_string().unwrap()),
        ),
    ]
}

pub fn reformat_rust_code(text: String) -> String {
    // Since rustfmt is used with nightly features, it takes 2 runs to reach a fixed point.
    reformat_rust_code_inner(reformat_rust_code_inner(text))
}
pub fn reformat_rust_code_inner(text: String) -> String {
    let sh = Shell::new().unwrap();
    sh.set_var("RUSTUP_TOOLCHAIN", "nightly-2022-07-27");
    let rustfmt_toml = project_root().join("rustfmt.toml");
    let mut stdout = cmd!(sh, "rustfmt --config-path {rustfmt_toml}").stdin(text).read().unwrap();
    if !stdout.ends_with('\n') {
        stdout.push('\n');
    }
    stdout
}

fn generate_kinds_code() -> rust::Tokens {
    let spec = get_spec();
    let mut tokens = quote! {
        $("// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.\n")
    };
    let mut kinds = rust::Tokens::new();

    // SyntaxKind.
    for Node { name, kind } in spec.into_iter() {
        match kind {
            NodeKind::Enum { variants: _, missing_variant: _ } => {}
            _ => {
                kinds.extend(quote! {
                    $name,
                });
            }
        }
    }
    tokens.extend(quote! {
        #[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
        pub enum SyntaxKind{
            $kinds
        }
    });
    tokens
}

fn generate_ast_code() -> rust::Tokens {
    let spec = get_spec();
    let mut tokens = quote! {
        $("// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.\n")
        use std::ops::Deref;

        use crate::token::TokenKind;

        use super::kind::SyntaxKind;
        use super::element_list::ElementList;
        use super::green::GreenNodeInternal;
        use super::{TypedSyntaxNode, GreenId, GreenInterner, GreenNode, SyntaxNode, Token};
    };
    for Node { name, kind } in spec.into_iter() {
        tokens.extend(match kind {
            NodeKind::Enum { variants, missing_variant } => {
                gen_enum_code(name, variants, missing_variant)
            }
            NodeKind::Struct { members } => gen_struct_code(name, members),
            NodeKind::List { element_type } => gen_list_code(name, element_type, 1),
            NodeKind::SeparatedList { element_type } => gen_list_code(name, element_type, 2),
        })
    }
    tokens
}

fn gen_list_code(name: String, element_type: String, step: usize) -> rust::Tokens {
    // TODO(spapini): Change Deref to Borrow.
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $(&name)(ElementList<$(&element_type),$step>);
        impl Deref for $(&name){
            type Target = ElementList<$(&element_type),$step>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl $(&name){
            pub fn new_green(db: &dyn GreenInterner, children: Vec<GreenId>) -> GreenId{
                let width = children.iter().map(|id|
                    db.lookup_intern_green(*id).width()).sum();
                db.intern_green(GreenNode::Internal(GreenNodeInternal{
                    kind: SyntaxKind::$(&name),
                    children,
                    width
                }))
            }
        }
        impl TypedSyntaxNode for $(&name) {
            fn missing(db: &dyn GreenInterner) -> GreenId {
                db.intern_green(
                    GreenNode::Internal(GreenNodeInternal {
                        kind: SyntaxKind::$(&name), children: vec![], width: 0
                    })
                )
            }
            fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
                Self(ElementList::new(node))
            }
            fn as_syntax_node(&self) -> SyntaxNode{
                self.node.clone()
            }
        }
    }
}

fn gen_enum_code(
    name: String,
    variants: Vec<Member>,
    missing_variant: Option<String>,
) -> rust::Tokens {
    let mut enum_body = quote! {};
    let mut from_node_body = quote! {};
    let mut from_token_body = quote! {};
    for variant in &variants {
        let v = &variant.name;
        match &variant.kind {
            crate::spec::MemberKind::Token(k) => {
                enum_body.extend(quote! {
                    $v(Token),
                });
                from_token_body.extend(quote! {
                    TokenKind::$k => $(&name)::$v(Token::from_syntax_node(db, node)),
                });
            }
            crate::spec::MemberKind::Node(v) => {
                enum_body.extend(quote! {
                    $v($v),
                });
                from_node_body.extend(quote! {
                    SyntaxKind::$v => $(&name)::$v($v::from_syntax_node(db, node)),
                });
            }
        }
    }
    let missing_body = match missing_variant {
        Some(missing) => quote! {
            db.intern_green(GreenNode::Internal(GreenNodeInternal{
                kind: SyntaxKind::$missing,
                children: vec![],
                width: 0
            }))
        },
        None => quote! {
            panic!("No missing variant.");
        },
    };
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub enum $(&name){
            $enum_body
        }
        impl TypedSyntaxNode for $(&name){
            fn missing(db: &dyn GreenInterner) -> GreenId {
                $missing_body
            }
            fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
                match db.lookup_intern_green(node.0.green) {
                    GreenNode::Internal(internal) => {
                        match internal.kind{
                            $from_node_body
                            _ => panic!(
                                "Unexpected syntax kind {:?} when constructing {}.",
                                internal.kind,
                                $[str]($[const](&name))),
                        }
                    },
                    GreenNode::Token(token) => {
                        match token.kind{
                            $from_token_body
                            _ => panic!(
                                "Unexpected token kind {:?} when constructing {}.",
                                token.kind,
                                $[str]($[const](&name))),
                        }
                    },
                }
            }
            fn as_syntax_node(&self) -> SyntaxNode {
                match self {
                    $(for v in &variants => $(&name)::$(&v.name)(x) => x.as_syntax_node(),)
                }
            }
        }
    }
}

fn gen_struct_code(name: String, members: Vec<Member>) -> rust::Tokens {
    let mut body = rust::Tokens::new();
    let mut args = quote! {};
    let mut params = quote! {};
    let mut arg_missings = quote! {};
    for (i, Member { name, kind }) in members.iter().enumerate() {
        params.extend(quote! {$name: GreenId,});
        args.extend(quote! {$name,});
        // TODO(spapini): Validate that children SyntaxKinds are as expected.
        match kind {
            MemberKind::Token(_) => {
                body.extend(quote! {
                    pub fn $name(&self, db: &dyn GreenInterner) -> Token {
                        let child = self.children[$i].clone();
                        Token::from_syntax_node(db, child)
                    }
                });
                arg_missings.extend(quote! {Token::missing(db),});
            }
            MemberKind::Node(node) => {
                body.extend(quote! {
                    pub fn $name(&self, db: &dyn GreenInterner) -> $node {
                        $node::from_syntax_node(db, self.children[$i].clone())
                    }
                });
                arg_missings.extend(quote! {$node::missing(db),});
            }
        };
    }
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $(&name){
            node: SyntaxNode,
            children: Vec<SyntaxNode>,
        }
        impl $(&name) {
            pub fn new_green(db: &dyn GreenInterner, $params) -> GreenId {
                let children: Vec<GreenId> = vec![$args];
                let width = children.iter().map(|id|
                    db.lookup_intern_green(*id).width()).sum();
                db.intern_green(GreenNode::Internal(GreenNodeInternal {
                    kind: SyntaxKind::$(&name),
                    children,
                    width
                }))
            }
            $body
        }
        impl TypedSyntaxNode for $(&name){
            fn missing(db: &dyn GreenInterner) -> GreenId{
                // Note: A missing syntax element should result in an internal green node
                // of width 0, with as much structure as possible.
                db.intern_green(GreenNode::Internal(GreenNodeInternal {
                    kind: SyntaxKind::$(&name),
                    children: vec![$arg_missings],
                    width: 0
                }))
            }
            fn from_syntax_node(db: &dyn GreenInterner, node: SyntaxNode) -> Self {
                match db.lookup_intern_green(node.0.green) {
                    GreenNode::Internal(internal) => {
                        if internal.kind != SyntaxKind::$(&name) {
                            panic!("Unexpected SyntaxKind {:?}. Expected {:?}.", internal.kind, SyntaxKind::$(&name));
                        }
                        let children = node.children(db);
                        Self{ node, children }
                    },
                    GreenNode::Token(token) => {
                        panic!("Unexpected Token {:?}. Expected {:?}.", token, SyntaxKind::$(&name));
                    }
                }
            }
            fn as_syntax_node(&self) -> SyntaxNode {
                self.node.clone()
            }
        }
    }
}
