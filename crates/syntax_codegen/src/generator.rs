use std::fs;
use std::path::PathBuf;

use genco::prelude::*;
use xshell::{cmd, Shell};

use crate::cairo_spec::get_spec;
use crate::spec::{Member, MemberKind, Node, NodeKind, Variant};

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
        (
            "crates/syntax/src/node/key_fields.rs".into(),
            reformat_rust_code(generate_key_fields_code().to_string().unwrap()),
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
        use core::fmt;
    };
    let mut kinds = rust::Tokens::new();

    // SyntaxKind.
    for Node { name, kind } in spec.into_iter() {
        match kind {
            NodeKind::Enum { .. } => {}
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
        impl fmt::Display for SyntaxKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:?}", self)
            }
        }
    });
    tokens
}

fn generate_key_fields_code() -> rust::Tokens {
    let spec = get_spec();
    let mut arms = rust::Tokens::new();

    for Node { name, kind } in spec.into_iter() {
        match kind {
            NodeKind::Struct { members } => {
                let mut fields = rust::Tokens::new();
                for (i, member) in members.into_iter().enumerate() {
                    let field_name = member.name;
                    if member.key {
                        fields.extend(quote! { $("/*") $field_name $("*/") children[$i] });
                    }
                }
                arms.extend(quote! {
                    SyntaxKind::$name => {vec![$fields]},
                });
            }
            NodeKind::List { .. } | NodeKind::SeparatedList { .. } => {
                arms.extend(quote! {
                    SyntaxKind::$name => {vec![]},
                });
            }
            NodeKind::Enum { .. } => {}
        }
    }
    let tokens = quote! {
        $("// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.\n")
        use super::ids::GreenId;
        use super::kind::SyntaxKind;

        $("/// Gets the vector of children ids that are the indexing key for this SyntaxKind.\n")
        $("/// Each SyntaxKind has some children that are defined in the spec to be its indexing key\n")
        $("/// for its stable pointer. See [super::stable_ptr].\n")
        pub fn get_key_fields(kind: SyntaxKind, children: Vec<GreenId>) -> Vec<GreenId> {
            // TODO(spapini): Implement this.
            match kind {
                $arms
            }
        }
    };
    tokens
}

fn generate_ast_code() -> rust::Tokens {
    let spec = get_spec();
    let mut tokens = quote! {
        $("// Autogenerated file. To regenerate, please run `cargo run --bin generate_syntax`.\n")
        #![allow(clippy::match_single_binding)]
        #![allow(clippy::too_many_arguments)]
        #![allow(dead_code)]
        #![allow(unused_variables)]
        use std::ops::Deref;

        use crate::token::TokenKind;

        use super::kind::SyntaxKind;
        use super::element_list::ElementList;
        use super::green::GreenNodeInternal;
        use super::{
            TypedSyntaxNode, GreenId, SyntaxGroup, GreenNode, SyntaxNode, SyntaxStablePtrId,
            SyntaxStablePtr, Token, TokenGreen,
        };
    };
    for Node { name, kind } in spec.into_iter() {
        tokens.extend(match kind {
            NodeKind::Enum { variants, missing_variant } => {
                gen_enum_code(name, variants, missing_variant)
            }
            NodeKind::Struct { members } => gen_struct_code(name, members),
            NodeKind::List { element_type } => gen_list_code(name, element_type),
            NodeKind::SeparatedList { element_type } => gen_separated_list_code(name, element_type),
        })
    }
    tokens
}

fn gen_list_code(name: String, element_type: String) -> rust::Tokens {
    // TODO(spapini): Change Deref to Borrow.
    let ptr_name = format!("{name}Ptr");
    let green_name = format!("{name}Green");
    let element_green_name = format!("{element_type}Green");
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $(&name)(ElementList<$(&element_type),1>);
        impl Deref for $(&name){
            type Target = ElementList<$(&element_type),1>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl $(&name){
            pub fn new_green(
                db: &dyn SyntaxGroup, children: Vec<$(&element_green_name)>
            ) -> $(&green_name) {
                let width = children.iter().map(|id|
                    db.lookup_intern_green(id.0).width()).sum();
                $(&green_name)(db.intern_green(GreenNode::Internal(GreenNodeInternal{
                    kind: SyntaxKind::$(&name),
                    children: children.iter().map(|x| x.0).collect(),
                    width
                })))
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&ptr_name)(SyntaxStablePtrId);
        impl $(&ptr_name) {
            pub fn untyped(&self) -> SyntaxStablePtrId {
                self.0
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&green_name)(pub GreenId);
        impl TypedSyntaxNode for $(&name) {
            type StablePtr = $(&ptr_name);
            type Green = $(&green_name);
            fn missing(db: &dyn SyntaxGroup) -> Self::Green {
                $(&green_name)(db.intern_green(
                    GreenNode::Internal(GreenNodeInternal {
                        kind: SyntaxKind::$(&name), children: vec![], width: 0
                    })
                ))
            }
            fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
                Self(ElementList::new(node))
            }
            fn as_syntax_node(&self) -> SyntaxNode{
                self.node.clone()
            }
            fn from_ptr(db: &dyn SyntaxGroup, root: &SyntaxFile, ptr: Self::StablePtr) -> Self {
                Self::from_syntax_node(db, root.as_syntax_node().lookup_ptr(db, ptr.0))
            }
            fn stable_ptr(&self) -> Self::StablePtr {
                $(&ptr_name)(self.node.0.stable_ptr)
            }
        }
    }
}

fn gen_separated_list_code(name: String, element_type: String) -> rust::Tokens {
    // TODO(spapini): Change Deref to Borrow.
    let ptr_name = format!("{name}Ptr");
    let green_name = format!("{name}Green");
    let single_green_name = format!("{name}SingleGreen");
    let element_green_name = format!("{element_type}Green");
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $(&name)(ElementList<$(&element_type),2>);
        impl Deref for $(&name){
            type Target = ElementList<$(&element_type),2>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }
        impl $(&name){
            pub fn new_green(
                db: &dyn SyntaxGroup, children: Vec<$(&single_green_name)>
            ) -> $(&green_name) {
                let width = children.iter().map(|id|
                    db.lookup_intern_green(id.id()).width()).sum();
                $(&green_name)(db.intern_green(GreenNode::Internal(GreenNodeInternal{
                    kind: SyntaxKind::$(&name),
                    children: children.iter().map(|x| x.id()).collect(),
                    width
                })))
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&ptr_name)(SyntaxStablePtrId);
        impl $(&ptr_name) {
            pub fn untyped(&self) -> SyntaxStablePtrId {
                self.0
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub enum $(&single_green_name) {
            Separator(TerminalGreen),
            Element($(&element_green_name)),
        }
        impl From<TerminalGreen> for $(&single_green_name) {
            fn from(value: TerminalGreen) -> Self {
                $(&single_green_name)::Separator(value)
            }
        }
        impl From<$(&element_green_name)> for $(&single_green_name) {
            fn from(value: $(&element_green_name)) -> Self {
                $(&single_green_name)::Element(value)
            }
        }
        impl $(&single_green_name) {
            fn id(&self) -> GreenId {
                match self {
                    $(&single_green_name)::Separator(green) => green.0,
                    $(&single_green_name)::Element(green) => green.0,
                }
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&green_name)(pub GreenId);
        impl TypedSyntaxNode for $(&name) {
            type StablePtr = $(&ptr_name);
            type Green = $(&green_name);
            fn missing(db: &dyn SyntaxGroup) -> Self::Green {
                $(&green_name)(db.intern_green(
                    GreenNode::Internal(GreenNodeInternal {
                        kind: SyntaxKind::$(&name), children: vec![], width: 0
                    })
                ))
            }
            fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
                Self(ElementList::new(node))
            }
            fn as_syntax_node(&self) -> SyntaxNode{
                self.node.clone()
            }
            fn from_ptr(db: &dyn SyntaxGroup, root: &SyntaxFile, ptr: Self::StablePtr) -> Self {
                Self::from_syntax_node(db, root.as_syntax_node().lookup_ptr(db, ptr.0))
            }
            fn stable_ptr(&self) -> Self::StablePtr {
                $(&ptr_name)(self.node.0.stable_ptr)
            }
        }
    }
}

fn gen_enum_code(
    name: String,
    variants: Vec<Variant>,
    missing_variant: Option<String>,
) -> rust::Tokens {
    let ptr_name = format!("{name}Ptr");
    let green_name = format!("{name}Green");
    let mut enum_body = quote! {};
    let mut from_node_body = quote! {};
    let mut from_token_body = quote! {};
    let mut green_conversions = quote! {};
    let mut has_token = false;
    for variant in &variants {
        let n = &variant.name;
        match &variant.kind {
            crate::spec::MemberKind::Token(k) => {
                has_token = true;
                enum_body.extend(quote! {
                    $n(Token),
                });
                from_token_body.extend(quote! {
                    TokenKind::$k => $(&name)::$n(Token::from_syntax_node(db, node)),
                });
            }
            crate::spec::MemberKind::Node(k) => {
                enum_body.extend(quote! {
                    $n($k),
                });
                from_node_body.extend(quote! {
                    SyntaxKind::$k => $(&name)::$n($k::from_syntax_node(db, node)),
                });
                let child_green = format!("{k}Green");
                green_conversions.extend(quote! {
                    impl From<$(&child_green)> for $(&green_name) {
                        fn from(value: $(&child_green)) -> Self {
                            Self(value.0)
                        }
                    }
                });
            }
        }
    }
    if has_token {
        green_conversions.extend(quote! {
            impl From<TokenGreen> for $(&green_name) {
                fn from(value: TokenGreen) -> Self {
                    Self(value.0)
                }
            }
        });
    }
    let missing_body = match missing_variant {
        Some(missing) => quote! {
            $(&green_name)(db.intern_green(GreenNode::Internal(GreenNodeInternal{
                kind: SyntaxKind::$missing,
                children: vec![],
                width: 0
            })))
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
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&ptr_name)(SyntaxStablePtrId);
        impl $(&ptr_name) {
            pub fn untyped(&self) -> SyntaxStablePtrId {
                self.0
            }
        }
        $green_conversions
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&green_name)(pub GreenId);
        impl TypedSyntaxNode for $(&name){
            type StablePtr = $(&ptr_name);
            type Green = $(&green_name);
            fn missing(db: &dyn SyntaxGroup) -> Self::Green {
                $missing_body
            }
            fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
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
            fn from_ptr(db: &dyn SyntaxGroup, root: &SyntaxFile, ptr: Self::StablePtr) -> Self {
                Self::from_syntax_node(db, root.as_syntax_node().lookup_ptr(db, ptr.0))
            }
            fn stable_ptr(&self) -> Self::StablePtr {
                $(&ptr_name)(self.as_syntax_node().0.stable_ptr)
            }
        }
    }
}

fn gen_struct_code(name: String, members: Vec<Member>) -> rust::Tokens {
    let green_name = format!("{name}Green");
    let mut body = rust::Tokens::new();
    let mut args = quote! {};
    let mut params = quote! {};
    let mut arg_missings = quote! {};
    let mut ptr_getters = quote! {};
    let mut key_field_index: usize = 0;
    for (i, Member { name, kind, key }) in members.iter().enumerate() {
        let key_name_green = format!("{name}_green");
        args.extend(quote! {$name.0,});
        // TODO(spapini): Validate that children SyntaxKinds are as expected.
        let child_green = match kind {
            MemberKind::Token(_) => {
                let child_green = "TokenGreen".to_string();
                params.extend(quote! {$name: TokenGreen,});
                body.extend(quote! {
                    pub fn $name(&self, db: &dyn SyntaxGroup) -> Token {
                        let child = self.children[$i].clone();
                        Token::from_syntax_node(db, child)
                    }
                });
                arg_missings.extend(quote! {Token::missing(db).0,});
                child_green
            }
            MemberKind::Node(node) => {
                let child_green = format!("{node}Green");
                params.extend(quote! {$name: $(&child_green),});
                body.extend(quote! {
                    pub fn $name(&self, db: &dyn SyntaxGroup) -> $node {
                        $node::from_syntax_node(db, self.children[$i].clone())
                    }
                });
                arg_missings.extend(quote! {$node::missing(db).0,});
                child_green
            }
        };
        if *key {
            ptr_getters.extend(quote! {
                pub fn $(&key_name_green)(self, db: &dyn SyntaxGroup) -> $(&child_green) {
                    let ptr = db.lookup_intern_stable_ptr(self.0);
                    if let SyntaxStablePtr::Child { key_fields, .. } = ptr {
                        $(&child_green)(key_fields[$key_field_index])
                    } else {
                        panic!("Unexpected key field query on root.");
                    }
                }
            });
            key_field_index += 1;
        }
    }
    let ptr_name = format!("{name}Ptr");
    quote! {
        #[derive(Clone, Debug, Eq, Hash, PartialEq)]
        pub struct $(&name){
            node: SyntaxNode,
            children: Vec<SyntaxNode>,
        }
        impl $(&name) {
            pub fn new_green(db: &dyn SyntaxGroup, $params) -> $(&green_name) {
                let children: Vec<GreenId> = vec![$args];
                let width = children.iter().copied().map(|id|
                    db.lookup_intern_green(id).width()).sum();
                $(&green_name)(db.intern_green(GreenNode::Internal(GreenNodeInternal {
                    kind: SyntaxKind::$(&name),
                    children,
                    width
                })))
            }
            $body
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&ptr_name)(SyntaxStablePtrId);
        impl $(&ptr_name) {
            $ptr_getters

            pub fn untyped(&self) -> SyntaxStablePtrId {
                self.0
            }
        }
        #[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
        pub struct $(&green_name)(pub GreenId);
        impl TypedSyntaxNode for $(&name){
            type StablePtr = $(&ptr_name);
            type Green = $(&green_name);
            fn missing(db: &dyn SyntaxGroup) -> Self::Green {
                // Note: A missing syntax element should result in an internal green node
                // of width 0, with as much structure as possible.
                $(&green_name)(db.intern_green(GreenNode::Internal(GreenNodeInternal {
                    kind: SyntaxKind::$(&name),
                    children: vec![$arg_missings],
                    width: 0
                })))
            }
            fn from_syntax_node(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
                match db.lookup_intern_green(node.0.green) {
                    GreenNode::Internal(internal) => {
                        if internal.kind != SyntaxKind::$(&name) {
                            panic!(
                                "Unexpected SyntaxKind {:?}. Expected {:?}.",
                                internal.kind,
                                SyntaxKind::$(&name),
                            );
                        }
                        let children = node.children(db).collect();
                        Self { node, children }
                    },
                    GreenNode::Token(token) => {
                        panic!(
                            "Unexpected Token {:?}. Expected {:?}.",
                            token,
                            SyntaxKind::$(&name),
                        );
                    }
                }
            }
            fn from_ptr(db: &dyn SyntaxGroup, root: &SyntaxFile, ptr: Self::StablePtr) -> Self {
                Self::from_syntax_node(db, root.as_syntax_node().lookup_ptr(db, ptr.0))
            }
            fn as_syntax_node(&self) -> SyntaxNode {
                self.node.clone()
            }
            fn stable_ptr(&self) -> Self::StablePtr {
                $(&ptr_name)(self.node.0.stable_ptr)
            }
        }
    }
}
