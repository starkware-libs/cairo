use std::fmt::Write;
use std::path::Path;

use indoc::indoc;
use tower_lsp::lsp_types;
use tower_lsp::lsp_types::lsp_request;

use crate::support::{cursors, sandbox};

fn caps(base: lsp_types::ClientCapabilities) -> lsp_types::ClientCapabilities {
    use lsp_types::*;
    ClientCapabilities {
        text_document: base.text_document.or_else(Default::default).map(|it| {
            TextDocumentClientCapabilities {
                hover: Some(HoverClientCapabilities {
                    dynamic_registration: Some(false),
                    content_format: Some(vec![MarkupKind::Markdown, MarkupKind::PlainText]),
                }),
                ..it
            }
        }),
        ..base
    }
}

#[test]
fn basic() {
    check(
        "tests/test_data/hover/basic.txt",
        indoc! {r#"
            fn main() {
                let mut x = 5;
                p<caret>rintln!("The value of x is: {}", x);
                x<caret> = <caret>a<caret>dd_two<caret>(x);

                front<caret>_of_house::ho<caret>sting::add<caret>_to_waitlist();
            }

            /// `add_two` documentation.
            fn add_t<caret>wo(x: u32) -> u32 { x + 2 }

            /// Rectangle struct.
            #[derive(Copy, Drop)]
            struct Rectangle {
                /// Width of the rectangle.
                width: u64,
                /// Height of the rectangle.
                height: u64,
            }

            /// Rectangle trait.
            trait RectangleTrait {
                /// Calculate the area of the rectangle.
                fn area(self: @Rec<caret>tangle) -> u64;
            }

            /// Implementing the `RectangleTrait` for the `Rectangle` struct.
            impl RectangleImpl of RectangleTrait {
                fn area(self: @Rec<caret>tangle) -> u64 {
                    (*self.wi<caret>dth) * (*self.height)
                }
            }

            /// Testing `#[generate_trait]` attribute.
            #[generate_trait]
            impl RectangleImpl2 of RectangleTrait2 {
                /// Calculate the area of the rectangle #2.
                fn area(self: @Rec<caret>tangle) -> u64 {
                    (*self.wi<caret>dth) * (*self.height)
                }
            }

            enum Coin {
                Penny,
            }

            fn value_in_cents(coin: C<caret>oin) -> felt252 {
                match coin {
                    Coin::P<caret>enny => 1,
                }
            }

            /// Front of house module.
            pub mod front_of_house {
                /// Hosting module.
                pub mod hosting {
                    /// Add to waitlist function.
                    pub fn add_to_waitlist() {}
                }
            }
        "#},
    );
}

#[test]
fn starknet() {
    check(
        "tests/test_data/hover/starknet.txt",
        indoc! {r#"
            use Balance::contr<caret>act_state_for_testing;

            /// The balance contract interface.
            #[starknet::interface]
            trait IBalance<T> {
                /// Returns the current balance.
                fn get(self: @T) -> u128;
                /// Increases the balance by the given amount.
                fn increase(ref self: T, a: u128);
            }

            /// The balance contract.
            #[starknet::contract]
            mod Balance {
                use core::traits::Into;

                #[storage]
                struct Storage {
                    /// Storage value.
                    value: u128,
                }

                #[constructor]
                fn constructor(ref se<caret>lf: Cont<caret>ractState, value_: u128) {
                    self.va<caret>lue.write(value_);
                }

                #[abi(embed_v0)]
                impl Balance of super::IBa<caret>lance<Con<caret>tractState> {
                    fn get(self: @ContractState) -> u128 {
                        self.value.r<caret>ead()
                    }
                    fn increase(ref self: ContractState, a: u128)  {
                        self.value.wr<caret>ite( self.value.read() + a );
                    }
                }
            }
        "#},
    );
}

/// Perform hover test.
///
/// This function spawns a sandbox language server with the given code in the `src/lib.cairo` file.
/// The Cairo source code is expected to contain caret markers.
/// The function then requests hover information at each caret position and compares the result with
/// the expected hover information from the snapshot file.
fn check(test_data: &str, cairo: &str) {
    let (cairo, cursors) = cursors(cairo);

    let mut ls = sandbox! {
        files {
            "cairo_project.toml" => indoc! {r#"
                [crate_roots]
                hello = "src"

                [config.global]
                edition = "2023_11"
            "#},
            "src/lib.cairo" => cairo.clone(),
        }
        client_capabilities = caps;
    };

    ls.open("src/lib.cairo");

    let mut expected = String::new();

    for position in cursors.carets() {
        writeln!(&mut expected, "//! > hover at {position:?}").unwrap();

        writeln!(&mut expected).unwrap();
        writeln!(&mut expected, "//! > source context").unwrap();
        let source_line = cairo.lines().nth(position.line as usize).unwrap();
        writeln!(&mut expected, "{source_line}").unwrap();
        writeln!(
            &mut expected,
            "{caret:>width$}",
            caret = "↑",
            width = position.character as usize + 1
        )
        .unwrap();

        let hover = ls.send_request::<lsp_request!("textDocument/hover")>(lsp_types::HoverParams {
            text_document_position_params: lsp_types::TextDocumentPositionParams {
                text_document: ls.doc_id("src/lib.cairo"),
                position,
            },
            work_done_progress_params: Default::default(),
        });

        writeln!(&mut expected).unwrap();
        writeln!(&mut expected, "//! > popover").unwrap();
        match hover {
            Some(hover) => expected.push_str(&render(hover)),
            None => expected.push_str("No hover information.\n"),
        }

        writeln!(&mut expected, "\n=========\n").unwrap();
    }

    cairo_lang_test_utils::compare_contents_or_fix_with_path(Path::new(test_data), expected);
}

/// Render a hover response to a Markdown string that resembles what would be shown in a hover popup
/// in the text editor.
///
/// Any additional hover metadata is rendered as HTML comments at the beginning of the output.
fn render(h: lsp_types::Hover) -> String {
    use lsp_types::*;
    let mut buf = String::new();

    if let Some(range) = h.range {
        writeln!(&mut buf, "<!-- range: {range:?} -->").unwrap();
    }

    let mut write_marked_string = |content| match content {
        MarkedString::String(contents) => buf.push_str(&contents),
        MarkedString::LanguageString(LanguageString { language, value }) => {
            write!(&mut buf, "```{language}\n{value}\n```").unwrap();
        }
    };

    match h.contents {
        HoverContents::Scalar(content) => write_marked_string(content),
        HoverContents::Markup(MarkupContent { value, .. }) => {
            buf.push_str(&value);
        }
        HoverContents::Array(contents) => {
            for content in contents {
                write_marked_string(content);
            }
        }
    }

    buf
}
