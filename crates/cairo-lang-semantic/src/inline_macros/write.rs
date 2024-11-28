use std::fmt;

use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, MacroPluginMetadata, NamedPlugin, PluginDiagnostic,
    PluginGeneratedFile,
};
use cairo_lang_defs::plugin_utils::{try_extract_unnamed_arg, unsupported_bracket_diagnostic};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{TypedStablePtr, TypedSyntaxNode, ast};
use cairo_lang_utils::{OptionHelper, try_extract_matches};
use indoc::indoc;
use num_bigint::{BigInt, Sign};

pub const FELT252_BYTES: usize = 31;

/// Macro for writing into a formatter.
#[derive(Debug, Default)]
pub struct WriteMacro;
impl NamedPlugin for WriteMacro {
    const NAME: &'static str = "write";
}
impl InlineMacroExprPlugin for WriteMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, false)
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Writes formatted data into a formatter.

            This macro accepts a `formatter`, a format string, and a list of arguments. \
            Arguments will be formatted according to the specified format string and the result \
            will be passed to the formatter. The formatter is of the type `core::fmt::Formatter`. \
            The macro returns `Result<(), core::fmt::Error>`.

            # Panics
            Panics if any of the formatting of arguments fails.

            # Examples
            ```cairo
            let f: core::fmt::Formatter = Default::default();
            write!(f, "hello"); // `f` contains "hello".
            let world: ByteArray = "world";
            write!(f, "hello {}", world_ba); // `f` contains "hellohello world".
            write!(f, "hello {world_ba}"); // `f` contains "hellohello worldhello world".
            let (x, y) = (1, 2);
            write!(f, "{x} + {y} = 3");  // `f` contains "hellohello worldhello world1 + 2 = 3".
            ```
        "#}
            .to_string(),
        )
    }
}

/// Macro for writing into a formatter with an additional new line.
#[derive(Debug, Default)]
pub struct WritelnMacro;
impl NamedPlugin for WritelnMacro {
    const NAME: &'static str = "writeln";
}
impl InlineMacroExprPlugin for WritelnMacro {
    fn generate_code(
        &self,
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
        _metadata: &MacroPluginMetadata<'_>,
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, true)
    }

    fn documentation(&self) -> Option<String> {
        Some(
            indoc! {r#"
            Writes formatted data into a formatter, with an additional newline.

            This macro accepts a `formatter`, a format string, and a list of arguments. \
            Arguments will be formatted according to the specified format string and the result \
            will be passed to the formatter. The formatter is of the type `core::fmt::Formatter`. \
            The macro returns `Result<(), core::fmt::Error>`.

            # Panics
            Panics if any of the formatting of arguments fails.

            # Examples
            ```cairo
            let f: core::fmt::Formatter = Default::default();
            writeln!(f, "hello"); // `f` contains "hello\n".
            let world: ByteArray = "world";
            writeln!(f, "hello {}", world_ba); // `f` contains "hello\nhello world\n".
            writeln!(f, "hello {world_ba}"); // `f` contains "hello\nhello world\nhello world\n".
            let (x, y) = (1, 2);
            writeln!(f, "{x}+{y}=3"); // `f` contains "hello\nhello world\nhello world\n1+2=3\n".
            ```
        "#}
            .to_string(),
        )
    }
}

fn generate_code_inner(
    syntax: &ast::ExprInlineMacro,
    db: &dyn SyntaxGroup,
    with_newline: bool,
) -> InlinePluginResult {
    let info = match FormattingInfo::extract(db, syntax) {
        Ok(info) => info,
        Err(diagnostics) => return InlinePluginResult { code: None, diagnostics },
    };
    let mut builder = PatchBuilder::new(db, syntax);
    let mut diagnostics = vec![];
    info.add_to_formatter(&mut builder, &mut diagnostics, with_newline);
    if !diagnostics.is_empty() {
        return InlinePluginResult { code: None, diagnostics };
    }
    let (content, code_mappings) = builder.build();
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: format!("{}_macro", get_macro_name(with_newline)).into(),
            content,
            code_mappings,
            aux_data: None,
            diagnostics_note: Default::default(),
        }),
        diagnostics: vec![],
    }
}

/// Gets the macro name according to the `with_newline` flag.
fn get_macro_name(with_newline: bool) -> &'static str {
    if with_newline { WritelnMacro::NAME } else { WriteMacro::NAME }
}

/// Information about a formatting a string for the write macros.
struct FormattingInfo {
    /// The syntax rewrite node for the formatter input for the macro.
    formatter_arg_node: RewriteNode,
    /// The format string argument.
    format_string_arg: ast::Arg,
    /// The format string content.
    format_string: String,
    /// The positional arguments for the format string.
    args: Vec<ast::Expr>,
}
impl FormattingInfo {
    /// Extracts the arguments from a formatted string macro.
    fn extract(
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> Result<FormattingInfo, Vec<PluginDiagnostic>> {
        let ast::WrappedArgList::ParenthesizedArgList(arguments) = syntax.arguments(db) else {
            return Err(unsupported_bracket_diagnostic(db, syntax).diagnostics);
        };
        let argument_list_elements = arguments.arguments(db).elements(db);
        let mut args_iter = argument_list_elements.iter();
        let Some(formatter_arg) = args_iter.next() else {
            return Err(vec![PluginDiagnostic::error(
                arguments.lparen(db).stable_ptr().untyped(),
                "Macro expected formatter argument.".to_string(),
            )]);
        };
        let Some(formatter_expr) = try_extract_unnamed_arg(db, formatter_arg) else {
            return Err(vec![PluginDiagnostic::error(
                formatter_arg.stable_ptr().untyped(),
                "Formatter argument must unnamed.".to_string(),
            )]);
        };
        if matches!(formatter_expr, ast::Expr::String(_)) {
            return Err(vec![PluginDiagnostic::error(
                formatter_arg.stable_ptr().untyped(),
                "Formatter argument must not be a string literal.".to_string(),
            )]);
        }
        let Some(format_string_arg) = args_iter.next() else {
            return Err(vec![PluginDiagnostic::error(
                arguments.lparen(db).stable_ptr().untyped(),
                "Macro expected format string argument.".to_string(),
            )]);
        };
        let Some(format_string_expr) = try_extract_unnamed_arg(db, format_string_arg) else {
            return Err(vec![PluginDiagnostic::error(
                format_string_arg.stable_ptr().untyped(),
                "Format string argument must be unnamed.".to_string(),
            )]);
        };
        let Some(format_string) = try_extract_matches!(format_string_expr, ast::Expr::String)
            .and_then(|arg| arg.string_value(db))
        else {
            return Err(vec![PluginDiagnostic::error(
                format_string_arg.stable_ptr().untyped(),
                "Format string argument must be a string literal.".to_string(),
            )]);
        };
        let mut diagnostics = vec![];
        let args: Vec<_> = args_iter
            .filter_map(|arg| {
                try_extract_unnamed_arg(db, arg).on_none(|| {
                    diagnostics.push(PluginDiagnostic::error(
                        arg.stable_ptr().untyped(),
                        "Expected unnamed argument.".to_string(),
                    ))
                })
            })
            .collect();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(FormattingInfo {
            formatter_arg_node: RewriteNode::from_ast_trimmed(formatter_arg),
            format_string_arg: format_string_arg.clone(),
            // `unwrap` is ok because the above `on_none` ensures it's not None.
            format_string,
            args,
        })
    }

    /// Adds the formatted string from macro to the formatter.
    fn add_to_formatter(
        &self,
        builder: &mut PatchBuilder<'_>,
        diagnostics: &mut Vec<PluginDiagnostic>,
        with_newline: bool,
    ) {
        let mut next_arg_index = 0..self.args.len();
        let mut arg_used = vec![false; self.args.len()];
        let mut format_iter = self.format_string.chars().enumerate().peekable();
        let mut pending_chars = String::new();
        let mut ident_count = 1;
        let mut missing_args = 0;
        let format_string_base = self
            .format_string_arg
            .as_syntax_node()
            .span_start_without_trivia(builder.db)
            .add_width(TextWidth::from_char('"'));
        builder.add_str("{\n");
        for (i, arg) in self.args.iter().enumerate() {
            self.add_indentation(builder, ident_count);
            builder.add_modified(RewriteNode::interpolate_patched(
                &format!("let __write_macro_arg{i}__ = @($arg$);\n"),
                &[("arg".to_string(), RewriteNode::from_ast_trimmed(arg))].into(),
            ));
        }
        while let Some((idx, c)) = format_iter.next() {
            if c == '{' {
                if matches!(format_iter.peek(), Some(&(_, '{'))) {
                    pending_chars.push('{');
                    format_iter.next();
                    continue;
                }
                let argument_info = match extract_placeholder_argument(&mut format_iter) {
                    Ok(argument_info) => argument_info,
                    Err(error_message) => {
                        diagnostics.push(PluginDiagnostic::error(
                            self.format_string_arg.as_syntax_node().stable_ptr(),
                            format!("Invalid format string: {error_message}."),
                        ));
                        return;
                    }
                };
                match argument_info.source {
                    PlaceholderArgumentSource::Positional(positional) => {
                        let Some(arg) = self.args.get(positional) else {
                            diagnostics.push(PluginDiagnostic::error(
                                self.format_string_arg.as_syntax_node().stable_ptr(),
                                format!(
                                    "Invalid reference to positional argument {positional} (there \
                                     are {} arguments).",
                                    self.args.len()
                                ),
                            ));
                            return;
                        };
                        arg_used[positional] = true;
                        self.append_formatted_arg(
                            builder,
                            &mut ident_count,
                            &mut pending_chars,
                            RewriteNode::mapped_text(
                                format!("__write_macro_arg{positional}__"),
                                builder.db,
                                arg,
                            ),
                            argument_info.formatting_trait,
                        );
                    }
                    PlaceholderArgumentSource::Next => {
                        if let Some(i) = next_arg_index.next() {
                            arg_used[i] = true;
                            self.append_formatted_arg(
                                builder,
                                &mut ident_count,
                                &mut pending_chars,
                                RewriteNode::mapped_text(
                                    format!("__write_macro_arg{i}__"),
                                    builder.db,
                                    &self.args[i],
                                ),
                                argument_info.formatting_trait,
                            );
                        } else {
                            missing_args += 1;
                        }
                    }
                    PlaceholderArgumentSource::Named(argument) => {
                        let start = format_string_base
                            .add_width(TextWidth::from_str(&self.format_string[..(idx + 1)]));
                        let end = start.add_width(TextWidth::from_str(&argument));
                        self.append_formatted_arg(
                            builder,
                            &mut ident_count,
                            &mut pending_chars,
                            RewriteNode::new_modified(vec![
                                RewriteNode::text("@"),
                                RewriteNode::Mapped {
                                    origin: TextSpan { start, end },
                                    node: RewriteNode::text(&argument).into(),
                                },
                            ]),
                            argument_info.formatting_trait,
                        );
                    }
                }
            } else if c == '}' {
                if matches!(format_iter.peek(), Some(&(_, '}'))) {
                    pending_chars.push('}');
                    format_iter.next();
                } else {
                    diagnostics.push(PluginDiagnostic::error(
                        self.format_string_arg.as_syntax_node().stable_ptr(),
                        "Closing `}` without a matching `{`.".to_string(),
                    ));
                }
            } else {
                pending_chars.push(c);
            }
        }
        if missing_args > 0 {
            diagnostics.push(PluginDiagnostic::error(
                self.format_string_arg.as_syntax_node().stable_ptr(),
                format!(
                    "{} positional arguments in format string, but only {} arguments.",
                    self.args.len() + missing_args,
                    self.args.len()
                ),
            ));
            return;
        }
        if with_newline {
            pending_chars.push('\n');
        }

        self.flush_pending_chars(builder, &mut pending_chars, ident_count);
        self.add_indentation(builder, ident_count);
        builder.add_str("core::result::Result::<(), core::fmt::Error>::Ok(())\n");
        while ident_count > 1 {
            ident_count -= 1;
            self.add_indentation(builder, ident_count);
            builder.add_str("},\n");
            self.add_indentation(builder, ident_count);
            builder.add_str("core::result::Result::Err(err) => core::result::Result::Err(err),\n");
            ident_count -= 1;
            self.add_indentation(builder, ident_count);
            builder.add_str("}\n");
        }
        builder.add_str("}\n");
        for (position, used) in arg_used.into_iter().enumerate() {
            if !used {
                diagnostics.push(PluginDiagnostic::error(
                    self.args[position].as_syntax_node().stable_ptr(),
                    "Unused argument.".to_string(),
                ));
            }
        }
    }

    fn add_indentation(&self, builder: &mut PatchBuilder<'_>, count: usize) {
        for _ in 0..count {
            builder.add_str("    ");
        }
    }

    /// Appends a formatted argument to the formatter, flushing the pending bytes if necessary.
    /// This includes opening a new match, which is only closed at the end of the macro handling.
    fn append_formatted_arg(
        &self,
        builder: &mut PatchBuilder<'_>,
        ident_count: &mut usize,
        pending_chars: &mut String,
        arg: RewriteNode,
        fmt_type: FormattingTrait,
    ) {
        self.flush_pending_chars(builder, pending_chars, *ident_count);
        self.add_indentation(builder, *ident_count);
        builder.add_modified(RewriteNode::interpolate_patched(
            &format!("match core::fmt::{fmt_type}::fmt($arg$, ref $f$) {{\n"),
            &[("arg".to_string(), arg), ("f".to_string(), self.formatter_arg_node.clone())].into(),
        ));
        *ident_count += 1;
        self.add_indentation(builder, *ident_count);
        builder.add_str("core::result::Result::Ok(_) => {\n");
        *ident_count += 1;
    }

    /// Flushes the pending bytes to the formatter.
    fn flush_pending_chars(
        &self,
        builder: &mut PatchBuilder<'_>,
        pending_chars: &mut String,
        ident_count: usize,
    ) {
        for chunk in pending_chars.as_bytes().chunks(FELT252_BYTES) {
            self.add_indentation(builder, ident_count);
            builder.add_modified(RewriteNode::interpolate_patched(
                &format!(
                    "core::byte_array::ByteArrayTrait::append_word(ref $f$.buffer, {:#x}, {});\n",
                    BigInt::from_bytes_be(Sign::Plus, chunk),
                    chunk.len(),
                ),
                &[("f".to_string(), self.formatter_arg_node.clone())].into(),
            ));
        }
        pending_chars.clear();
    }
}

/// Information about a placeholder argument of a format string, to inject into the formatter.
struct PlaceholderArgumentInfo {
    /// The source of the argument.
    source: PlaceholderArgumentSource,
    /// The formatting trait to use (e.g. `Display`, `Debug`).
    formatting_trait: FormattingTrait,
}

/// The source of a placeholder argument.
enum PlaceholderArgumentSource {
    /// The placeholder argument is given by a position index.
    Positional(usize),
    /// The placeholder argument is given by the next argument.
    Next,
    /// The placeholder argument is given by a name.
    Named(String),
}

/// A formatting trait is a specific method for how to format placeholder arguments within a format
/// string.
enum FormattingTrait {
    /// Got `{}` and we should use the `Display` trait.
    Display,
    /// Got `{:?}` and we should use the `Debug` trait.
    Debug,
    /// Got `{:x}` and we should use the `LowerHex` trait.
    LowerHex,
}
impl fmt::Display for FormattingTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormattingTrait::Display => write!(f, "Display"),
            FormattingTrait::Debug => write!(f, "Debug"),
            FormattingTrait::LowerHex => write!(f, "LowerHex"),
        }
    }
}

/// Extracts a placeholder argument from a format string. On error, returns Err with a relevant
/// error string.
fn extract_placeholder_argument(
    format_iter: &mut std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'_>>>,
) -> Result<PlaceholderArgumentInfo, &'static str> {
    // The part before the ':' (if any), indicating the name of the parameter.
    let mut parameter_name = String::new();
    // The part after the ':' (if any), indicating the formatting specification.
    let mut formatting_spec = String::new();
    let mut placeholder_terminated = false;
    for (_, c) in format_iter.by_ref() {
        if c == '}' {
            placeholder_terminated = true;
            break;
        }
        if c == ':' {
            for (_, c) in format_iter.by_ref() {
                if c == '}' {
                    placeholder_terminated = true;
                    break;
                }
                if c == ':' {
                    return Err("Unexpected character in placeholder: the formatting \
                                specification part (after the ':') can not contain a ':'");
                }
                if c.is_ascii_graphic() {
                    formatting_spec.push(c);
                } else {
                    return Err("Unexpected character in placeholder: the formatting \
                                specification part (after the ':') can only contain graphic \
                                characters");
                }
            }
            break;
        }
        if c.is_ascii_alphanumeric() || c == '_' {
            parameter_name.push(c);
        } else {
            return Err("Unexpected character in placeholder: parameter name can only contain \
                        alphanumeric characters and '_'. You may be missing a ':'");
        }
    }
    if !placeholder_terminated {
        return Err("Unterminated placeholder: no matching '}' for '{'");
    }

    let fmt_type = match formatting_spec.as_str() {
        "" => FormattingTrait::Display,
        "?" => FormattingTrait::Debug,
        "x" => FormattingTrait::LowerHex,
        _ => {
            return Err("Unsupported formatting trait: only `Display`, `Debug` and `LowerHex` \
                        are supported");
        }
    };

    let source = if parameter_name.is_empty() {
        PlaceholderArgumentSource::Next
    } else if let Ok(position) = parameter_name.parse::<usize>() {
        PlaceholderArgumentSource::Positional(position)
    } else if parameter_name.starts_with(|c: char| c.is_ascii_digit()) {
        return Err("Invalid parameter name");
    } else {
        PlaceholderArgumentSource::Named(parameter_name)
    };
    Ok(PlaceholderArgumentInfo { source, formatting_trait: fmt_type })
}
