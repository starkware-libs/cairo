use cairo_lang_defs::patcher::{PatchBuilder, RewriteNode};
use cairo_lang_defs::plugin::{
    InlineMacroExprPlugin, InlinePluginResult, NamedPlugin, PluginDiagnostic, PluginGeneratedFile,
};
use cairo_lang_filesystem::span::{TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::helpers::WrappedArgListHelper;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};
use cairo_lang_utils::{try_extract_matches, OptionHelper};
use num_bigint::{BigInt, Sign};

use super::{try_extract_unnamed_arg, unsupported_bracket_diagnostic};

/// Macro for printing.
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
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, false)
    }
}

/// Macro for printing with a new line.
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
    ) -> InlinePluginResult {
        generate_code_inner(syntax, db, true)
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
    let mut builder = PatchBuilder::new(db);
    let mut diagnostics = vec![];
    info.add_to_formatter(&mut builder, &mut diagnostics, with_newline);
    if !diagnostics.is_empty() {
        return InlinePluginResult { code: None, diagnostics };
    }
    InlinePluginResult {
        code: Some(PluginGeneratedFile {
            name: format!("{}_macro", get_macro_name(with_newline)).into(),
            content: builder.code,
            diagnostics_mappings: builder.diagnostics_mappings,
            aux_data: None,
        }),
        diagnostics: vec![],
    }
}

/// Gets the macro name according to the `with_newline` flag.
fn get_macro_name(with_newline: bool) -> &'static str {
    if with_newline { WritelnMacro::NAME } else { WriteMacro::NAME }
}

/// Information about a formating a string for the write macros.
struct FormattingInfo {
    /// The syntax rewrite node for the formatter input for the macro.
    formatter_node: RewriteNode,
    /// The format string argument.
    format_string_arg: ast::Arg,
    /// The format string content.
    format_string: String,
    /// The positional arguments for the format string.
    args: Vec<ast::Expr>,
}
impl FormattingInfo {
    /// Extracts the arguments from a formatted string macro.
    /// `defined_formatter` should be provided if it is defined outside.
    fn extract(
        db: &dyn SyntaxGroup,
        syntax: &ast::ExprInlineMacro,
    ) -> Result<FormattingInfo, Vec<PluginDiagnostic>> {
        let arguments = syntax.arguments(db);
        if !matches!(
            arguments,
            ast::WrappedArgList::ParenthesizedArgList(_) | ast::WrappedArgList::BracedArgList(_)
        ) {
            return Err(unsupported_bracket_diagnostic(db, syntax).diagnostics);
        }
        // `unwrap` is ok because the above `matches` condition ensures it's not None (unless the
        // pattern contains the `Missing` variant).
        let macro_arg_list = arguments.arg_list(db).unwrap();
        let macro_arg_list_elements = macro_arg_list.elements(db);
        let Some((formatter, without_formatter)) = macro_arg_list_elements.split_first() else {
            return Err(vec![PluginDiagnostic {
                stable_ptr: arguments.left_bracket_stable_ptr(db),
                message: "Macro expected formatter argument.".to_string(),
            }]);
        };
        let Some((format_string_arg, args)) = without_formatter.split_first() else {
            return Err(vec![PluginDiagnostic {
                stable_ptr: arguments.left_bracket_stable_ptr(db),
                message: "Macro expected format string argument.".to_string(),
            }]);
        };
        let mut diagnostics = vec![];
        let format_string = try_extract_unnamed_arg(db, format_string_arg)
            .and_then(|arg| try_extract_matches!(arg, ast::Expr::String)?.string_value(db))
            .on_none(|| {
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: format_string_arg.stable_ptr().untyped(),
                    message: "Argument must be a string literal.".to_string(),
                })
            });
        let args: Vec<_> = args
            .iter()
            .filter_map(|arg| {
                try_extract_unnamed_arg(db, arg).on_none(|| {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: arg.stable_ptr().untyped(),
                        message: "Expected unnamed argument.".to_string(),
                    })
                })
            })
            .collect();
        if !diagnostics.is_empty() {
            return Err(diagnostics);
        }
        Ok(FormattingInfo {
            formatter_node: RewriteNode::new_trimmed(formatter.as_syntax_node()),
            format_string_arg: format_string_arg.clone(),
            // `unwrap` is ok because the above `on_none` ensures it's not None.
            format_string: format_string.unwrap(),
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
        let mut arg_iter = self.args.iter().enumerate();
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
        while let Some((idx, c)) = format_iter.next() {
            if c == '{' {
                match format_iter.peek() {
                    None => {
                        diagnostics.push(PluginDiagnostic {
                            stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                            message: "Opening `{` without a matching `}`.".to_string(),
                        });
                        return;
                    }
                    Some(&(_, '{')) => {
                        pending_chars.push('{');
                        format_iter.next();
                        continue;
                    }
                    Some(&(_, '}')) => {
                        if let Some((position, arg)) = arg_iter.next() {
                            arg_used[position] = true;
                            self.append_formatted_arg(
                                builder,
                                &mut ident_count,
                                &mut pending_chars,
                                RewriteNode::new_trimmed(arg.as_syntax_node()),
                            );
                            format_iter.next();
                            continue;
                        } else {
                            missing_args += 1;
                        }
                    }
                    _ => {}
                }
                let Some(argument_length) = extract_argument_length(&mut format_iter) else {
                    diagnostics.push(PluginDiagnostic {
                        stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                        message: "Invalid format string: expected `}` or variable name."
                            .to_string(),
                    });
                    return;
                };
                let argument = &self.format_string[(idx + 1)..(idx + 1 + argument_length)];
                if let Ok(positional) = argument.parse::<usize>() {
                    let Some(arg) = self.args.get(positional) else {
                        diagnostics.push(PluginDiagnostic {
                            stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                            message: format!(
                                "Invalid reference to positional argument {positional} (there are \
                                 {} arguments).",
                                self.args.len()
                            ),
                        });
                        return;
                    };
                    arg_used[positional] = true;
                    self.append_formatted_arg(
                        builder,
                        &mut ident_count,
                        &mut pending_chars,
                        RewriteNode::new_trimmed(arg.as_syntax_node()),
                    );
                } else {
                    let start = format_string_base
                        .add_width(TextWidth::from_str(&self.format_string[..(idx + 1)]));
                    let end = start.add_width(TextWidth::from_str(argument));
                    self.append_formatted_arg(
                        builder,
                        &mut ident_count,
                        &mut pending_chars,
                        RewriteNode::CopiedText(argument.to_string(), TextSpan { start, end }),
                    );
                }
            } else if c == '}' {
                if matches!(format_iter.peek(), Some(&(_, '}'))) {
                    pending_chars.push('}');
                    format_iter.next();
                    continue;
                }
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                    message: "Closing `}` without a matching `{`.".to_string(),
                });
                return;
            } else {
                pending_chars.push(c);
            }
        }
        if missing_args > 0 {
            diagnostics.push(PluginDiagnostic {
                stable_ptr: self.format_string_arg.as_syntax_node().stable_ptr(),
                message: format!(
                    "{} positional arguments in format string, but only {} arguments.",
                    self.args.len() + missing_args,
                    self.args.len()
                ),
            });
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
                diagnostics.push(PluginDiagnostic {
                    stable_ptr: self.args[position].as_syntax_node().stable_ptr(),
                    message: "Unused argument.".to_string(),
                });
            }
        }
    }

    fn add_indentation(&self, builder: &mut PatchBuilder<'_>, count: usize) {
        for _ in 0..count {
            builder.add_str("    ");
        }
    }

    /// Appends a formatted argument to the formatter, flushing the pending bytes if necessary.
    fn append_formatted_arg(
        &self,
        builder: &mut PatchBuilder<'_>,
        ident_count: &mut usize,
        pending_chars: &mut String,
        arg: RewriteNode,
    ) {
        self.flush_pending_chars(builder, pending_chars, *ident_count);
        self.add_indentation(builder, *ident_count);
        builder.add_modified(RewriteNode::interpolate_patched(
            "match core::fmt::Display::fmt(@$arg$, ref $f$) {\n",
            &[("arg".to_string(), arg), ("f".to_string(), self.formatter_node.clone())].into(),
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
        const FELT252_BYTES: usize = 31;
        for chunk in pending_chars.as_bytes().chunks(FELT252_BYTES) {
            self.add_indentation(builder, ident_count);
            builder.add_modified(RewriteNode::interpolate_patched(
                &format!(
                    "core::byte_array::ByteArrayTrait::append_word(ref $f$.buffer, {:#x}, {});\n",
                    BigInt::from_bytes_be(Sign::Plus, chunk),
                    chunk.len(),
                ),
                &[("f".to_string(), self.formatter_node.clone())].into(),
            ));
        }
        pending_chars.clear();
    }
}

/// Extracts an argument from a format string.
fn extract_argument_length(
    format_iter: &mut std::iter::Peekable<std::iter::Enumerate<std::str::Chars<'_>>>,
) -> Option<usize> {
    let mut length = 0;
    for (_, c) in format_iter.by_ref() {
        if c == '}' {
            return Some(length);
        }
        if c.is_ascii_alphanumeric() || c == '_' {
            length += 1;
        } else {
            return None;
        }
    }
    None
}
