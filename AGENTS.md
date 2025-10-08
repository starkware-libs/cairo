Do not read or grep crates/cairo-lang-syntax/src/node/ast.rs. Instead read crates/cairo-lang-syntax-codegen/src/generator.rs.
Do not use bash -c unless absolutely necessary (use cargo test -p cairo-lang-syntax-codegen instead of bash -c "cd ...").
Attempt to write commands without redirects (cat | less) if possible..
