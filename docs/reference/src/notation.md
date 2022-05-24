# Notation

## Grammar

The following notational convention is used in grammar snippets:

<style>
/* Prevent wrapping notation/examples in grammar table. */
#content code {
    white-space: nowrap;
}
</style>

| Notation                | Example                       | Meaning                                                   |
|-------------------------|-------------------------------|-----------------------------------------------------------|
| `CAPITAL_CASE`          | `IDENTIFIER`                  | A named nonterminal or lexer token                        |
| `"string"`              | `"if"`, `"+"`                 | The exact character(s)                                    |
| <code>\`string\`</code> | <code>\`"\`</code>            | Alternative syntax for exact character(s)                 |
| `snake_case`            | `let_stmt`, `item`            | A syntactical production                                  |
| `\x`                    | `\n`, `\r`, `\t`, `\0`        | The character represented by this escape                  |
| `x?`                    | `pub?`                        | An optional item                                          |
| `x*`                    | `item*`                       | 0 or more of `x`                                          |
| `x+`                    | `item+`                       | 1 or more of `x`                                          |
| `x{a}`                  | `[0-9a-fA-F]{2}`              | Exactly `a` repetitions of `x`                            |
| `x{a,b}`                | `[0-9a-fA-F]{1,6}`            | `a` to `b` repetitions of `x`                             |
| <code>&#124;</code>     | <code>felt &#124; int</code>  | Either one or another                                     |
| `[ ]`                   | `[bB]`                        | Any of the characters listed                              |
| `[ - ]`                 | `[a-z]`                       | Any of the characters in the range                        |
| `~[ ]`                  | `~[bB]`                       | Any characters, except those listed                       |
| `~"string"`             | `~"\n"`, `~"*/"`              | Any characters, except this sequence                      |
| `~CAPITAL_CASE`         | `~EOL*`                       | Any characters, except sequences matching the lexer token |
| `( )`                   | `( "," parameter )?`          | Groups items                                              |
| `/* text */`            | `ITEM /* except IMPL_ITEM */` | Human words supplementation                               |
