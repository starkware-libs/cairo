# Numeric literals

```bnf
INTEGER_LITERAL : ( DEC_LITERAL | HEX_LITERAL | BIN_LITERAL ) LITERAL_SUFFIX?

DEC_LITERAL : [0-9] [0-9_]*
HEX_LITERAL : "0x" [a-fA-F0-9_]*
BIN_LITERAL : "0b" [01_]*

LITERAL_SUFFIX : XID_Start XID_Continue*
```

Integer literal can be written using three bases:

1. Decimal literal starts with a decimal digi and then a mixture of decimal digits and underscores.
2. Hexadecimal literal starts with the character sequence `0x` and continues as a mixture of hex
   digits and underscores.
3. Binary literal starts with the character sequence `0b` and continues as a mixture of binary
   digits and underscores.

Underscore character (`_`) is only a visual separator, and it has no influence on the number's
value.

> **Note**: Character sequence `_1234` is a valid identifier, not an integer literal.

> **Note**: Floating-point numbers are not supported.

Numeric literal may be followed (immediately, without any spaces) by a _literal suffix_, which
forcibly sets the type of the literal.
The literal suffix is any valid identifier which does not start with underscore (`_`), but only
selected values are semantically correct.

The type of _unsuffixed number literal_ is determined by type inference:

1. If an integer type can be _uniquely_ determined from the surrounding program context, the
   unsuffixed integer literal has that type.
2. If the program context under-constrains the type, it defaults to `felt`.
3. If the program context over-constrains the type, it is considered a static type error.

Examples of integer literals of various forms:

| Literal                 | Value | Type   |
|-------------------------|-------|--------|
| `1234`                  | 1234  | `felt` |
| `1234f`                 | 1234  | `felt` |
| `1234_f`                | 1234  | `felt` |
| `1234i`                 | 1234  | `int`  |
| `1234u`                 | 1234  | `uint` |
| `0x4D2`                 | 1234  | `felt` |
| `0x4_D2`                | 1234  | `felt` |
| `0x_4_D2`               | 1234  | `felt` |
| `0b0000_0100_1101_0010` | 1234  | `felt` |

Examples of invalid integer literals:

- Invalid suffix: \
  `1234suffix`
- Use of digits of wrong base: \
  `123AFB43`, `0b0102`, `0o0581`
- Binary and hexadecimal literals must have at least one digit: \
  `0b_`, `0x____`

> **Note:** Crust syntax considers `-1` as an application of
> the [unary minus operator](negation-operators.md) to integer literal `1`, rather than a single
> integer literal.
