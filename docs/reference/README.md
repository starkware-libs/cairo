# Crust Reference

This document is the primary reference for the Crust programming language.

## Dependencies

- [mdBook](https://rust-lang.github.io/mdBook/) (use `brew install mdbook` or `cargo install mdbook`
  to install it).
- Python `^3.10` available globally as `python3` command.

## Development

This is a standard mdBook project.
We do not use any Rust-specific features.

To render a book, run:

```shell
mdbook build
```

To start the book in live server and open it in your browser, run:

```shell
mdbook serve
```

See [Creating a Book](https://rust-lang.github.io/mdBook/guide/creating.html) guide for more.

### Syntax highlighting

We maintain custom syntax definitions for Crust and our EBNF flavor
in [`theme/custom-highlight.js`](theme/custom-highlight.js) script.

### BNF auto-linking

We employ a little machinery to get auto-linking functionality in our grammar definitions, without
having to maintain these manually.

1. First, there is a preprocessor registered in mdBook which
   calls [`bnf_indexer.py`](bnf_indexer.py) script.
   This preprocessor collects all `bnf` code blocks, finds any non-terminal usages, and builds
   nonterminal ➡️ chapter path index.
   Then, in each chapter it writes a `BNF_INDEX` global JS variable with a tailored version of the
   global index, which only includes non-terminals defined in other chapters.

   The same preprocessor also populates the full grammar appendix.
2. Our `bnf` syntax highlighter adds `non-terminal` CSS class to all non-terminal references.
3. Finally, the [`theme/bnf-linking.js`](theme/bnf-linking.js) script gets `BNF_INDEX` and
   all `span.non-terminal` DOM nodes and replaces these spans with `a` links to appropriate
   chapters.
   Because the index does not contain non-terminals defined in current chapter, we will not clutter
   the page with links pointing to itself.
