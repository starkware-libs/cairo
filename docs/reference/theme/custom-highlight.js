function kw(str) {
    return str.replace(/\s/g, ' ').trim();
}

/*
 * Based on Language: Rust
 * Original Contributors:
 *     Andrey Vlasovskikh <andrey.vlasovskikh@gmail.com>,
 *     Roman Shmatov <romanshmatov@gmail.com>,
 *     Kasper Andersen <kma_untrusted@protonmail.com>,
 */
function crustHighlight(hljs) {
    const NUM_SUFFIX = '(f|uint|int|[ui](8|16|32|64|128))?';
    const KEYWORDS = kw(`
        as break const continue else enum fn for hint if in let pub return Self self struct true
        type use
        assert do dyn extern impl macro match mod move ref static_assert static super trait try
        typeof unsafe where while while with yield
    `);
    const BUILTINS = kw(`
        felt
        int uint
        i8 816 i32 i64 i128 i256
        u8 816 u32 u64 u128 u256
        str char
        bool
    `);
    const LITERALS = kw(`
        true false
    `);
    return {
        name: 'Crust',
        keywords: {
            $pattern: hljs.IDENT_RE,
            keyword: KEYWORDS,
            literal: LITERALS,
            built_in: BUILTINS,
        },
        illegal: '</',
        contains: [
            hljs.C_LINE_COMMENT_MODE,
            hljs.inherit(hljs.QUOTE_STRING_MODE, {
                begin: /[a-z]*"/,
                illegal: null,
            }),
            {
                begin: /[a-z]*"""/,
                end: /"""/,
                contains: [
                    hljs.BACKSLASH_ESCAPE,
                ],
            },
            {
                className: 'string',
                variants: [
                    {
                        begin: /r(#*)"(.|\n)*?"\1(?!#)/,
                    },
                    {
                        begin: /b?'\\?(x\w{2}|u\w{4}|U\w{8}|.)'/,
                    },
                ],
            },
            {
                className: 'symbol',
                begin: /'[a-zA-Z_][a-zA-Z0-9_]*/,
            },
            {
                className: 'number',
                variants: [
                    {
                        begin: '\\b0b([01_]+)' + NUM_SUFFIX,
                    },
                    {
                        begin: '\\b0o([0-7_]+)' + NUM_SUFFIX,
                    },
                    {
                        begin: '\\b0x([A-Fa-f0-9_]+)' + NUM_SUFFIX,
                    },
                    {
                        begin:
                            '\\b(\\d[\\d_]*(\\.[0-9_]+)?([eE][+-]?[0-9_]+)?)' +
                            NUM_SUFFIX,
                    },
                ],
                relevance: 0,
            },
            {
                className: 'function',
                beginKeywords: 'fn',
                end: '(\\(|<)',
                excludeEnd: true,
                contains: [hljs.UNDERSCORE_TITLE_MODE],
            },
            {
                className: 'class',
                beginKeywords: 'type',
                end: ';',
                contains: [
                    hljs.inherit(hljs.UNDERSCORE_TITLE_MODE, {
                        endsParent: true,
                    }),
                ],
                illegal: '\\S',
            },
            {
                className: 'class',
                beginKeywords: 'trait enum struct',
                end: /\{/,
                contains: [
                    hljs.inherit(hljs.UNDERSCORE_TITLE_MODE, {
                        endsParent: true,
                    }),
                ],
                illegal: '[\\w\\d]',
            },
            {
                begin: hljs.IDENT_RE + '::',
                keywords: {
                    built_in: BUILTINS,
                },
            },
            {
                begin: '->',
            },
        ],
    };
}

/*
 * Original Language: Extended Backus-Naur Form
 * Original Contributors:
 *     Alex McKibben <alex@nullscope.net>,
 */
function bnfHighlight(hljs) {
    const nonTerminalMode = {
        className: 'keyword non-terminal',
        begin: /[A-Z][a-zA-Z_]*/,
    };

    const charactersMode = {
        className: 'string',
        variants: [
            { begin: '"', end: '"' },
            { begin: '`', end: '`' },
            { begin: '\\[', end: '\\]' },
        ],
    };

    const commentMode = hljs.inherit(hljs.C_BLOCK_COMMENT_MODE, {
        contains: [
            nonTerminalMode,
            charactersMode,
        ],
    });

    const ruleBodyMode = {
        begin: /[:|]/,
        end: /\n/,
        contains: [
            nonTerminalMode,
            charactersMode,
            commentMode,
        ],
    };

    return {
        name: 'Extended Backus-Naur Form',
        illegal: /\S/,
        contains: [
            nonTerminalMode,
            ruleBodyMode,
        ],
    };
}

hljs.registerLanguage('crust', crustHighlight);
hljs.registerLanguage('bnf', bnfHighlight);
hljs.initHighlightingOnLoad();
