/*
Language: rzk
Author: Nikolai Kudasov <nickolay.kudasov@gmail.com>
Category: functional
Website: https://github.com/fizruk/highlightjs-rzk
*/
hljs.registerLanguage('rzk',
  function (hljs) {
    const COMMENT = {
      variants: [
        hljs.COMMENT('--', '$'),
      ]
    };
    const TYPE = {
      begin: /(\b(CUBE|TOPE|U|𝒰|Sigma|1|2|𝟙|𝟚)\b|(∑|Σ))/,
      className: "type"
    };
    const SHAPE = {
      begin: /((\*_1|⋆)|\b(0_2|1_2|TOP|BOT)\b|(===|<=|\\\/ | \/\\|⊤|⊥))/,
      className: "number"
    };
    const BUILTIN = {
      begin: /\b(recOR|rec∨|recBOT|rec⊥|idJ|refl|first|second|π₁|π₂)\b/,
      className: "title.class"
    };
    const IDENT_RE = /[^\-\?\!\.\\;:,#\"\]\[\)\(\}\{><\| \t\n\r][^\.\\;:,#\"\]\[\)\(\}\{><\| \t\n\r]*/;
    const MANY_IDENT_RE = /[^\-\?\!\.\\;:,#\"\]\[\)\(\}\{><\| \t\n\r][^\.\\;:,#\"\]\[\)\(\}\{><\|]*/;
    const POINT_VAR = {
      begin: [
        /\{/,
        /\s*/,
        IDENT_RE,
        /\s*:\s*/,
        /[^\}|]+/,
        /\s+\|\s+/,
        /[^\}|]+/,
        /\}/
      ],
      className: {
        3: "variable",
        // 5: "type",
        7: "number"
      },
      contains: [COMMENT, TYPE, SHAPE, BUILTIN]
    };
    const CONTEXTS = {
      variants: [
        {
          begin: /\b(as)\b/,
          className: "keyword"
        },
        {
          begin: [
            /\(/,
            /\s*/,
            MANY_IDENT_RE,
            /\s+:/
          ],
          className: { 3: "variable" },
        },
        {
          begin: /_\{/,
          end: /\}/,
          scope: "string"
        }
      ],
      contains: [
        'self', COMMENT, POINT_VAR, TYPE, SHAPE, BUILTIN
      ]
    };
    return {
      name: 'rzk',
      aliases: ['rzk'],
      illegal: '</',
      contains: [
        COMMENT,
        POINT_VAR,
        CONTEXTS,
        TYPE,
        SHAPE,
        BUILTIN,
        {
          begin: [
            /#lang/,
            /\s+/,
            /rzk-1/
          ],
          className: {
            1: "meta",
            3: "keyword"
          }
        },
        {
          begin: [
            /(#def|#define|#postulate)\b/,
            /\s+/,
            IDENT_RE,
            /\s+/,
            /\buses\b/,
            /\s*\(\s*/,
            MANY_IDENT_RE,
            /\)/
          ],
          className: {
            1: "meta",
            3: "title.function",
            5: "meta",
            7: "variable"
          }
        },
        {
          begin: [
            /(#def|#define|#postulate)\b/,
            /\s+/,
            IDENT_RE
          ],
          className: {
            1: "meta",
            3: "title.function"
          }
        },
        {
          begin: [
            /(#section|#end)\b/,
            /\s+/,
            IDENT_RE
          ],
          className: {
            1: "keyword",
            3: "section.name"
          }
        },
        {
          begin: [
            /(#section|#end)\b/
          ],
          className: {
            1: "keyword"
          }
        },
        {
          begin: [
            /(#check|#compute(-whnf|-nf)?|#set-option|#unset-option)\b/
          ],
          className: {
            1: "type.command"
          }
        },
        {
          begin: [
            /(#assume|#variable|#variables)\b/,
            /\s+/,
            MANY_IDENT_RE,
          ],
          className: {
            1: "meta",
            3: "variable"
          }
        }
      ]
    };
  }
);
