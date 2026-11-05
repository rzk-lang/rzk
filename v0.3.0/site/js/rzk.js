/*
Language: rzk
Author: Nikolai Kudasov <nickolay.kudasov@gmail.com>
Category: functional
Website: https://fizruk.github.io/rzk/split.html
*/
hljs.registerLanguage('rzk',
function(hljs) {
  const COMMENT = { variants: [
    hljs.COMMENT('--', '$'),
  ] };
  const TYPE = {
    begin: /CUBE|TOPE|U|∑|2/,
    className: "type"
  };
  const SHAPE = {
    begin: /TOP|BOT/,
    className: "number"
  };
  const BUILTIN = {
    begin: /recOR|recBOT|idJ|refl/,
    className: "title.class"
  };
  const IDENT_RE = /[^\(\)\[\]\{\}\s]+/;
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
    contains: [ COMMENT, TYPE, SHAPE, BUILTIN ]
  };
  const CONTEXTS = { variants: [
    {
      begin: [
        /\(/,
        /\s*/,
        IDENT_RE,
        /\s+:/
      ],
      className: { 3: "variable" },
    },
    {
      begin: [
        /\[/,
        /\s*/,
        /[^\|]+/,
        /\s+\|->/
      ],
      className: { 3: "number" }
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
    aliases: [ 'rzk' ],
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
          /#def/,
          /\s+/,
          IDENT_RE,
        ],
        className: {
          1: "meta",
          3: "title.function"
        }
      }
    ]
  };
}
);
