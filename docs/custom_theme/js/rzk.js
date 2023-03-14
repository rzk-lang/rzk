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
    begin: /CUBE|TOPE|U|∑/,
    className: "type"
  };
  const BUILTIN = {
    begin: /recOR|recBOT|idJ|refl/,
    className: "title.class"
  };
  const POINT_VAR = {
    begin: [
      /\{/,
      /\s*/,
      /[a-zA-Z_][a-zA-Z_0-9-]*/,
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
    contains: [ COMMENT, TYPE, BUILTIN ]
  };
  const CONTEXTS = { variants: [
    {
      begin: [
        /\(/,
        /\s*/,
        /[a-zA-Z_][a-zA-Z_0-9-]*/,
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
      'self', COMMENT, POINT_VAR, TYPE, BUILTIN
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
          /[a-zA-Z_][a-zA-Z_0-9-]*/
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
