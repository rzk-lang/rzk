# Tools

Rzk proof assistant comes with built-in language server and formatter.

Other tools help enhance user experience or automate things.

### VS Code extension for Rzk

See [rzk-lang/vscode-rzk](https://github.com/rzk-lang/vscode-rzk).
VS Code extension offers a lot of conveniences and using VS Code is recommended for newcomers,
as it is considered the primary use case and has most support from the developers.

### Interactive games engine for Rzk

See [rzk-lang/rzk-game](https://github.com/rzk-lang/rzk-game).
This engine powers interactive proof games in the style of the Lean 4 games, but
for synthetic ∞-category theory. It compiles to WebAssembly and links the Rzk
library, so the typechecker runs in the browser. The player fills holes
(`#!rzk ?`) in a term, and for each hole the engine shows its goal and local
context.

Two games are playable in the browser, with no installation:

- the [Rzk Warm-up Game](https://rzk-lang.github.io/warmup-game/) assumes no
  prior Rzk and builds up from functions and pairs to a taste of directed types;
- the [∞-Yoneda Game](https://rzk-lang.github.io/yoneda-game/) follows Riehl's
  geodesic to the ∞-categorical Yoneda lemma.

Authoring a game needs no Haskell: a game is a table of contents and one file
per level. Start from the
[template](https://github.com/rzk-lang/rzk-game-template), which is itself
[playable](https://rzk-lang.github.io/rzk-game-template/) as a tour of what a
game can do.

### MkDocs plugin for Rzk

See [rzk-lang/mkdocs-plugin-rzk](https://github.com/rzk-lang/mkdocs-plugin-rzk).
MkDocs plugin enhances documentation build from literate Rzk Markdown files:

- adds diagram rendering (experimental)
- adds definition anchors (helpful to have "permalinks" to definitions)

### GitHub Action for Rzk

See [rzk-lang/rzk-action](https://github.com/rzk-lang/rzk-action).
This action allows to check your Rzk formalizations on GitHub automatically.
It can also be used to check formatting (experimental).

### Syntax highlighting (Pygments) for Rzk

See [rzk-lang/pygments-rzk](https://github.com/rzk-lang/pygments-rzk).
This is a simple syntax highlighter for Pygments (used by MkDocs and `minted` package in LaTeX).
Note that VS Code extension is using the Rzk Language Server for more accurate "semantic highlighting".
