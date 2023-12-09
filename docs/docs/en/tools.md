# Tools

Rzk proof assistant comes with built-in language server and formatter.

Other tools help enhance user experience or automate things.

### VS Code extension for Rzk

See [rzk-lang/vscode-rzk](https://github.com/rzk-lang/vscode-rzk).
VS Code extension offers a lot of conveniences and using VS Code is recommended for newcomers,
as it is considered the primary use case and has most support from the developers.

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
