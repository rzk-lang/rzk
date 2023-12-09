# How to install Rzk

## VS Code extension with binaries (recommended)

These instructions will walk you through setting up Rzk using the "basic" setup and VS Code as the editor.

1. Install [VS Code](https://code.visualstudio.com/).
2. Launch VS Code and install the [`rzk` extension](https://marketplace.visualstudio.com/items?itemName=NikolaiKudasovfizruk.rzk-1-experimental-highlighting).
3. Create a new file using "File > New Text File" (<kbd>Ctrl+N</kbd>). Click the `Select a language` prompt, type in `rzk`, and select "Literate Rzk Markdown".
   ![VS Code rzk language selector.](../assets/images/vscode-rzk-select-language.png)
4. You should see the following popup:
   ![VS Code rzk install prompt.](../assets/images/vscode-rzk-install-prompt.png)
5. Click "Yes" button.
6. While it is installing, you can paste the following literate Rzk program into the new file:

   ````markdown
   # Sample literate Rzk markdown

   ```rzk
   #lang rzk-1

   #define id (A : U)
     : A -> A
     := \ x -> x
   ```
   ````

7. When the installation is done you should see the following popup:
   ![VS Code rzk reload prompt.](../assets/images/vscode-rzk-install-success-reload-prompt.png)
8. Click "Reload button".
9. Save your file as `example.rzk.md`.
10. Open local Terminal (<kbd>Ctrl+`</kbd>).

    <!-- ` -->

11. In the terminal, run

    ```sh
    rzk typecheck example.rzk.md
    ```

12. You should see the output of the proof assistant:

    ```text
    Loading file example.rzk.md
    Checking module from example.rzk.md
    [ 1 out of 1 ] Checking #define id
    Everything is ok!
    ```

13. Congratulations! Now you have a working rzk setup :) Note that the rzk extension will notify you about updates of `rzk` and prompt updating to new versions.

14. See [Quickstart](quickstart.rzk.md) to get familiar with the Rzk language!

## Install binaries

### Download from GitHub

You can download and use binaries (at least for some platforms) directly for one of the latest releases on GitHub at <https://github.com/rzk-lang/rzk/releases>. If your platform is not represented, please consider leaving an issue at <https://github.com/rzk-lang/rzk/issues/new>.

## Install from sources

You can install `rzk` from sources. You can get the latest "stable" release from Hackage or build from the `develop` branch on GitHub.

### Stack

To build and install with Stack from Hackage:

```sh
stack install rzk
```

To build and install with Stack from sources on GitHub:

```sh
git clone https://github.com/rzk-lang/rzk.git
cd rzk
git checkout develop
stack build && stack install
```

### cabal-install

To build and install with `cabal-install` from Hackage:

```sh
cabal v2-update
cabal v2-install rzk
```

To build and install with `cabal-install` from sources on GitHub:

```sh
git clone https://github.com/rzk-lang/rzk.git
cd rzk
git checkout develop
cabal v2-build && cabal v2-install
```

### Nix

!!! warning "Work-in-progress"

    To be done.
