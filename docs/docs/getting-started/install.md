# How to install Rzk

## VS Code extension with binaries (recommended)

!!! warning "Work-in-progress"
    We plan to make installation from a VS Code extension as recommended to simplify user experience. See [https://github.com/rzk-lang/vscode-rzk/issues/21](https://github.com/rzk-lang/vscode-rzk/issues/21) for details and current status.

## Install binaries

### Download from GitHub

You can download and use binaries (at least for some platforms) directly for one of the latest releases on GitHub at https://github.com/rzk-lang/rzk/releases. If your platform is not represented, please consider leaving an issue at https://github.com/rzk-lang/rzk/issues/new.

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
