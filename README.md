# ocron

A feature-rich, safer cron alternative, written in OCaml.

# Installation

## Homebrew

First, add [this tap](https://github.com/Brendon-Hablutzel/homebrew-tools):

```bash
brew tap Brendon-Hablutzel/homebrew-tools
```

Then, install the ocron formula:

```bash
brew install ocron
```

This will install the ocron CLI (`ocron`) and daemon (`ocrond`).

## Other options

Source code and binary downloads can be found on the [releases page](https://github.com/Brendon-Hablutzel/ocron/releases) for this repository.

# Development

Note: because of some oddities with using the terminal's raw mode, you can't run the CLI via dune and instead need to invoke the executable directly:

```bash
./_build/default/cli/main.exe
```
