# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## Features

* syntax highlighting for Hyper IR
* shows all functions in the document outline

## Development

1. `cd vscode-extension`
2. `pnpm i`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>

## TODO:

* LSP functionality
    * [x] Finish tokenizer
    * [x] Basic parser
    * Parser for function bodies
    * [x] Show document outline
    * Go to definition / declaration / references for function
    * Go to definition / references for debug refs
    * Go to definition / references for global variables
    * Go to definition / references for local variables
    * Go to definition / references for jump targets / basic blocks
    * Somehow Hyperlink the stack trace
    * Code folding on function bodies
    * Code folding on basic blocks; display terminating instruction
    * tokenizer: only keep spans; don't copy out strings
    * take care of error recovery / make it robust
    * report warnings on duplicate function names
    * incremental sync
    * Control flow graph as Mermaid charts
* VS Code
    * [x] get a packaged VS Code extension
    * [x] correct word boundaries
    * use Webassembly instead of native binary
* configure in neovim
* Github CI
    * lint JS
    * [x] compile rust
    * [x] run rust test cases
    * package VS Code extension
    * automatically create release artifacts