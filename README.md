# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## Features

* syntax highlighting for Hyper IR
* shows all functions in the document outline
* Go to definition / references / declaration for functions, variables and debug references

## Development

1. `cd vscode-extension`
2. `pnpm i`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>

## TODO:

* LSP functionality
    * [x] Finish tokenizer
    * [x] Basic parser
    * [x] Parser for function bodies: Assigments & Labels
    * Parser for function bodies: Block terminators & phis
    * [x] Document outline: Variables & Functions
    * Document outline: Function-local Labels
    * [x] Go to definition / declaration / references for function
    * [x] Go to definition / references for debug refs
    * [x] Go to definition / references for global variables
    * [x] Go to definition / references for local variables
    * Go to definition / references for jump targets / basic blocks
    * Somehow Hyperlink the stack trace
    * Code folding on function bodies
    * Code folding on basic blocks; display terminating instruction
    * Highlight provider for function-local variables
    * tokenizer: only keep spans; don't copy out strings
    * take care of error recovery / make it robust
    * report warnings on duplicate function names / variable names
    * incremental sync
    * Control flow graph as Mermaid charts
    * Inlay hint at end of function: Display function name
    * Inlay hint at basic block: List incoming edges
    * Support renames (functions, global vars, labels, local vars)
    * Add "Go to definition" for C++ hard-coded proxies
    * "Inline variables" debugger support?
* VS Code extension
    * [x] get a packaged VS Code extension
    * [x] correct word boundaries
    * use Webassembly instead of native binary
    * Proper logo
    * Proper README
* configure in neovim
* Github CI
    * lint JS
    * [x] compile rust
    * [x] run rust test cases
    * package VS Code extension
    * automatically create release artifacts
* Hyper:
    * Fix printing of references to unnamed globals
    * name the "column names" global variables
    * missing allocas?
    * missing types?
