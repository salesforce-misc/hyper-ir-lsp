# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## Features

* syntax highlighting for Hyper IR
* shows all functions in the document outline
* go to definition / references / declaration for functions, variables and debug references
* code folding support

## Development

1. `cd vscode-extension`
2. `pnpm i`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>

## TODO:

* LSP functionality
    * [x] Finish tokenizer
    * [x] Basic parser
    * [x] Parser for function bodies: Assigments & Labels
    * [x] Parser for function bodies: Branches
    * Parser for function bodies: phi nodes
    * Parser for function bodies: switch
    * Parser for function bodies: overflow arithmetics (`saddbr`, `longmuldivbr`, ...)
    * Parser: Support for debug annotation on external functions (forward compatibility)
    * Parser: Support for dependency declarations
    * [x] Document outline: Variables & Functions
    * [x] Document outline: Function-local Labels
    * [x] Go to definition / declaration / references for function
    * [x] Go to definition / references for debug refs
    * [x] Go to definition / references for global variables
    * [x] Go to definition / references for local variables
    * [x] Go to definition / references for basic blocks
    * [x] Code folding on function bodies
    * [x] Code folding on basic blocks
    * Code folding: Figure out why "folded text" does not work for VS Code; If we can get it to work, take advantage of it
    * [x] Inlay hint at end of function: Display function name
    * Inlay hint at basic block: List incoming edges
    * Somehow Hyperlink the stack trace
    * Highlight provider for function-local variables
    * Hover provider for function-local variables; Show "SSA chain"
    * tokenizer: only keep spans; don't copy out strings
    * take care of error recovery / make it robust
    * report warnings on duplicate function names / variable names
    * report warnings on unknown function names / variable names
    * incremental sync
    * Control flow graph as Mermaid charts
    * Support renames (functions, global vars, labels, local vars)
    * Add "Go to definition" for C++ hard-coded proxies
    * "Inline variables" debugger support?
* VS Code extension
    * [x] get a packaged VS Code extension
    * [x] correct word boundaries
    * use Webassembly instead of native binary
    * Proper logo
    * Package README
    * Write proper README
* configure in neovim
* Github CI
    * lint JS
    * [x] compile rust
    * [x] run rust test cases
    * package VS Code extension
    * automatically create release artifacts
* script to auto-generate the HyperIR dumps using HyperAPI
* Hyper:
    * Fix printing of references to unnamed globals
    * name the "column names" global variables
    * phi node: no ',' between incoming edges; compare to LLVM?
    * phi node: no whitespace around ','
    * conditional break: no ',' between condition and first basicblock?
    * missing "pure" modifier?
    * missing allocas?
    * missing types?
