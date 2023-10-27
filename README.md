# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## Features

* Syntax highlighting for Hyper IR
* Document outline: Directly jump to the function you are interested in
* Go to definition / references / declaration for functions, variables and debug information
* Code folding support

## Building from source

1. `cd vscode-extension`
2. `pnpm i`
3. `pnpm package`
4. Install the "VSIX" in VS Code

## TODO

* LSP functionality
    * ✔ Finish tokenizer
    * ✔ Basic parser
    * ✔ Parser: Support for dependency declarations
    * ✔ Parser: Support debug annotation on external functions (forward compatibility)
    * ✔ Parser for function bodies: Assigments & Labels
    * ✔ Parser for function bodies: Branches
    * Parser for function bodies: phi nodes
    * Parser for function bodies: switch
    * Parser for function bodies: overflow arithmetics (`saddbr`, `longmuldivbr`, ...)
    * ✔ Document outline: Variables & Functions
    * ✔ Document outline: Function-local Labels
    * ✔ Go to definition / declaration / references for function
    * ✔ Go to definition / references for debug refs
    * ✔ Go to definition / references for global variables
    * ✔ Go to definition / references for local variables
    * ✔ Go to definition / references for basic blocks
    * ✔ Code folding on function bodies
    * ✔ Code folding on basic blocks
    * ✔ Inlay hint at end of function: Display function name
    * Inlay hint at basic block: List incoming edges
    * Somehow Hyperlink the stack trace
    * Highlight provider for function-local variables
    * Hover provider for function-local variables; Show "SSA chain"
    * Report warnings on duplicate function names / variable names
    * Report warnings on unknown function names / variable names
    * ✔ Report warnings when we failed to extract the basic blocks from a branching instruction
    * Tokenizer: only keep "string views"; don't copy out strings
    * Take care of error recovery / make it robust
    * Incremental sync
    * Control flow graph as Mermaid charts
    * Support renames (functions, global vars, labels, local vars)
    * Add "Go to definition" for C++ hard-coded proxies
    * "Inline variables" debugger support?
    * Code folding: Use "folded text" as soon as VS Code supports it
* VS Code extension
    * ✔ Get a packaged VS Code extension
    * ✔ Correct word boundaries
    * ✔ Include README
    * Proper logo
    * Write proper README
    * Use Webassembly instead of native binary
* Configuration for neovim
* Github CI
    * Lint JS
    * ✔ compile rust
    * ✔ run rust test cases
    * Package VS Code extension
    * Automatically create release artifacts
* Script to auto-generate the HyperIR dumps using HyperAPI
* Hyper:
    * Fix printing of references to unnamed globals
    * Name the "column names" global variables
    * Phi node: no ',' between incoming edges; compare to LLVM?
    * Phi node: no whitespace around ','
    * Conditional break: no ',' between condition and first basicblock?
    * Missing "pure" modifier?
    * Missing allocas?
    * Missing types?
