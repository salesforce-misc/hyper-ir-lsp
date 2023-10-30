# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## Features

* Syntax highlighting for Hyper IR
* Document outline: Directly jump to the function you are interested in
* Go to definition / references / declaration for functions, variables and debug information
* Inlay hints listing the incoming edges for each basic block
* Inlay hints at function end to easily jump back to function beginning
* Code folding support

## Building from source

1. `cd vscode-extension`
2. `pnpm i`
3. `pnpm package`
4. Install the "hyper-ir-lsp-*.vsix" in VS Code

## TODO

* LSP functionality
    * ✔ Finish tokenizer
    * ✔ Basic parser
    * ✔ Parser: Support for dependency declarations
    * ✔ Parser: Support debug annotation on external functions (forward compatibility)
    * ✔ Parser for function bodies: Assigments & Labels
    * ✔ Parser for function bodies: Branches
    * ✔ Parser for function bodies: phi nodes
    * ✔ Parser for function bodies: switch
    * ✔ Parser for function bodies: overflow arithmetics (`saddbr`, `longmuldivbr`, ...)
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
    * ✔ Inlay hint at basic block: List incoming edges
    * Somehow Hyperlink the stack trace
    * ✔ Report warnings on duplicate function names / variable names
    * ✔ Report warnings on unknown function names / variable names
    * ✔ Report warnings when we failed to extract the basic blocks from a branching instruction
    * Add "Go to definition" for C++ hard-coded proxies
    * ✔ Control flow graph visualization
    * Hover provider for function-local variables; Show "SSA chain"
    * "Inline variables" debugger support?
    * Incremental sync
    * Take care of error recovery / make it robust
    * Figure out what those "*.hir.git" files are about which show up in the problems list
    * Support renames (functions, global vars, labels, local vars)
    * Highlight provider for function-local variables (not sure it's worth it? How is this used by VSCode?)
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
    * ✔ Compile rust
    * ✔ Run rust test cases
    * ✔ Lint JS
    * ✔ Package VS Code extension
    * ✔ Compile also for Windows and macos
    * Automatically create release artifacts
* Script to auto-generate the HyperIR dumps using HyperAPI
* Hyper:
    * Fix printing of references to unnamed globals
    * Name the "column names" global variables
    * Phi node: Missing ',' between incoming edges
    * Phi node: Missing whitespace around ','
    * `switch`: Missing ',' between value and default
    * Conditional break: no ',' between condition and first basicblock
    * Teardown-functions: move `destructDone` to the end
    * Stop printing stack traces at `main`
    * Missing "pure" modifier?
    * Missing allocas?
    * Missing types?
* Code Style / Things I still need to learn about Rust
    * Deduplicate the `just` + `map_with_span` pattern when parsing instructions
    * Tokenizer: only keep "string views"; don't copy out strings
