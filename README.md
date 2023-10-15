# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## TODO:

* LSP functionality
    * [x] Finish tokenizer
    * Grammar / parser
    * Go to definition / references for debug refs
    * Go to definition / references for global variables
    * Go to definition / declaration references for function
    * Go to definition / references for local variables
    * Go to definition / references for jump targets / basic blocks
    * Somehow Hyperlink the stack trace
    * Code folding on function bodies
    * Code folding on basic blocks; display terminating instruction
    * Control flow graph as Mermaid charts
* VS Code: correct word boundaries
* packaging
    * configure in neovim
    * auto-build in Github CI

## Development
1. `pnpm i`
2. `cargo build`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>
> **Note**  
> 
> If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
> please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details
