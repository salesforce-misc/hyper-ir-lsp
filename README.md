# Hyper IR language server

Code intelligence for Hyper IR files.
Pass the `dump_ir=1` parameter to `hyperd` to get Hyper IR dumps.

## TODO:

* Finish tokenizer
* Report bugs as warnings, not errors
* Grammar / parser
* Go to definition / references for global variables
* Go to definition / references for local variables
* Go to definition / references for jump targets / basic blocks
* Go to definition / references for debug refs

## Development
1. `pnpm i`
2. `cargo build`
3. press <kbd>F5</kbd> or change to the Debug panel and click <kbd>Launch Client</kbd>
> **Note**  
> 
> If encountered errors like `Cannot find module '/xxx/xxx/dist/extension.js'`
> please try run command `tsc -b` manually, you could refer https://github.com/IWANABETHATGUY/tower-lsp-boilerplate/issues/6 for more details
