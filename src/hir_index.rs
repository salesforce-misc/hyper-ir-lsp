use crate::{
    hir_parser::Statement,
    hir_tokenizer::{Span, Spanned, Token},
};
use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct UseDef {
    pub decls: Vec<Span>,
    pub defs: Vec<Span>,
    pub uses: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct HIRIndex {
    pub global_vars: HashMap<String, UseDef>,
    pub functions: HashMap<String, UseDef>,
    pub dgb_annotations: HashMap<String, UseDef>,
}

fn add_decl(m: &mut HashMap<String, UseDef>, u: &Spanned<String>) {
    m.entry(u.0.clone()).or_default().decls.push(u.1.clone());
}

fn add_def(m: &mut HashMap<String, UseDef>, u: &Spanned<String>) {
    m.entry(u.0.clone()).or_default().defs.push(u.1.clone());
}

pub fn create_index(_tokens: &[Spanned<Token>], stmts: &[Statement]) -> HIRIndex {
    let mut index = HIRIndex {
        global_vars: HashMap::new(),
        functions: HashMap::new(),
        dgb_annotations: HashMap::new(),
    };

    // Index all global symbols
    for s in stmts.iter() {
        match s {
            Statement::GlobalVar { name, def: _ } => add_def(&mut index.global_vars, name),
            Statement::FuncDecl { signature, addr: _ } => {
                add_decl(&mut index.functions, &signature.name)
            }
            Statement::FuncDef { signature, body: _ } => {
                add_def(&mut index.functions, &signature.name)
            }
            Statement::DbgAnnotation { name, def: _ } => add_def(&mut index.dgb_annotations, name),
        }
    }

    index
}

#[test]
fn test_index() {
    // Test with arguments and with modifiers
    let res = crate::hir_parser::parse_from_str(
        "
        @a = \"test\"
        @b = \"test2\"
        declare void @foo::bar(int1 %, data128 %baz)
        define void @foo::bar(int1 %, data128 %baz) {
          ret !21 # some comment
        }
        !21 {\"some\": \"data\"}",
    );
    assert_eq!(res.errors, []);
    let idx = create_index(&res.tokens, &res.stmts);
    assert_eq!(
        idx.global_vars,
        HashMap::from([
            (
                "@a".to_string(),
                UseDef {
                    decls: Vec::new(),
                    defs: vec![9..11],
                    uses: Vec::new()
                }
            ),
            (
                "@b".to_string(),
                UseDef {
                    decls: Vec::new(),
                    defs: vec![29..31],
                    uses: Vec::new()
                }
            )
        ])
    );

    assert_eq!(
        idx.functions,
        HashMap::from([(
            "@foo::bar".to_string(),
            UseDef {
                decls: vec![63..72],
                defs: vec![115..124],
                uses: Vec::new()
            }
        )])
    );

    assert_eq!(
        idx.dgb_annotations,
        HashMap::from([(
            "21".to_string(),
            UseDef {
                decls: Vec::new(),
                defs: vec![200..203],
                uses: Vec::new()
            }
        )])
    );
}
