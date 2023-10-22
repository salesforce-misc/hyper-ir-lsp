use crate::{
    hir_parser::Statement,
    hir_tokenizer::{Span, Spanned, Token},
};
use std::collections::{BTreeMap, HashMap};

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct UseDefList {
    pub decls: Vec<Span>,
    pub defs: Vec<Span>,
    pub uses: Vec<Span>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UseDefKind {
    Decl,
    Def,
    Use,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    GlobalVar,
    Function,
    DbgAnnotation,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SymbolOccurrence {
    pub span: Span,
    pub use_def: UseDefKind,
    pub symbol_kind: SymbolKind,
    pub name: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct HIRIndex {
    pub global_vars: HashMap<String, UseDefList>,
    pub functions: HashMap<String, UseDefList>,
    pub dgb_annotations: HashMap<String, UseDefList>,
    pub reverse_idx: BTreeMap<usize, SymbolOccurrence>,
}

impl HIRIndex {
    pub fn add(&mut self, k: SymbolKind, ud: UseDefKind, span: &Span, name: &str) {
        let map = match k {
            SymbolKind::GlobalVar => &mut self.global_vars,
            SymbolKind::Function => &mut self.functions,
            SymbolKind::DbgAnnotation => &mut self.dgb_annotations,
        };
        let e = map.entry(name.to_string()).or_default();
        if e.decls.contains(span) || e.defs.contains(span) || e.uses.contains(span) {
            return;
        }
        match ud {
            UseDefKind::Decl => e.decls.push(span.clone()),
            UseDefKind::Def => e.defs.push(span.clone()),
            UseDefKind::Use => e.uses.push(span.clone()),
        };
        self.reverse_idx.insert(
            span.start,
            SymbolOccurrence {
                span: span.clone(),
                use_def: ud,
                symbol_kind: k,
                name: name.to_string(),
            },
        );
    }

    pub fn add_spanned(&mut self, k: SymbolKind, ud: UseDefKind, u: &Spanned<String>) {
        self.add(k, ud, &u.1, &u.0)
    }
}

pub fn create_index(tokens: &[Spanned<Token>], stmts: &[Statement]) -> HIRIndex {
    let mut index = HIRIndex {
        ..Default::default()
    };

    // Index all global symbols
    for s in stmts.iter() {
        match s {
            Statement::GlobalVar { name, def: _ } => {
                index.add_spanned(SymbolKind::GlobalVar, UseDefKind::Def, &name)
            }
            Statement::FuncDecl { signature, addr: _ } => {
                index.add_spanned(SymbolKind::Function, UseDefKind::Decl, &signature.name)
            }
            Statement::FuncDef { signature, body: _ } => {
                index.add_spanned(SymbolKind::Function, UseDefKind::Def, &signature.name)
            }
            Statement::DbgAnnotation { name, def: _ } => {
                index.add_spanned(SymbolKind::DbgAnnotation, UseDefKind::Def, &name)
            }
        }
    }

    // Index all uses
    for t in tokens.iter() {
        match &t.0 {
            Token::GlobalName(name) => {
                if index.global_vars.contains_key(&name.to_string()) {
                    index.add(SymbolKind::GlobalVar, UseDefKind::Use, &t.1, &name)
                }
                if index.functions.contains_key(&name.to_string()) {
                    index.add(SymbolKind::Function, UseDefKind::Use, &t.1, &name)
                }
            }
            Token::DebugRef(name) => {
                index.add(SymbolKind::DbgAnnotation, UseDefKind::Use, &t.1, &name)
            }
            _ => {}
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
        declare void @foo::bar(ptr %, int32 %baz) = 0x1234
        declare void @_test1(int1 %, data128 %baz)
        define void @_test1(int1 %, data128 %baz) {
          call @foo::bar(ptr @a, int32 8)
          ret !21 # some comment
        }
        !21 {\"some\": \"data\"}",
    );
    assert_eq!(res.errors, []);
    let idx = create_index(&res.tokens, &res.stmts);
    assert_eq!(
        idx.global_vars,
        HashMap::from([(
            "@a".to_string(),
            UseDefList {
                decls: Vec::new(),
                defs: vec![9..11],
                uses: vec![212..214]
            }
        )])
    );

    assert_eq!(
        idx.functions,
        HashMap::from([
            (
                "@foo::bar".to_string(),
                UseDefList {
                    decls: vec![42..51],
                    defs: Vec::new(),
                    uses: vec![198..207]
                }
            ),
            (
                "@_test1".to_string(),
                UseDefList {
                    decls: vec![101..108],
                    defs: vec![151..158],
                    uses: Vec::new()
                }
            ),
        ])
    );

    assert_eq!(
        idx.dgb_annotations,
        HashMap::from([(
            "21".to_string(),
            UseDefList {
                decls: Vec::new(),
                defs: vec![276..279],
                uses: vec![239..242]
            }
        )])
    );
}
