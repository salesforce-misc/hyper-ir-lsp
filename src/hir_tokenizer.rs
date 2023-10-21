use chumsky::prelude::*;
use chumsky::text::Character;
use chumsky::Parser;
use core::fmt;

pub type Span = std::ops::Range<usize>;
pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Comment,              // Started by `#`
    Num(String),          // Numbers
    HexNum(String),       // Hexadecimal numbers
    Str(String),          // Strings
    LocalName(String),    // Local name, starting with `%`
    GlobalName(String),   // Global name, starting with `@`
    DebugRef(String),     // `!134` at end of line
    Type(String),         // Types (int32, void, etc.)
    Ident(String),        // Any other identifier
    FuncModifier(String), // Function modifiers
    Punctuation(char),    // Operators; pretty much all punctuation
    Newline,
    Declare,
    Define,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Comment => write!(f, "comment"),
            Token::DebugRef(x) => write!(f, "!{}", x),
            Token::Num(s) => write!(f, "{}", s),
            Token::HexNum(s) => write!(f, "{}", s),
            Token::Str(s) => write!(f, "{}", s),
            Token::LocalName(s) => write!(f, "%{}", s),
            Token::GlobalName(c) => write!(f, "@{}", c),
            Token::Type(s) => write!(f, "{}", s),
            Token::Ident(s) => write!(f, "{}", s),
            Token::FuncModifier(s) => write!(f, "{}", s),
            Token::Punctuation(s) => write!(f, "{}", s),
            Token::Newline => write!(f, "\\n"),
            Token::Declare => write!(f, "declare"),
            Token::Define => write!(f, "define"),
        }
    }
}

pub fn tokenizer() -> impl Parser<char, Vec<Spanned<Token>>, Error = Simple<char>> {
    // A newline parser
    let newline = just('\n')
        .or(just('\r').then_ignore(just('\n')))
        .to(Token::Newline);

    // A comment parser
    let comment = just('#')
        .chain(take_until(just('\n').or(end().to('\n')).rewind()))
        .to(Token::Comment);

    // A parser for hex numbers
    let hexnum = just("0x").ignore_then(text::int(16)).map(Token::HexNum);

    // A parser for numbers
    let num = text::int(10).map(Token::Num);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(filter(|c| *c != '"').repeated())
        .then_ignore(just('"'))
        .collect::<String>()
        .map(Token::Str);

    // A parser for punctuation / control characters
    let punctuation = one_of("[]{}()<>=,;:*").map(Token::Punctuation);

    // An identifier starts with an alphabetic character or an underscore
    let ident_head = filter(|c: &char| c.is_alphabetic() || c.to_char() == '_');

    // An identifier can contain an alphanumeric char or an underscore in the middle or at the end
    let ident_char = filter(|c: &char| c.is_alphanumeric() || c.to_char() == '_');

    // In addition, an identifier can also contain ':', but no trailing `:`
    let ident_tail = (just(':').repeated().then(ident_char)).repeated();

    // A parser for identifiers, keywords and type names
    let ident = ident_head
        .chain(ident_tail)
        .collect()
        .map(|ident: String| match ident.as_str() {
            "declare" => Token::Declare,
            "define" => Token::Define,
            "exported" => Token::FuncModifier(ident),
            "async" => Token::FuncModifier(ident),
            "memberfunc" => Token::FuncModifier(ident),
            "void" => Token::Type(ident),
            "int1" => Token::Type(ident),
            "int8" => Token::Type(ident),
            "int16" => Token::Type(ident),
            "int32" => Token::Type(ident),
            "int64" => Token::Type(ident),
            "float64" => Token::Type(ident),
            "data128" => Token::Type(ident),
            "ptr" => Token::Type(ident),
            _ => Token::Ident(ident),
        });

    // A parser for local variables
    let local = just('%')
        .chain::<char, _, _>(ident_tail)
        .collect::<String>()
        .map(Token::LocalName);

    // A parser for global variables
    let global = just('@')
        .chain::<char, _, _>(ident_tail)
        .collect::<String>()
        .map(Token::GlobalName);

    // A parser for debug references
    let debugref = just('!').ignore_then(text::digits(10)).map(Token::DebugRef);

    // A single token can be one of the above
    let token = choice((
        comment,
        hexnum,
        num,
        str_,
        punctuation,
        ident,
        global,
        local,
        debugref,
        newline,
    ))
    .recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|tok, span| (tok, span))
        .padded_by(one_of(" \t").repeated())
        .repeated()
        .then_ignore(end())
}

#[test]
fn test_tokenizer() {
    // Our parser accepts empty strings
    assert_eq!(tokenizer().parse(""), Ok(Vec::from([])));

    let tokens_only = |e: &str| {
        tokenizer()
            .parse(e)
            .map(|v| v.iter().map(|e| e.0.clone()).collect::<Vec<_>>())
    };

    // Numbers
    assert_eq!(
        tokens_only("123"),
        Ok(Vec::from([Token::Num("123".to_string())]))
    );
    assert_eq!(
        tokens_only("0xDead0123beef"),
        Ok(Vec::from([Token::HexNum("Dead0123beef".to_string())]))
    );

    // Strings
    assert_eq!(
        tokens_only("\"abc\""),
        Ok(Vec::from([Token::Str("abc".to_string())]))
    );
    // Empty string
    assert_eq!(
        tokens_only("\"\""),
        Ok(Vec::from([Token::Str("".to_string())]))
    );

    // Local name; including `:` and `_`
    assert_eq!(
        tokens_only("%abc::def_foo"),
        Ok(Vec::from([Token::LocalName("%abc::def_foo".to_string())]))
    );
    // We also accept empty, unnamed local variables
    assert_eq!(
        tokens_only("%"),
        Ok(Vec::from([Token::LocalName("%".to_string())]))
    );

    // Linux newlines
    assert_eq!(
        tokens_only("\n\n"),
        Ok(Vec::from([Token::Newline, Token::Newline]))
    );
    // Windows newlines
    assert_eq!(
        tokens_only("\r\n\r\n"),
        Ok(Vec::from([Token::Newline, Token::Newline]))
    );

    // Global name; including `:` and `_`
    assert_eq!(
        tokens_only("@abc::def_foo"),
        Ok(Vec::from([Token::GlobalName("@abc::def_foo".to_string())]))
    );

    // Debug ref
    assert_eq!(
        tokens_only("!123"),
        Ok(Vec::from([Token::DebugRef("123".to_string())]))
    );

    // Types
    assert_eq!(
        tokens_only("void"),
        Ok(Vec::from([Token::Type("void".to_string())]))
    );
    assert_eq!(
        tokens_only("ptr"),
        Ok(Vec::from([Token::Type("ptr".to_string())]))
    );

    // Identifier
    assert_eq!(
        tokens_only("ptr234"),
        Ok(Vec::from([Token::Ident("ptr234".to_string())]))
    );
    // Single character identifier
    assert_eq!(
        tokens_only("x"),
        Ok(Vec::from([Token::Ident("x".to_string())]))
    );
    // The trailing `:` is not part of the identifier.
    // Thereby, we can still parse basic block labels
    assert_eq!(
        tokens_only("my_lbl:"),
        Ok(Vec::from([
            Token::Ident("my_lbl".to_string()),
            Token::Punctuation(':'),
        ]))
    );
    assert_eq!(
        tokens_only("my_lbl::"),
        Ok(Vec::from([
            Token::Ident("my_lbl".to_string()),
            Token::Punctuation(':'),
            Token::Punctuation(':'),
        ]))
    );

    // Function modifiers
    assert_eq!(
        tokens_only("exported"),
        Ok(Vec::from([Token::FuncModifier("exported".to_string())]))
    );
    assert_eq!(
        tokens_only("async"),
        Ok(Vec::from([Token::FuncModifier("async".to_string())]))
    );
    assert_eq!(
        tokens_only("memberfunc"),
        Ok(Vec::from([Token::FuncModifier("memberfunc".to_string())]))
    );

    // Key words
    assert_eq!(tokens_only("declare"), Ok(Vec::from([Token::Declare])));
    assert_eq!(tokens_only("define"), Ok(Vec::from([Token::Define])));

    assert_eq!(tokens_only(":"), Ok(Vec::from([Token::Punctuation(':')])));

    // Usually, comments are terminated by a line break. The line break is a separate token
    assert_eq!(
        tokens_only("# comment\n"),
        Ok(Vec::from([Token::Comment, Token::Newline]))
    );
    // A comment might not be terminated by a line break
    assert_eq!(tokens_only("# comment"), Ok(Vec::from([Token::Comment])));

    // Multiple tokens
    assert_eq!(
        tokens_only("declare void @foo(ptr %1) # comment"),
        Ok(Vec::from([
            Token::Declare,
            Token::Type("void".to_string()),
            Token::GlobalName("@foo".to_string()),
            Token::Punctuation('('),
            Token::Type("ptr".to_string()),
            Token::LocalName("%1".to_string()),
            Token::Punctuation(')'),
            Token::Comment,
        ]))
    );
}
