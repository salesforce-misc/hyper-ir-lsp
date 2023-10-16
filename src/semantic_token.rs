use ropey::Rope;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

use crate::hir_tokenizer::{Span, Token};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
    SemanticTokenType::COMMENT,
    SemanticTokenType::TYPE,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::STRING,
    SemanticTokenType::NUMBER,
    SemanticTokenType::KEYWORD,
    SemanticTokenType::MODIFIER,
];

#[derive(Debug)]
pub struct HIRSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}

pub fn create_semantic_token(span: &Span, ttype: &SemanticTokenType) -> HIRSemanticToken {
    HIRSemanticToken {
        start: span.start,
        length: span.len(),
        token_type: LEGEND_TYPE.iter().position(|item| item == ttype).unwrap(),
    }
}

/// Creates semantic tokens from the lexer tokens
pub fn semantic_tokens_from_tokens(tokens: Vec<(Token, Span)>) -> Vec<HIRSemanticToken> {
    tokens
        .iter()
        .filter_map(|(token, span)| match token {
            Token::Newline => None,
            Token::Comment => Some(create_semantic_token(span, &SemanticTokenType::COMMENT)),
            Token::Num(_) => Some(create_semantic_token(span, &SemanticTokenType::NUMBER)),
            Token::HexNum(_) => Some(create_semantic_token(span, &SemanticTokenType::NUMBER)),
            Token::Str(_) => Some(create_semantic_token(span, &SemanticTokenType::STRING)),
            Token::LocalName(_) => Some(create_semantic_token(span, &SemanticTokenType::VARIABLE)),
            Token::GlobalName(_) => Some(create_semantic_token(span, &SemanticTokenType::VARIABLE)),
            Token::Ident(_) => None,
            Token::DebugRef(_) => None,
            Token::Type(_) => Some(create_semantic_token(span, &SemanticTokenType::TYPE)),
            Token::Punctuation(_) => None,
            Token::Declare => Some(create_semantic_token(span, &SemanticTokenType::KEYWORD)),
            Token::Define => Some(create_semantic_token(span, &SemanticTokenType::KEYWORD)),
            Token::FuncModifier(_) => {
                Some(create_semantic_token(span, &SemanticTokenType::MODIFIER))
            }
        })
        .collect::<Vec<_>>()
}

// Converts our internal semantic tokens to the LSP representation of tokens
pub fn convert_to_lsp_tokens(rope: &Rope, semtoks: &[HIRSemanticToken]) -> Vec<SemanticToken> {
    let mut pre_line = 0;
    let mut pre_start = 0;
    let lsp_tokens = semtoks
        .iter()
        .filter_map(|token| {
            let line = rope.try_byte_to_line(token.start).ok()? as u32;
            let first = rope.try_line_to_char(line as usize).ok()? as u32;
            let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
            let delta_line = line - pre_line;
            let delta_start = if delta_line == 0 {
                start - pre_start
            } else {
                start
            };
            let ret = Some(SemanticToken {
                delta_line,
                delta_start,
                length: token.length as u32,
                token_type: token.token_type as u32,
                token_modifiers_bitset: 0,
            });
            pre_line = line;
            pre_start = start;
            ret
        })
        .collect::<Vec<_>>();
    lsp_tokens
}

/*
pub fn semantic_token_from_ast(ast: &HashMap<String, Func>) -> Vec<ImCompleteSemanticToken> {
    let mut semantic_tokens = vec![];

    ast.iter().for_each(|(_func_name, function)| {
        function.args.iter().for_each(|(_, span)| {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: span.start,
                length: span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::PARAMETER)
                    .unwrap(),
            });
        });
        let (_, span) = &function.name;
        semantic_tokens.push(ImCompleteSemanticToken {
            start: span.start,
            length: span.len(),
            token_type: LEGEND_TYPE
                .iter()
                .position(|item| item == &SemanticTokenType::FUNCTION)
                .unwrap(),
        });
        semantic_token_from_expr(&function.body, &mut semantic_tokens);
    });

    semantic_tokens
}

pub fn semantic_token_from_expr(
    expr: &Spanned<Expr>,
    semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
    match &expr.0 {
        Expr::Error => {}
        Expr::Value(_) => {}
        Expr::List(_) => {}
        Expr::Local((_name, span)) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: span.start,
                length: span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
        }
        Expr::Let(_, rhs, rest, name_span) => {
            semantic_tokens.push(ImCompleteSemanticToken {
                start: name_span.start,
                length: name_span.len(),
                token_type: LEGEND_TYPE
                    .iter()
                    .position(|item| item == &SemanticTokenType::VARIABLE)
                    .unwrap(),
            });
            semantic_token_from_expr(rhs, semantic_tokens);
            semantic_token_from_expr(rest, semantic_tokens);
        }
        Expr::Then(first, rest) => {
            semantic_token_from_expr(first, semantic_tokens);
            semantic_token_from_expr(rest, semantic_tokens);
        }
        Expr::Binary(lhs, _op, rhs) => {
            semantic_token_from_expr(lhs, semantic_tokens);
            semantic_token_from_expr(rhs, semantic_tokens);
        }
        Expr::Call(expr, params) => {
            semantic_token_from_expr(expr, semantic_tokens);
            params.0.iter().for_each(|p| {
                semantic_token_from_expr(p, semantic_tokens);
            });
        }
        Expr::If(test, consequent, alternative) => {
            semantic_token_from_expr(test, semantic_tokens);
            semantic_token_from_expr(consequent, semantic_tokens);
            semantic_token_from_expr(alternative, semantic_tokens);
        }
        Expr::Print(expr) => {
            semantic_token_from_expr(expr, semantic_tokens);
        }
    }
}
 */
