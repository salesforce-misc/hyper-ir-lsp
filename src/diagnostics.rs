use chumsky::prelude::Simple;
use ropey::Rope;
use tower_lsp::lsp_types::Diagnostic;

use crate::{
    hir_parser::{FuncBody, Statement},
    lsp_utils::range_to_lsp,
};

pub fn diagnostics_from_parser<'a>(
    rope: &'a Rope,
    errors: &'a [Simple<String>],
) -> impl Iterator<Item = Diagnostic> + 'a {
    errors.iter().filter_map(move |item| {
        let (message, span) = match item.reason() {
            chumsky::error::SimpleReason::Unclosed { span, delimiter } => {
                (format!("Unclosed delimiter {}", delimiter), span.clone())
            }
            chumsky::error::SimpleReason::Unexpected => (
                format!(
                    "{}, expected {}",
                    if item.found().is_some() {
                        "Unexpected token in input"
                    } else {
                        "Unexpected end of input"
                    },
                    if item.expected().len() == 0 {
                        "something else".to_string()
                    } else {
                        item.expected()
                            .map(|expected| match expected {
                                Some(expected) => expected.to_string(),
                                None => "end of input".to_string(),
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                ),
                item.span(),
            ),
            chumsky::error::SimpleReason::Custom(msg) => (msg.to_string(), item.span()),
        };

        || -> Option<Diagnostic> {
            Some(Diagnostic::new_simple(range_to_lsp(rope, &span)?, message))
        }()
    })
}

pub fn diagnostics_from_statements<'a>(
    rope: &'a Rope,
    stmts: &'a [Statement],
) -> impl Iterator<Item = Diagnostic> + 'a {
    stmts
        .iter()
        .filter_map(move |s| match s {
            Statement::FuncDef {
                body: FuncBody { basic_blocks, .. },
                ..
            } => Some(basic_blocks.iter()),
            _ => None,
        })
        .flatten()
        .flat_map(move |bb| bb.instructions.iter())
        .filter_map(move |i| {
            let inst_name = &i.instruction.0;
            if (inst_name.ends_with("br") || inst_name == "phi" || inst_name == "switch")
                && i.jump_targets.is_empty()
            {
                let message = format!(
                    "Failed to extract basic block references from `{}` instruction",
                    inst_name
                );
                Some(Diagnostic::new_simple(
                    range_to_lsp(rope, &i.instruction.1)?,
                    message,
                ))
            } else {
                None
            }
        })
}
