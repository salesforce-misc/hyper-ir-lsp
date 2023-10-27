use chumsky::prelude::Simple;
use ropey::Rope;
use tower_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, Location, Url,
};

use crate::{
    hir_index::HIRIndex,
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

pub fn diagnostics_from_index<'a>(
    rope: &'a Rope,
    uri: &'a Url,
    index: &'a HIRIndex,
) -> impl Iterator<Item = Diagnostic> + 'a {
    let all_use_defs = index
        .global_vars
        .iter()
        .chain(index.functions.iter())
        .chain(index.dgb_annotations.iter())
        .chain(index.function_bodies.iter().flat_map(|fb| fb.labels.iter()))
        .chain(
            index
                .function_bodies
                .iter()
                .flat_map(|fb| fb.local_vars.iter()),
        );

    all_use_defs
        .filter_map(move |(name, ud)| {
            // Diagnose all symbols which are used but not defined / declared
            if ud.decls.is_empty() && ud.defs.is_empty() {
                let diags = ud
                    .uses
                    .iter()
                    .filter_map(move |use_| {
                        let message = format!("Use of undefined symbol `{}`", name);
                        Some(Diagnostic::new_simple(range_to_lsp(rope, use_)?, message))
                    })
                    .collect::<Vec<_>>();
                Some(diags)
            } else if ud.defs.len() > 1 {
                let diags = ud
                    .defs
                    .iter()
                    .skip(1)
                    .filter_map(move |use_| {
                        let message = format!("Symbol `{}` already defined previously", name);
                        let related_information = Some(vec![DiagnosticRelatedInformation {
                            location: Location {
                                uri: uri.clone(),
                                range: range_to_lsp(rope, &ud.defs[0])?,
                            },
                            message: "Previously defined here".to_string(),
                        }]);
                        Some(Diagnostic {
                            range: range_to_lsp(rope, use_)?,
                            message,
                            severity: None,
                            code: None,
                            code_description: None,
                            source: None,
                            related_information,
                            tags: None,
                            data: None,
                        })
                    })
                    .collect::<Vec<_>>();
                Some(diags)
            } else {
                None
            }
        })
        .flatten()
}
