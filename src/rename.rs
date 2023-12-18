use ropey::Rope;
use tower_lsp::lsp_types::TextEdit;

use crate::{
    hir_index::{HIRIndex, SymbolKind, UseDefList},
    lsp_utils::range_to_lsp,
};

pub fn extract_number_from_identifier(id: &str) -> Option<(&str, i64)> {
    let offset = id
        .rfind(|c: char| !c.is_numeric())
        .map(|i| i + 1)
        .unwrap_or(0);
    let (name, nr) = id.split_at(offset);
    Some((name, nr.parse::<i64>().ok()?))
}

pub fn get_rename_edits(rope: &Rope, usedefs: &UseDefList, new_name: &str) -> Vec<TextEdit> {
    usedefs
        .decls
        .iter()
        .chain(usedefs.defs.iter())
        .chain(usedefs.uses.iter())
        .filter_map(move |range| {
            Some(TextEdit {
                range: range_to_lsp(rope, range)?,
                new_text: new_name.to_string(),
            })
        })
        .collect()
}

pub fn get_shift_edits(
    rope: &Rope,
    index: &HIRIndex,
    symbol_kind: SymbolKind,
    func_body_id: Option<usize>,
    renamed_nr: i64,
    shift: i64,
) -> Vec<TextEdit> {
    index
        .get_by_symbol_kind(
            symbol_kind,
            func_body_id.map(|id| &index.function_bodies[id]),
        )
        .iter()
        .filter_map(|(name, usedefs)| {
            let (prefix, nr) = extract_number_from_identifier(name)?;
            if nr < renamed_nr {
                return None;
            }
            let new_name = format!("{}{}", prefix, (nr + shift).wrapping_abs());
            Some(get_rename_edits(rope, usedefs, &new_name))
        })
        .flatten()
        .collect::<Vec<_>>()
}

#[test]
fn test_extract_number_from_identifier() {
    assert_eq!(
        extract_number_from_identifier("%test_1"),
        Some(("%test_", 1))
    );
    assert_eq!(
        extract_number_from_identifier("%test991"),
        Some(("%test", 991))
    );
    assert_eq!(extract_number_from_identifier("%v30"), Some(("%v", 30)));
    assert_eq!(
        extract_number_from_identifier("%v30a12"),
        Some(("%v30a", 12))
    );
    assert_eq!(extract_number_from_identifier("%v30a"), None);
    assert_eq!(extract_number_from_identifier("!123"), Some(("!", 123)));
    assert_eq!(extract_number_from_identifier("123"), Some(("", 123)));
    assert_eq!(extract_number_from_identifier("%test"), None);
}
