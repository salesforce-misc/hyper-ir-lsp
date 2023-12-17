use std::cmp::max;

use tower_lsp::lsp_types::{
    InlayHint, InlayHintLabel, InlayHintLabelPart, InlayHintLabelPartTooltip, MarkupContent,
    MarkupKind, Position, Url,
};

pub fn resolve_relative_path(root_paths: &[Url], path: &str) -> Option<Url> {
    root_paths.iter().find_map(|baseuri| {
        let mut uri = baseuri.clone();
        uri.path_segments_mut().unwrap().extend(path.split('/'));
        if uri.to_file_path().ok()?.exists() {
            Some(uri)
        } else {
            None
        }
    })
}

#[derive(Debug, Clone)]
pub struct Frame {
    pub symbol: String,
    pub filepath: Option<String>,
    pub line: u32,
    pub resolved_filepath: Option<Url>,
}

pub fn parse_backtrace_from_json(
    root_paths: &[Url],
    json: serde_json::Value,
) -> Option<Vec<Frame>> {
    let backtrace = json.get("backtrace")?.as_array()?;
    let bt = backtrace
        .iter()
        .map(|frame| {
            // The first array entry is the symbol name
            let json_name = frame.get(0).and_then(|e| e.as_str());
            // The second array entry is the `file:linenr`
            let json_link = frame.get(1).and_then(|s| s.as_str());

            // If the name is empty, fallback to the file name or use `<invalid>`
            let mut name = "<invalid>";
            if json_name.is_some() && !json_name.unwrap().is_empty() {
                // Use the name provided in the backtrace if present
                name = json_name.unwrap();
            } else if let Some(link) = json_link {
                let separator_pos = max(link.rfind('/'), link.rfind('\\'));
                name = link.split_at(separator_pos.map(|p| p + 1).unwrap_or(0)).1;
            }

            // Parse `file:linenr`
            let filepath_linenr = json_link.and_then(|s| -> Option<(String, u32)> {
                if let [filepath, linestr] = s.split(':').collect::<Vec<_>>()[..] {
                    if let Ok(line) = linestr.parse::<u32>() {
                        return Some((filepath.to_string(), line));
                    }
                }
                None
            });

            // Resolve the file path against the workspace roots
            let resolved_filepath = filepath_linenr
                .as_ref()
                .and_then(|f| resolve_relative_path(root_paths, &f.0));

            let line = filepath_linenr.as_ref().map(|f| f.1).unwrap_or(0);
            Frame {
                symbol: name.to_string(),
                filepath: filepath_linenr.map(|f| f.0),
                resolved_filepath,
                line,
            }
        })
        .collect::<Vec<_>>();
    Some(bt)
}

pub fn identify_relevant_frames(frames: &[Frame]) -> Option<usize> {
    // Finding a `Translator::consume` or `drivePipeline` would be the best case
    let symbol_preferences = [
        "Translator::consume",
        "Translator::produceMaterializedResult",
        "Translator::drivePipeline",
        "Translator::deriveValue",
        "Translator::",
    ];
    for pref in symbol_preferences {
        let pref_frame = frames.iter().position(|f| f.symbol.contains(pref));
        if pref_frame.is_some() {
            return pref_frame;
        }
    }

    // As a fallback, find the first, non-internal frame. We consider everything inside the
    // `hyper/ir` and the `hyper/codegen` folder as internal. Those are our codegen
    // utilities.
    frames.iter().position(|f| match &f.filepath {
        Some(fp) => !fp.contains("hyper/ir/") && !fp.contains("hyper/codegen"),
        None => false,
    })
}

// Removes all potentially dangerous characters form a string which we are
// about to embed into Markdown
fn sanitize_markdown(txt: &str) -> String {
    let mut sanitized = String::with_capacity(txt.bytes().len());
    for c in txt.chars() {
        if c.is_alphanumeric() || c.is_whitespace() {
            // We allow all alphanumeric chars.
            // Hopefully, none of them has a special meaning in Markdown.
            sanitized.push(c);
        } else {
            match c {
                // Forward characters which are safe in Markdown
                '.' => sanitized.push(c),
                ',' => sanitized.push(c),
                ':' => sanitized.push(c),
                '+' => sanitized.push(c),
                '-' => sanitized.push(c),
                '=' => sanitized.push(c),
                '/' => sanitized.push(c),
                // Characters which need to be escaped
                '*' => sanitized.push_str("\\*"),
                '<' => sanitized.push_str("&lt;"),
                '>' => sanitized.push_str("&gt;"),
                '_' => sanitized.push_str("\\_"),
                // Unsupported characters are replaced
                _ => sanitized.push('�'),
            }
        }
    }
    sanitized
}

pub fn backtrace_json_to_md(frames: &[Frame]) -> String {
    frames
        .iter()
        .map(|frame| format!("* {}", sanitize_markdown(&frame.symbol)))
        .collect::<Vec<_>>()
        .join("\n")
}

fn shorten_name(s: &str, len: usize) -> String {
    if s.len() < len - 2 {
        return s.to_string();
    } else {
        return s[..len - 2].to_string() + "…";
    }
}

pub fn inlay_hint_for_backtrace(pos: Position, frames: &[Frame]) -> InlayHint {
    // Extract all interesting frames
    let mut inlay_hint_parts: Vec<InlayHintLabelPart> = Vec::new();
    if let Some(idx) = identify_relevant_frames(&frames) {
        let frame = &frames[idx];
        let symbol = shorten_name(&frame.symbol,50);
        let tooltip = if symbol == frame.symbol {
            None
        } else {
            Some(InlayHintLabelPartTooltip::String(frame.symbol.clone()))
        };
        inlay_hint_parts.push(InlayHintLabelPart {
            value: symbol.to_string(),
            tooltip,
            location: None,
            command: None,
        });
        inlay_hint_parts.push(InlayHintLabelPart {
            value: ", ".to_string(),
            tooltip: None,
            location: None,
            command: None,
        });
    }

    // Put the full backtrace into a tooltip
    let tooltip_md = backtrace_json_to_md(&frames);
    let tooltip = MarkupContent {
        kind: MarkupKind::Markdown,
        value: tooltip_md,
    };
    inlay_hint_parts.push(InlayHintLabelPart {
        value: "Full backtrace".to_string(),
        tooltip: Some(InlayHintLabelPartTooltip::MarkupContent(tooltip)),
        location: None,
        command: None,
    });

    InlayHint {
        position: pos,
        label: InlayHintLabel::LabelParts(inlay_hint_parts),
        kind: None,
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: Some(true),
        data: None,
    }
}

#[cfg(test)]
fn parse_test_frame(txt: &str) -> Frame {
    let wrapped = r#"{"backtrace": ["#.to_string() + txt + r#"]}"#;
    let json_val = serde_json::from_str::<serde_json::Value>(&wrapped).unwrap();
    let mut bt = parse_backtrace_from_json(&vec![], json_val).unwrap();
    assert_eq!(bt.len(), 1);
    return bt.remove(0);
}

#[test]
fn parse_frame_with_filepath() {
    let frame = parse_test_frame(r#"["myFunc", "./my/File.cpp:11"]"#);
    assert_eq!(frame.symbol, "myFunc");
    assert_eq!(frame.filepath, Some("./my/File.cpp".to_string()));
    assert_eq!(frame.line, 11);
}

#[test]
fn parse_frame_without_filepath() {
    // The file name is optional. We should still pick up the symbol name
    let frame = parse_test_frame(r#"["myFunc"]"#);
    assert_eq!(frame.symbol, "myFunc");
    assert_eq!(frame.filepath, None);
    assert_eq!(frame.line, 0);
}

#[test]
fn parse_frame_without_symbol() {
    // If the symbol name is empty, we are using the file name
    // instead (without the file path)
    let frame = parse_test_frame(r#"["", "./my/File.cpp:11"]"#);
    assert_eq!(frame.symbol, "File.cpp:11");
    assert_eq!(frame.filepath, Some("./my/File.cpp".to_string()));
    assert_eq!(frame.line, 11);
    // We also correctly detect windows path separators
    let frame2 = parse_test_frame(r#"["", ".\\my\\File.cpp:11"]"#);
    assert_eq!(frame2.symbol, "File.cpp:11");
    assert_eq!(frame2.filepath, Some(".\\my\\File.cpp".to_string()));
    assert_eq!(frame2.line, 11);
    // Also fallback if we received some invalid symbol name, e.g. a `null`
    let frame3 = parse_test_frame(r#"[null, "./my/File.cpp:11"]"#);
    assert_eq!(frame3.symbol, "File.cpp:11");
    // ... or a number
    let frame4 = parse_test_frame(r#"[123, "./my/File.cpp:11"]"#);
    assert_eq!(frame4.symbol, "File.cpp:11");
}

#[test]
fn parse_invaid_frame() {
    // Completely empty frame
    let frame = parse_test_frame(r#"[]"#);
    assert_eq!(frame.symbol, "<invalid>");
    assert_eq!(frame.filepath, None);
    assert_eq!(frame.line, 0);
    // Unexpected types
    let frame = parse_test_frame(r#"[null, null]"#);
    assert_eq!(frame.symbol, "<invalid>");
    assert_eq!(frame.filepath, None);
    assert_eq!(frame.line, 0);
    // Empty symbol and no file location
    let frame = parse_test_frame(r#"[""]"#);
    assert_eq!(frame.symbol, "<invalid>");
    assert_eq!(frame.filepath, None);
    assert_eq!(frame.line, 0);
}

#[test]
fn parse_multi_frame_backtrace() {
    let json_val = serde_json::from_str::<serde_json::Value>(
        r#"{"backtrace": [["myFunc", "./my/File.cpp:11"], ["yourFunc", "./your/File.cpp:12"]]}"#,
    )
    .unwrap();
    let bt = parse_backtrace_from_json(&vec![], json_val).unwrap();
    // The first entry has a valid symbol name. Use it.
    // The file path can't be found in the root_paths, though.
    assert_eq!(bt[0].symbol, "myFunc");
    assert_eq!(bt[0].filepath, Some("./my/File.cpp".to_string()));
    assert_eq!(bt[0].line, 11);
    // The 2nd entry is missing its name.
    // Fall back to the file name (without the full file path).
    assert_eq!(bt[1].symbol, "yourFunc");
    assert_eq!(bt[1].filepath, Some("./your/File.cpp".to_string()));
    assert_eq!(bt[1].line, 12);
}
