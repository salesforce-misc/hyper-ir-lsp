use std::cell::Cell;
use std::collections::HashMap;
use std::sync::Mutex;

use dashmap::DashMap;
use hyper_ir_lsp::backtrace::{
    inlay_hint_for_backtrace, parse_backtrace_from_json, resolve_relative_path,
};
use hyper_ir_lsp::control_flow_graph::create_cfg_dot_visualization;
use hyper_ir_lsp::diagnostics::{
    diagnostics_from_index, diagnostics_from_parser, diagnostics_from_statements,
};
use hyper_ir_lsp::hir_index::{create_index, HIRIndex, SymbolOccurrence, UseDefKind, UseDefList};
use hyper_ir_lsp::hir_parser::{parse_from_str, BasicBlock, Instruction, ParserResult, Statement};
use hyper_ir_lsp::lsp_utils::{lsp_pos_to_offset, offset_to_lsp_pos, range_to_lsp};
use hyper_ir_lsp::rename::{extract_number_from_identifier, get_rename_edits, get_shift_edits};
use hyper_ir_lsp::semantic_token::{
    convert_to_lsp_tokens, semantic_tokens_from_tokens, HIRSemanticToken, LEGEND_TYPE,
};
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::{Error, ErrorCode, Result};
use tower_lsp::lsp_types::request::{GotoDeclarationResponse, Request};
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct AnalyzedDocument {
    rope: Rope,
    semantic_tokens: Vec<HIRSemanticToken>,
    stmts: Vec<Statement>,
    index: HIRIndex,
}

#[derive(Debug)]
struct Backend {
    client: Client,
    root_paths: Mutex<Vec<Url>>,
    code_actions_lazy_resolve: Mutex<Cell<bool>>,
    document_map: DashMap<String, AnalyzedDocument>,
}

#[derive(Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
enum CodeActionData {
    ShiftNumbering {
        uri: Url,
        symbol_kind: hyper_ir_lsp::hir_index::SymbolKind,
        func_body_id: Option<usize>,
        renamed_nr: u32,
    },
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        if let Some(workspace_folders) = params.workspace_folders {
            self.root_paths
                .lock()
                .unwrap()
                .extend(workspace_folders.iter().map(|f| f.uri.clone()));
        } else if let Some(root_uri) = params.root_uri {
            self.root_paths.lock().unwrap().push(root_uri);
        }
        let code_actions_lazy_resolve = params
            .capabilities
            .text_document
            .and_then(|c| c.code_action)
            .map(|c| {
                let supports_resolve = c
                    .resolve_support
                    .map(|c| c.properties.contains(&"edit".to_string()))
                    .unwrap_or(false);
                let supports_data = c.data_support.unwrap_or(false);
                supports_resolve && supports_data
            })
            .unwrap_or(false);
        self.code_actions_lazy_resolve
            .lock()
            .unwrap()
            .set(code_actions_lazy_resolve);

        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Hyper IR Language Server".to_string(),
                version: Some("1".to_string()),
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("hir".to_string()),
                                        scheme: None,
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: None,
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                document_symbol_provider: Some(OneOf::Left(true)),
                definition_provider: Some(OneOf::Left(true)),
                declaration_provider: Some(DeclarationCapability::Simple(true)),
                references_provider: Some(OneOf::Left(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: None,
                }),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: Some(true),
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                })),
                code_action_provider: Some(CodeActionProviderCapability::Options(
                    CodeActionOptions {
                        code_action_kinds: None,
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                        resolve_provider: Some(code_actions_lazy_resolve),
                    },
                )),
                execute_command_provider: Some(ExecuteCommandOptions {
                    commands: vec!["visualize-cfg".to_string()],
                    ..Default::default()
                }),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let lazy = self.code_actions_lazy_resolve.lock().unwrap().get();
        self.client
            .log_message(MessageType::INFO, format!("initialized (lazy: {})", lazy))
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let definition = || -> Option<GotoDefinitionResponse> {
            let (origin_selection_range, mut locations) =
                self.get_use_def_ranges(&params.text_document_position_params, UseDefKind::Def)?;
            let links = locations
                .drain(..)
                .map(|loc| LocationLink {
                    origin_selection_range: Some(origin_selection_range),
                    target_uri: loc.uri,
                    target_range: loc.range,
                    target_selection_range: loc.range,
                })
                .collect::<Vec<_>>();
            Some(GotoDefinitionResponse::Link(links))
        }();
        Ok(definition)
    }

    async fn goto_declaration(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let decl = || -> Option<GotoDeclarationResponse> {
            let (origin_selection_range, mut locations) =
                self.get_use_def_ranges(&params.text_document_position_params, UseDefKind::Decl)?;
            let links = locations
                .drain(..)
                .map(|loc| LocationLink {
                    origin_selection_range: Some(origin_selection_range),
                    target_uri: loc.uri,
                    target_range: loc.range,
                    target_selection_range: loc.range,
                })
                .collect::<Vec<_>>();
            Some(GotoDeclarationResponse::Link(links))
        }();
        Ok(decl)
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        let reference_list = self
            .get_use_def_ranges(&params.text_document_position, UseDefKind::Use)
            .map(|x| x.1);
        Ok(reference_list)
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        let lsp_tokens = || -> Option<Vec<SemanticToken>> {
            let doc = self.document_map.get(&uri)?;
            let lsp_tokens = convert_to_lsp_tokens(&doc.rope, &doc.semantic_tokens);
            Some(lsp_tokens)
        }();
        if let Some(semantic_token) = lsp_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri.to_string();
        let symbols = || -> Option<DocumentSymbolResponse> {
            let doc = self.document_map.get(&uri)?;
            let mut symbols = Vec::<DocumentSymbol>::new();

            fn get_def_symbols<'a>(
                ud: &'a HashMap<String, UseDefList>,
                rope: &'a Rope,
                kind: SymbolKind,
            ) -> impl Iterator<Item = tower_lsp::lsp_types::DocumentSymbol> + 'a {
                ud.iter().flat_map(move |f| {
                    f.1.defs.iter().filter_map(move |def| {
                        #[allow(deprecated)] // https://github.com/rust-lang/rust/issues/102777
                        Some(DocumentSymbol {
                            name: f.0.to_string(),
                            detail: None,
                            kind,
                            tags: None,
                            range: range_to_lsp(rope, def)?,
                            selection_range: range_to_lsp(rope, def)?,
                            deprecated: None,
                            children: None,
                        })
                    })
                })
            }

            symbols.extend(get_def_symbols(
                &doc.index.global_vars,
                &doc.rope,
                SymbolKind::CONSTANT,
            ));

            symbols.extend(doc.index.function_bodies.iter().filter_map(|f| {
                #[allow(deprecated)] // https://github.com/rust-lang/rust/issues/102777
                Some(DocumentSymbol {
                    name: f.name.0.clone(),
                    detail: None,
                    kind: SymbolKind::FUNCTION,
                    tags: None,
                    range: range_to_lsp(&doc.rope, &f.complete_range)?,
                    selection_range: range_to_lsp(&doc.rope, &f.name.1)?,
                    deprecated: None,
                    children: None,
                    // We could add the labels as children of the functions.
                    // I decided against it, because it looks a bit too crowded.
                    // children: Some(get_def_symbols(&f.labels, &rope, SymbolKind::KEY).collect::<Vec<_>>()),
                })
            }));

            Some(DocumentSymbolResponse::from(symbols))
        }();

        Ok(symbols)
    }

    async fn prepare_rename(
        &self,
        pos: TextDocumentPositionParams,
    ) -> Result<Option<PrepareRenameResponse>> {
        Ok(|| -> Option<PrepareRenameResponse> {
            let uri_str = pos.text_document.uri.to_string();
            let doc = self.document_map.get(&uri_str)?;
            let offset = lsp_pos_to_offset(&doc.rope, &pos.position)?;
            let symbol = doc.index.find_symbol_at_position(offset)?;
            let range = range_to_lsp(&doc.rope, &symbol.span);
            Some(PrepareRenameResponse::Range(range?))
        }())
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        // Find the document
        let pos = params.text_document_position;
        let uri_str = pos.text_document.uri.to_string();
        let doc = self
            .document_map
            .get(&uri_str)
            .ok_or_else(|| Error::invalid_params("Document not found"))?;

        // Find the symbol under the cursor
        let symbol = (|| -> Option<&SymbolOccurrence> {
            let offset = lsp_pos_to_offset(&doc.rope, &pos.position)?;
            doc.index.find_symbol_at_position(offset)
        })()
        .ok_or_else(|| Error::invalid_params("No symbol at the given position"))?;

        // Check the new name and make sure it contains the expected prefix
        let prefix = match symbol.symbol_kind {
            hyper_ir_lsp::hir_index::SymbolKind::GlobalVar => "@",
            hyper_ir_lsp::hir_index::SymbolKind::Function => "@",
            hyper_ir_lsp::hir_index::SymbolKind::DbgAnnotation => "!",
            hyper_ir_lsp::hir_index::SymbolKind::Label => "",
            hyper_ir_lsp::hir_index::SymbolKind::LocalVar => "%",
        };
        let mut new_name = params.new_name;
        if let Some(stripped) = new_name.strip_prefix(prefix) {
            new_name = stripped.to_string();
        }
        if new_name
            .chars()
            .any(|c| !c.is_alphanumeric() && c != '_' && c != ':')
        {
            return Result::Err(Error::invalid_params("Name contains invalid character"));
        }
        if new_name.chars().next().unwrap().is_numeric()
            && symbol.symbol_kind != hyper_ir_lsp::hir_index::SymbolKind::DbgAnnotation
        {
            return Result::Err(Error::invalid_params("Name must not start with a digit"));
        }
        new_name = format!("{}{}", prefix, new_name);

        // Rename all occurrences
        let usedefs = doc
            .index
            .get_by_symbol_kind(
                symbol.symbol_kind,
                symbol.func_body_id.map(|id| &doc.index.function_bodies[id]),
            )
            .get(&symbol.name)
            .unwrap();
        let edits = get_rename_edits(&doc.rope, usedefs, &new_name);
        let edit = WorkspaceEdit {
            changes: Some(HashMap::from([(pos.text_document.uri, edits)])),
            ..Default::default()
        };
        Ok(Some(edit))
    }

    async fn code_action(&self, params: CodeActionParams) -> Result<Option<CodeActionResponse>> {
        // Find the document
        let uri_str = params.text_document.uri.to_string();
        let doc = self
            .document_map
            .get(&uri_str)
            .ok_or_else(|| Error::invalid_params("Document not found"))?;

        // We don't support ranges, yet
        let range = params.range;
        if range.start != range.end {
            return Ok(None);
        }

        let mut actions = vec![];
        // The "Increment identifier" action, if available
        (|| -> Option<()> {
            let offset = lsp_pos_to_offset(&doc.rope, &range.start)?;
            let symbol = doc.index.find_symbol_at_position(offset)?;
            let (_, renamed_nr) = extract_number_from_identifier(&symbol.name)?;

            let title = match symbol.symbol_kind {
                hyper_ir_lsp::hir_index::SymbolKind::GlobalVar => "Shift global variable numbering",
                hyper_ir_lsp::hir_index::SymbolKind::Function => "Shift function numbering",
                hyper_ir_lsp::hir_index::SymbolKind::DbgAnnotation => "Shift debug numbering",
                hyper_ir_lsp::hir_index::SymbolKind::Label => "Shift basic block numbering",
                hyper_ir_lsp::hir_index::SymbolKind::LocalVar => "Shift variable numbering",
            }
            .to_string();

            actions.push(CodeAction {
                title,
                data: Some(
                    serde_json::to_value(CodeActionData::ShiftNumbering {
                        uri: params.text_document.uri.clone(),
                        symbol_kind: symbol.symbol_kind,
                        func_body_id: symbol.func_body_id,
                        renamed_nr,
                    })
                    .ok()?,
                ),
                ..Default::default()
            });
            None
        })();

        // Eagerly resolve all edits, if the client doesn't support lazy resolving
        if !self.code_actions_lazy_resolve.lock().unwrap().get() {
            actions = actions
                .drain(..)
                .map(|a| self.do_code_action_resolve(a).ok().unwrap())
                .collect();
        }

        Ok(Some(
            actions
                .drain(..)
                .map(CodeActionOrCommand::CodeAction)
                .collect(),
        ))
    }

    async fn code_action_resolve(&self, action: CodeAction) -> Result<CodeAction> {
        self.do_code_action_resolve(action)
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let folding_ranges = || -> Option<Vec<FoldingRange>> {
            let uri = &params.text_document.uri;
            let uri_str = uri.to_string();
            let doc = self.document_map.get(&uri_str)?;
            let mut folding_ranges = Vec::<FoldingRange>::new();

            folding_ranges.extend(doc.index.function_bodies.iter().filter_map(|f| {
                let collapsed_text = format!("{} basic blocks", f.labels.len());
                let range = range_to_lsp(&doc.rope, &f.complete_range)?;
                Some(FoldingRange {
                    start_line: range.start.line,
                    start_character: None,
                    end_line: range.end.line,
                    end_character: None,
                    kind: None,
                    collapsed_text: Some(collapsed_text),
                })
            }));

            folding_ranges.extend(
                doc.index
                    .function_bodies
                    .iter()
                    .flat_map(|e| e.basic_blocks.iter())
                    .filter_map(|bb| {
                        let collapsed_text = format!("{} instructions", bb.instructions.len());
                        let range = range_to_lsp(&doc.rope, &bb.span)?;
                        Some(FoldingRange {
                            start_line: range.start.line,
                            start_character: None,
                            end_line: range.end.line,
                            end_character: None,
                            kind: None,
                            collapsed_text: Some(collapsed_text),
                        })
                    }),
            );

            Some(folding_ranges)
        }();
        return Ok(folding_ranges);
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let inlay_hints = || -> Option<Vec<InlayHint>> {
            let uri = &params.text_document.uri;
            let uri_str = uri.to_string();
            let doc = self.document_map.get(&uri_str)?;
            let mut inlay_hints: Vec<InlayHint> = Vec::<InlayHint>::new();

            // Insert back references for each basic block which point back to the incoming edges
            inlay_hints.extend(
                doc.index
                    .function_bodies
                    .iter()
                    .flat_map(|f| {
                        f.basic_blocks.iter().filter_map(|bb| {
                            Some((bb, f.incoming_bb_branches.get(&bb.label.as_ref()?.0)?))
                        })
                    })
                    .filter_map(|(bb, incoming_refs)| {
                        let label_parts = incoming_refs
                            .iter()
                            .enumerate()
                            .filter_map(|(idx, incoming)| {
                                Some(vec![
                                    InlayHintLabelPart {
                                        value: (if idx == 0 { "incoming: " } else { ", " })
                                            .to_string(),
                                        ..Default::default()
                                    },
                                    InlayHintLabelPart {
                                        value: incoming.0.clone(),
                                        location: Some(Location {
                                            uri: uri.clone(),
                                            range: range_to_lsp(&doc.rope, &incoming.1)?,
                                        }),
                                        ..Default::default()
                                    },
                                ])
                            })
                            .flatten()
                            .collect::<Vec<_>>();

                        Some(InlayHint {
                            position: offset_to_lsp_pos(
                                &doc.rope,
                                bb.label_comma_span.as_ref()?.end,
                            )?,
                            label: InlayHintLabel::LabelParts(label_parts),
                            kind: None,
                            text_edits: None,
                            tooltip: None,
                            padding_left: Some(true),
                            padding_right: Some(true),
                            data: None,
                        })
                    }),
            );

            // Insert inlay hints for call stacks
            inlay_hints.extend(
                doc.stmts
                    .iter()
                    .filter_map(|s| match s {
                        Statement::FuncDef { body, .. } => {
                            Some::<&Vec<BasicBlock>>(body.basic_blocks.as_ref())
                        }
                        _ => None,
                    })
                    .flatten()
                    .flat_map(|bb| -> &Vec<Instruction> { bb.instructions.as_ref() })
                    .filter_map(|stmt| {
                        let dbg_ref = stmt.dbg_ref.as_ref()?;
                        let json_txt = doc.index.dgb_annotation_values.get(&dbg_ref.0)?;
                        let json_val = serde_json::from_str::<serde_json::Value>(json_txt).ok()?;
                        let root_paths: std::sync::MutexGuard<'_, Vec<Url>> =
                            self.root_paths.lock().unwrap();
                        let frames = parse_backtrace_from_json(&root_paths, json_val)?;
                        Some(inlay_hint_for_backtrace(
                            offset_to_lsp_pos(&doc.rope, dbg_ref.1.end)?,
                            &frames,
                        ))
                    }),
            );

            // Insert hints at the end of a function body which point back to the beginning
            // of the function definition
            inlay_hints.extend(doc.index.function_bodies.iter().filter_map(|f| {
                Some(InlayHint {
                    position: offset_to_lsp_pos(&doc.rope, f.complete_range.end)?,
                    label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                        value: f.name.0.clone(),
                        location: Some(Location {
                            uri: uri.clone(),
                            range: range_to_lsp(&doc.rope, &f.name.1)?,
                        }),
                        ..Default::default()
                    }]),
                    kind: None,
                    text_edits: None,
                    tooltip: None,
                    padding_left: Some(true),
                    padding_right: Some(true),
                    data: None,
                })
            }));

            Some(inlay_hints)
        }();
        return Ok(inlay_hints);
    }

    async fn code_lens(&self, params: CodeLensParams) -> Result<Option<Vec<CodeLens>>> {
        let codelenses = || -> Option<Vec<CodeLens>> {
            let uri = &params.text_document.uri;
            let uri_str = uri.to_string();
            let doc = self.document_map.get(&uri_str)?;

            let codelenses = doc
                .index
                .functions
                .iter()
                .filter_map(|f| {
                    // Only show the code lens for functions with exactly one definition
                    if let [def_range] = &f.1.defs[..] {
                        Some(CodeLens {
                            range: range_to_lsp(&doc.rope, def_range)?,
                            command: Some(Command {
                                // Potential icons: ⇆⭾⧬⌸✍✒✎🧐
                                title: "⭾ Visualize Controlflow".to_string(),
                                command: "visualize-cfg".to_string(),
                                arguments: Some(vec![
                                    Value::String(uri_str.clone()),
                                    Value::String(f.0.clone()),
                                ]),
                            }),
                            data: None,
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            Some(codelenses)
        }();
        return Ok(codelenses);
    }

    async fn execute_command(&self, params: ExecuteCommandParams) -> Result<Option<Value>> {
        match (params.command.as_str(), &params.arguments[..]) {
            ("visualize-cfg", [Value::String(doc_uri), Value::String(func_name)]) => {
                let index = &self
                    .document_map
                    .get(doc_uri)
                    .ok_or_else(|| Error {
                        code: ErrorCode::InvalidParams,
                        message: format!("document `{}` not found", doc_uri).into(),
                        data: None,
                    })?
                    .index;
                let func_body = index
                    .function_bodies
                    .iter()
                    .find(|b| b.name.0 == *func_name)
                    .ok_or_else(|| Error {
                        code: ErrorCode::InvalidParams,
                        message: format!("function `{}` not found", func_name).into(),
                        data: None,
                    })?;

                let title = format!("CFG for {}", func_name);
                let dot_graph = create_cfg_dot_visualization(func_body);

                // Make this dependent on a client setting / client capability
                // since it reuqires additional client-siye collabolation
                self.client
                    .send_request::<ShowGraph>(ShowGraphParams { title, dot_graph })
                    .await
                    .map_err(|e| Error {
                        code: ErrorCode::InternalError,
                        message: format!("Failed displaying the dot graph: `{}`", e).into(),
                        data: None,
                    })?;
                Ok(None)
            }
            _ => Err(Error {
                code: ErrorCode::InvalidParams,
                message: format!("Invalid command `{}`", params.command).into(),
                data: None,
            }),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShowGraphParams {
    title: String,
    dot_graph: String,
}

#[derive(Debug, Eq, PartialEq, Clone, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct ShowGraphResponse {}

/// Custom command to show a dot graph.
#[derive(Debug)]
pub enum ShowGraph {}

impl Request for ShowGraph {
    type Params = ShowGraphParams;
    type Result = ShowGraphResponse;
    const METHOD: &'static str = "hyperir/showDot";
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        let src = rope.to_string();

        let ParserResult {
            tokens,
            stmts,
            errors,
        } = parse_from_str(&src);
        let semantic_tokens = semantic_tokens_from_tokens(&tokens);
        let index = create_index(&src, &tokens, &stmts);

        let mut diagnostics = Vec::<Diagnostic>::new();
        diagnostics.extend(diagnostics_from_parser(&rope, &errors));
        diagnostics.extend(diagnostics_from_statements(&rope, &stmts));
        diagnostics.extend(diagnostics_from_index(&rope, &params.uri, &index));

        self.document_map.insert(
            params.uri.to_string(),
            AnalyzedDocument {
                rope,
                semantic_tokens,
                stmts,
                index,
            },
        );

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }

    fn get_use_def_ranges(
        &self,
        pos: &TextDocumentPositionParams,
        ud: UseDefKind,
    ) -> Option<(Range, Vec<Location>)> {
        let uri_str = pos.text_document.uri.to_string();
        let doc = self.document_map.get(&uri_str)?;

        // Lookup the symbol at the given location
        let offset = lsp_pos_to_offset(&doc.rope, &pos.position)?;
        let symbol = doc.index.find_symbol_at_position(offset)?;

        // Find all use/defs from the index
        let usedefs = doc
            .index
            .get_by_symbol_kind(
                symbol.symbol_kind,
                symbol.func_body_id.map(|id| &doc.index.function_bodies[id]),
            )
            .get(&symbol.name)?;
        let mut ranges = usedefs
            .get_use_def_kind(ud)
            .iter()
            .filter_map(|span| {
                Some(Location::new(
                    pos.text_document.uri.clone(),
                    range_to_lsp(&doc.rope, span)?,
                ))
            })
            .collect::<Vec<_>>();

        // In addition, consider the external_defs
        if ud == UseDefKind::Def {
            ranges.extend(usedefs.external_defs.iter().filter_map(|d| {
                let root_paths = self.root_paths.lock().unwrap();
                let uri = resolve_relative_path(&root_paths, &d.filepath)?;

                Some(Location {
                    uri,
                    range: Range {
                        start: Position::new(d.line - 1, 0),
                        end: Position::new(d.line - 1, 0),
                    },
                })
            }));
        }

        let origin_selection_range = range_to_lsp(&doc.rope, &symbol.span)?;
        Some((origin_selection_range, ranges))
    }

    fn do_code_action_resolve(&self, action: CodeAction) -> Result<CodeAction> {
        let raw_data = action
            .data
            .as_ref()
            .ok_or_else(|| Error::invalid_params("Missing `data` in code action"))?;
        let data = serde_json::from_value::<CodeActionData>(raw_data.clone()).map_err(|err| {
            Error::invalid_params(format!("Invalid `data` for code action: {}", err))
        })?;
        let mut updated_action = action.clone();
        match data {
            CodeActionData::ShiftNumbering {
                uri,
                symbol_kind,
                func_body_id,
                renamed_nr,
            } => {
                let doc = self
                    .document_map
                    .get(&uri.to_string())
                    .ok_or_else(|| Error::invalid_params("Document not found"))?;
                let edits =
                    get_shift_edits(&doc.rope, &doc.index, symbol_kind, func_body_id, renamed_nr);
                updated_action.edit = Some(WorkspaceEdit {
                    changes: Some(HashMap::from([(uri, edits)])),
                    ..Default::default()
                });
            }
        };
        Ok(updated_action)
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        root_paths: Default::default(),
        code_actions_lazy_resolve: Default::default(),
        document_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
