use std::collections::HashMap;

use dashmap::DashMap;
use hir_language_server::hir_index::{create_index, HIRIndex, UseDef};
use hir_language_server::hir_parser::{parse_from_str, ParserResult};
use hir_language_server::hir_tokenizer::Span;
use hir_language_server::semantic_token::{
    convert_to_lsp_tokens, semantic_tokens_from_tokens, HIRSemanticToken, LEGEND_TYPE,
};
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<HIRSemanticToken>>,
    index_map: DashMap<String, HIRIndex>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
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

                // definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
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
    /*
       async fn goto_definition(
           &self,
           params: GotoDefinitionParams,
       ) -> Result<Option<GotoDefinitionResponse>> {
           let definition = async {
               let uri = params.text_document_position_params.text_document.uri;
               let ast = self.ast_map.get(uri.as_str())?;
               let rope = self.document_map.get(uri.as_str())?;

               let position = params.text_document_position_params.position;
               let char = rope.try_line_to_char(position.line as usize).ok()?;
               let offset = char + position.character as usize;
               // self.client.log_message(MessageType::INFO, &format!("{:#?}, {}", ast.value(), offset)).await;
               let span = get_definition(&ast, offset);
               self.client
                   .log_message(MessageType::INFO, &format!("{:?}, ", span))
                   .await;
               span.and_then(|(_, range)| {
                   let start_position = offset_to_position(range.start, &rope)?;
                   let end_position = offset_to_position(range.end, &rope)?;

                   let range = Range::new(start_position, end_position);

                   Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
               })
           }
           .await;
           Ok(definition)
       }

       async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
           let reference_list = || -> Option<Vec<Location>> {
               let uri = params.text_document_position.text_document.uri;
               let ast = self.ast_map.get(&uri.to_string())?;
               let rope = self.document_map.get(&uri.to_string())?;

               let position = params.text_document_position.position;
               let char = rope.try_line_to_char(position.line as usize).ok()?;
               let offset = char + position.character as usize;
               let reference_list = get_reference(&ast, offset, false);
               let ret = reference_list
                   .into_iter()
                   .filter_map(|(_, range)| {
                       let start_position = offset_to_position(range.start, &rope)?;
                       let end_position = offset_to_position(range.end, &rope)?;

                       let range = Range::new(start_position, end_position);

                       Some(Location::new(uri.clone(), range))
                   })
                   .collect::<Vec<_>>();
               Some(ret)
           }();
           Ok(reference_list)
       }
    */

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let lsp_tokens = || -> Option<Vec<SemanticToken>> {
            let sem_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let lsp_tokens = convert_to_lsp_tokens(&rope, &sem_tokens);
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
        let uri = params.text_document.uri.clone();
        let uri_str = uri.to_string();
        self.client
            .log_message(MessageType::LOG, "document_symbol")
            .await;

        let i = self.index_map.get(&uri_str).unwrap();
        let ii = i.value().clone();
        self.client
            .log_message(MessageType::LOG, &format!("idx {:?}", ii))
            .await;

        let symbols = || -> Option<DocumentSymbolResponse> {
            let index = self.index_map.get(&uri_str)?;
            let rope = self.document_map.get(&uri_str)?;
            let mut symbols = Vec::<SymbolInformation>::new();

            let mut add_symbols = |ud: &HashMap<String, UseDef>, kind: SymbolKind| {
                symbols.extend(ud.iter().flat_map(|f| {
                    f.1.defs.iter().filter_map(|def| {
                        Some(SymbolInformation {
                            name: f.0.to_string(),
                            kind: kind,
                            tags: None,
                            location: Location {
                                uri: uri.clone(),
                                range: range_to_lsp(&rope, &def)?,
                            },
                            deprecated: None,
                            container_name: None,
                        })
                    })
                }));
            };

            add_symbols(&index.global_vars, SymbolKind::CONSTANT);
            add_symbols(&index.functions, SymbolKind::FUNCTION);

            Some(DocumentSymbolResponse::Flat(symbols))
        }();

        self.client
            .log_message(MessageType::LOG, &format!("symbols {:?}", symbols))
            .await;

        Ok(symbols)
    }
}

struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        self.document_map
            .insert(params.uri.to_string(), rope.clone());

        let ParserResult {
            tokens,
            stmts,
            errors,
        } = parse_from_str(&rope.to_string());

        self.semantic_token_map
            .insert(params.uri.to_string(), semantic_tokens_from_tokens(&tokens));

        self.index_map
            .insert(params.uri.to_string(), create_index(&tokens, &stmts));

        let diagnostics = errors
            .into_iter()
            .filter_map(|item| {
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
                    // let start_line = rope.try_char_to_line(span.start)?;
                    // let first_char = rope.try_line_to_char(start_line)?;
                    // let start_column = span.start - first_char;
                    let start_position = offset_to_position(span.start, &rope)?;
                    let end_position = offset_to_position(span.end, &rope)?;
                    // let end_line = rope.try_char_to_line(span.end)?;
                    // let first_char = rope.try_line_to_char(end_line)?;
                    // let end_column = span.end - first_char;
                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        message,
                    ))
                }()
            })
            .collect::<Vec<_>>();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;
    }
}

fn offset_to_lsp_pos(rope: &Rope, pos: usize) -> Option<Position> {
    let line = rope.try_byte_to_line(pos).ok()?;
    let first = rope.try_line_to_char(line).ok()?;
    let character = rope.try_byte_to_char(pos).ok()? - first;
    Some(Position {
        line: line.try_into().ok()?,
        character: character.try_into().ok()?,
    })
}

fn range_to_lsp(rope: &Rope, span: &Span) -> Option<Range> {
    Some(Range {
        start: offset_to_lsp_pos(rope, span.start).unwrap(),
        end: offset_to_lsp_pos(rope, span.end).unwrap(),
    })
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
        index_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}
