use std::sync::Arc;
use dashmap::DashMap;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use lib_maniascript::parser;

mod utils {
    use lib_maniascript::parser::{TextRange, TextUnit};
    use tower_lsp::lsp_types::{Position, Range};

    pub fn offset_to_pos(code: &str, offset: TextUnit) -> Position {
        let offset = offset.to_usize();
        let start_of_line = code[..offset].rfind('\n').map_or(0, |n| n + 1);
        Position {
            line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
            character: code[start_of_line..offset]
                .chars()
                .map(|c| c.len_utf16() as u64)
                .sum(),
        }
    }

    pub fn range(code: &str, range: TextRange) -> Range {
        Range {
            start: offset_to_pos(code, range.start()),
            end: offset_to_pos(code, range.end()),
        }
    }
}

#[derive(Default)]
struct Backend {
    files: Arc<DashMap<Url, (parser::AST, String)>>,
}

impl Backend {
    fn send_diagnostics(&self, client: &Client, uri: Url, code: &str, ast: &parser::AST) {
        let errors = ast.errors();
        let mut diagnostics = Vec::with_capacity(errors.len());
        for err in errors {
            let message = err.to_string();
            match err {
                parser::NewParseError::Missing(node_range, _) => {
                    diagnostics.push(Diagnostic {
                        range: utils::range(code, node_range),
                        severity: Some(DiagnosticSeverity::Error),
                        message,
                        ..Diagnostic::default()
                    });
                }

                parser::NewParseError::UnknownToken(text_range, _) => {
                    diagnostics.push(Diagnostic {
                        range: utils::range(code, text_range),
                        severity: Some(DiagnosticSeverity::Error),
                        message,
                        ..Diagnostic::default()
                    });
                }

                _ => (),
            }
        }

        client.send_custom_notification::<notification::PublishDiagnostics>(
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            }
        );
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: &Client, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::Full,
                )),
                document_symbol_provider: Some(true),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, client: &Client, _: InitializedParams) {
        client.log_message(MessageType::Info, "server initialized!");
    }

    async fn did_open(&self, client: &Client, params: DidOpenTextDocumentParams) {
        client.log_message(MessageType::Info, format!("file opened: {}!", params.text_document.uri));

        let text = params.text_document.text;
        let parsed = parser::parse(&text);
        self.send_diagnostics(client, params.text_document.uri.clone(), &text, &parsed);
        self.files.insert(params.text_document.uri, (parsed, text));
    }

    async fn did_change(&self, client: &Client, params: DidChangeTextDocumentParams) {
        client.log_message(MessageType::Info, format!("file changed: {}!", params.text_document.uri));

        if let Some(change) = params.content_changes.into_iter().last() {
            let parsed = parser::parse(&change.text);
            self.send_diagnostics(client, params.text_document.uri.clone(), &change.text, &parsed);
            self.files.insert(params.text_document.uri, (parsed, change.text));
        }
    }

    async fn document_symbol(&self, params: DocumentSymbolParams) -> Result<Option<DocumentSymbolResponse>> {
        use lib_maniascript::parser::typed_node::*;

        fn add_named_symbol<N>(
            symbols: &mut Vec<SymbolInformation>,
            node: N,
            kind: SymbolKind,
            text: &str,
            uri: Url,
        ) where
            N: NamedNode,
        {
            let name = match node.name() {
                Some(n) => n,
                None => return,
            };

            let range = utils::range(text, name.node().text_range());
            let location = Location::new(uri, range);

            symbols.push(SymbolInformation {
                name: String::from(name.as_str()),
                kind,
                deprecated: None,
                location,
                container_name: None,
            });
        }

        let mut symbols: Vec<SymbolInformation> = vec![];

        if let Some(element) = self.files.get(&params.text_document.uri) {
            let (uri, (ast, text)) = element.pair();
            let root = ast.node();
            let root = Root::cast(root).unwrap();

            for n in root.functions() {
                add_named_symbol(&mut symbols, n, SymbolKind::Function, &text, uri.clone());
            }
            for n in root.labels() {
                add_named_symbol(&mut symbols, n, SymbolKind::Function, &text, uri.clone());
            }
            for n in root.consts() {
                add_named_symbol(&mut symbols, n, SymbolKind::Constant, &text, uri.clone());
            }
            for n in root.commands() {
                add_named_symbol(&mut symbols, n, SymbolKind::Constant, &text, uri.clone());
            }
            for n in root.globals() {
                add_named_symbol(&mut symbols, n, SymbolKind::Variable, &text, uri.clone());
            }
            for n in root.structs() {
                add_named_symbol(&mut symbols, n, SymbolKind::Struct, &text, uri.clone());
            }
            for n in root.includes() {
                add_named_symbol(&mut symbols, n, SymbolKind::Namespace, &text, uri.clone());
            }
            for n in root.settings() {
                add_named_symbol(&mut symbols, n, SymbolKind::Namespace, &text, uri.clone());
            }
        }

        Ok(Some(DocumentSymbolResponse::Flat(symbols)))
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(Backend::default());
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}
