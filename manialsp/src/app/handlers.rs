use log::info;

use lsp_types::{request::*, *};
use lsp_server::{RequestId, Response, Notification};

use lib_maniascript::parser::{self, typed_node::*, SyntaxKind, TextRange};

pub use crate::app::App;

mod utils {
    use lsp_types::{Position, Range};
    use lib_maniascript::parser::{TextRange, TextUnit};

    pub fn offset_to_pos(code: &str, offset: TextUnit) -> Position {
        let offset = offset.to_usize();
        let start_of_line = code[..offset].rfind('\n').map_or(0, |n| n+1);
        Position {
            line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
            character: code[start_of_line..offset].chars().map(|c| c.len_utf16() as u64).sum()
        }
    }

    pub fn range(code: &str, range: TextRange) -> Range {
        Range {
            start: offset_to_pos(code, range.start()),
            end: offset_to_pos(code, range.end()),
        }
    }
}

impl App {
    pub fn go_to_definition(&mut self, req_id: RequestId, _params: TextDocumentPositionParams) {
        info!("GoToDefinition");

        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
        let result = serde_json::to_value(&result).unwrap();

        self.reply(Response {
            id: req_id,
            result: Some(result),
            error: None,
        });
    }

    pub fn did_open_text_document(&mut self, params: DidOpenTextDocumentParams) {
        info!("DidOpenTextDocument");

        let text = params.text_document.text;
        let parsed = parser::parse(&text);
        self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed);
        self.files.insert(params.text_document.uri, (parsed, text));
    }

    pub fn did_change_text_document(&mut self, params: DidChangeTextDocumentParams) {
        info!("DidChangeTextDocument");

        if let Some(change) = params.content_changes.into_iter().last() {
            let parsed = parser::parse(&change.text);
            self.send_diagnostics(params.text_document.uri.clone(), &change.text, &parsed);
            self.files.insert(params.text_document.uri, (parsed, change.text));
        }
    }

    pub fn document_symbol(&mut self, req_id: RequestId, params: DocumentSymbolParams) {
        info!("DocumentSymbol");

        fn add_named_symbol<N>(symbols: &mut Vec<SymbolInformation>, node: N, kind: SymbolKind, text: &str, uri: Url)
        where
            N: NamedNode
        {
            let name = match node.name() {
                Some(n) => n,
                None => return
            };

            let range = utils::range(text, name.node().text_range());
            let location = Location::new(uri, range);

            symbols.push(SymbolInformation {
                name: String::from(name.as_str()),
                kind,
                deprecated: None,
                location,
                container_name: None
            });
        }

        let mut symbols : Vec<SymbolInformation> = vec![];

        if let Some((uri, (ast, text))) = self.files.get_key_value(&params.text_document.uri) {

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
            for n in root.globals() {
                add_named_symbol(&mut symbols, n, SymbolKind::Variable, &text, uri.clone());
            }
            for n in root.structs() {
                add_named_symbol(&mut symbols, n, SymbolKind::Struct, &text, uri.clone());
            }
        }

        let result = serde_json::to_value(symbols).unwrap();
        self.reply(Response {
            id: req_id,
            result: Some(result),
            error: None,
        });
    }

    pub fn folding_ranges(&mut self, req_id: RequestId, params: FoldingRangeParams) {
        info!("FoldingRanges");

        let mut ranges: Option<Vec<FoldingRange>> = None;

        if let Some((_uri, (ast, text))) = self.files.get_key_value(&params.text_document.uri) {
            let root = ast.node();

            let mut ranges_kinds: Vec<(TextRange, FoldingRangeKind)> = vec![];

            // Find all blocks and comment with a preorder traversal
            for event in root.preorder_with_tokens() {
                match &event {
                    WalkEvent::Enter(NodeOrToken::Node(node)) => {
                        if let SyntaxKind::NODE_BLOCK = node.kind() {
                            ranges_kinds.push((node.text_range(), FoldingRangeKind::Region));
                        }
                    }
                    WalkEvent::Enter(NodeOrToken::Token(token)) => {
                        if let SyntaxKind::TOKEN_COMMENT = token.kind() {
                            ranges_kinds.push((token.text_range(), FoldingRangeKind::Comment));
                        }
                    }
                    _ => ()
                }
            }

            // includes and label decls are top level so available from the root
            let root = Root::cast(root).unwrap();

            let mut includes = root.includes();
            if let Some(first) = includes.next() {
                let range = if let Some(end) = includes.last() {
                    TextRange::from_to(first.node().text_range().start(), end.node().text_range().end())
                }
                else {
                    first.node().text_range()
                };

                ranges_kinds.push((range, FoldingRangeKind::Imports));
            }

            let labels = root.labels();
            for label in labels {
                ranges_kinds.push((label.node().text_range(), FoldingRangeKind::Region));
            }

            ranges = Some(ranges_kinds.iter().map(|(range, kind)| FoldingRange {
                start_line: utils::offset_to_pos(text, range.start()).line,
                end_line:   utils::offset_to_pos(text, range.end()).line,
                start_character: None, // default to length of start_line, so it folds anything after the first line
                end_character: None,
                kind: Some(kind.clone())
            }).collect());
        }

        let result = serde_json::to_value(ranges).unwrap();
        self.reply(Response {
            id: req_id,
            result: Some(result),
            error: None,
        });
    }

    fn send_diagnostics(&mut self, uri: Url, code: &str, ast: &parser::AST) {
        info!("SendDiagnostics");

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

                parser::NewParseError::UnknownToken(token) => {
                    diagnostics.push(Diagnostic {
                        range: utils::range(code, token.text_range()),
                        severity: Some(DiagnosticSeverity::Error),
                        message,
                        ..Diagnostic::default()
                    });
                }

                _ => ()
            }
        }
        self.notify(Notification::new(
            "textDocument/publishDiagnostics".into(),
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: None,
            }
        ));
    }
}
