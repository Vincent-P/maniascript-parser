use log::info;
use lsp_types::{request::*, *};
use lsp_server::{RequestId, Response, Notification};

use lib_maniascript::parser;

pub use crate::app::App;

mod utils {
    use lsp_types::{Position, Range};
    use lib_maniascript::parser::TextRange;

    pub fn offset_to_pos(code: &str, offset: usize) -> Position {
        let start_of_line = code[..offset].rfind('\n').map_or(0, |n| n+1);
        Position {
            line: code[..start_of_line].chars().filter(|&c| c == '\n').count() as u64,
            character: code[start_of_line..offset].chars().map(|c| c.len_utf16() as u64).sum()
        }
    }

    pub fn range(code: &str, range: TextRange) -> Range {
        Range {
            start: offset_to_pos(code, range.start().to_usize()),
            end: offset_to_pos(code, range.end().to_usize()),
        }
    }
}

impl App {
    pub fn go_to_definition(&mut self, req_id: RequestId, params: TextDocumentPositionParams) {

        info!("Got gotoDefinition request #{}: {:?}", req_id, params);

        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
        let result = serde_json::to_value(&result).unwrap();

        self.reply(Response {
            id: req_id,
            result: Some(result),
            error: None,
        });
    }

    pub fn did_open_text_document(&mut self, params: DidOpenTextDocumentParams) {
        let text = params.text_document.text;
        let parsed = parser::parse(&text);
        self.send_diagnostics(params.text_document.uri.clone(), &text, &parsed);
        self.files.insert(params.text_document.uri, (parsed, text));
    }

    pub fn did_change_text_document(&mut self, params: DidChangeTextDocumentParams) {
        if let Some(change) = params.content_changes.into_iter().last() {
            let parsed = parser::parse(&change.text);
            self.send_diagnostics(params.text_document.uri.clone(), &change.text, &parsed);
            self.files.insert(params.text_document.uri, (parsed, change.text));
        }
    }

    fn send_diagnostics(&mut self, uri: Url, code: &str, ast: &parser::AST) {
        let errors = ast.errors();
        let mut diagnostics = Vec::with_capacity(errors.len());
        for err in errors {
            if let parser::NewParseError::Unexpected(node) = err {
                diagnostics.push(Diagnostic {
                    range: utils::range(code, node),
                    severity: Some(DiagnosticSeverity::Error),
                    message: err.to_string(),
                    ..Diagnostic::default()
                });
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
