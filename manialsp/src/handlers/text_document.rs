use crate::app_state::AppCtx;
use lib_maniascript::{lexer::Lexer, parser::{ParseError, Parser}, document::symbol::*};
use log::info;
use lsp_types::{notification::{PublishDiagnostics}, Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, MessageType, Position, PublishDiagnosticsParams, Range, Url};
use std::convert::TryInto;

pub fn did_save_handler(_param: DidSaveTextDocumentParams, app: AppCtx) {
}

fn validate_document(app: AppCtx, uri: Url, text: &str) {
    let lexer = Lexer::new(text);

    let diagnostics = match Parser::new(lexer).parse() {
        Ok(tree) => {
            let errors: Vec<Diagnostic> = tree.validate()
                .iter()
                .map(|error| {

                    let mut line: usize = 0;
                    let mut col: usize = 0;
                    let mut cur_char: usize = 0;
                    let mut start_col: usize = 0;
                    let mut start_line: usize = 0;
                    let mut end_col: usize = 0;
                    let mut end_line: usize = 0;

                    for c in text.chars() {
                        if cur_char >= error.span.0 && start_col == 0 && start_line == 0 {
                            start_col = col;
                            start_line = line;
                        }

                        if cur_char >= error.span.1 {
                            end_col = col;
                            end_line = line;
                            break;
                        }

                        cur_char += c.len_utf8();
                        col += c.len_utf8();
                        if c == '\n' {
                            line += 1;
                            col = 0;
                        }
                    }

                    info!("Node span: {:?} | start {}:{} | end {}:{}", error.span, start_line, start_col, end_line, end_col);

                    let s = Position::new(start_line.try_into().unwrap(), start_col.try_into().unwrap());
                    let e = Position::new(end_line.try_into().unwrap(), end_col.try_into().unwrap());
                    let range = Range::new(s, e);

                    Diagnostic::new_simple(range, error.msg.to_string())
                })
                .collect();

            if errors.is_empty() {
                let symbols = unchecked_find_symbols(&tree, text);
                info!("Symbols: {:?}", symbols);
            } else {
                info!("No symbols found!");
            }

            errors
        }

        Err(e) => {
            info!("Parse error: {:?}", e);

            let (span, msg) = match e {
                ParseError::Token(ref t, Some(e), span) => {
                    (span, format!("Expecting a {:?} but got a {:?}", e, t.kind))
                }
                ParseError::String(ref t, Some(ref e), span) => {
                    (span, format!("Expecting a {:?} but got a {:?}", e, t.kind))
                }
                ParseError::Token(ref t, None, span) => (span, format!("Got a {:?}", t.kind)),
                ParseError::String(ref t, None, span) => (span, format!("Got a {:?}", t.kind)),
                ParseError::EOF(span) => (span, "Expecting got EOF".to_string()),
            };

            let pos = Position::new(
                (span.line() - 1).try_into().unwrap(),
                span.col().try_into().unwrap(),
            );
            let range = Range::new(pos, pos);
            let diag = Diagnostic::new_simple(range, msg);
            vec![diag]
        }
    };

    let params = PublishDiagnosticsParams::new(uri, diagnostics);
    app.send_notification::<PublishDiagnostics>(params);
}

pub fn did_open_handler(_param: DidOpenTextDocumentParams, app: AppCtx) {
    let uri = _param.text_document.uri;
    let text = &_param.text_document.text;
    validate_document(app, uri, text);
}

pub fn did_change_handler(_param: DidChangeTextDocumentParams, app: AppCtx) {
    let uri = _param.text_document.uri;
    let text = &_param.content_changes[0].text;
    validate_document(app, uri, text);
}
