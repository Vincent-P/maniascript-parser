use crate::app_state::AppCtx;
use lib_maniascript::{lexer::Lexer, parser::{ParseError, Parser}};
use log::info;
use lsp_types::{notification::{PublishDiagnostics, ShowMessage}, Diagnostic, DidChangeTextDocumentParams, DidSaveTextDocumentParams, MessageType, Position, PublishDiagnosticsParams, Range, ShowMessageParams};
use std::convert::TryInto;
use tokio::prelude::Future;

pub fn did_save_handler(_param: DidSaveTextDocumentParams, app: AppCtx) {
    // send a test message to the client
    let msg = ShowMessageParams {
        typ: MessageType::Info,
        message: "Did save!".to_string(),
    };

    app.send_notification::<ShowMessage>(msg);
}

pub fn did_change_handler(_param: DidChangeTextDocumentParams, app: AppCtx) {
    let uri = _param.text_document.uri;
    let text = &_param.content_changes[0].text;

    let lexer = Lexer::new(text);

    let mut diagnostics = vec![];

    match Parser::new(lexer).parse() {
        Ok(_) => {}
        Err(e) => {
            info!("Parse error: {:?}", e);

            let ((l, c), msg) = match e {
                ParseError::ExpectedToken(e, Some((l, c, g))) => {
                    ((l, c), format!("Expected {} but got {}", e, g))
                }
                ParseError::ExpectedExpr(l, c, g) => {
                    ((l, c), format!("Expected an expression but got {}", g))
                }
                ParseError::ExpectedOperator(l, c, g) => {
                    ((l, c), format!("Expected an operator but got {}", g))
                }
                ParseError::UnexpectedToken(l, c, g) => {
                    ((l, c), format!("Unexpected token {}", g))
                }
                ParseError::GlobalsNotFirst => (
                    (0, 0),
                    "Globals need to be declared before functions and labels".to_string(),
                ),
                _ => return,
            };

            let pos = Position::new((l-1).try_into().unwrap(), c.try_into().unwrap());
            let range = Range::new(pos, pos);
            let diag = Diagnostic::new_simple(range, msg);
            diagnostics.push(diag);
        }
    }

    let params = PublishDiagnosticsParams::new(uri, diagnostics);
    app.send_notification::<PublishDiagnostics>(params);
}
