use bytes::{BufMut, BytesMut};
use std::{io, str};
use tokio_codec::{Decoder, Encoder};

pub struct LspRequestCodec {
    start_index: usize,
    len: Option<usize>,
}

impl LspRequestCodec {
    pub fn new() -> LspRequestCodec {
        LspRequestCodec {
            start_index: 0,
            len: None,
        }
    }
}

fn decode_request(buf: &mut BytesMut, content_length: usize, start: usize) -> Option<String> {
    let request_end = start + content_length;

    // The buffer contains the entire request
    if buf.len() > request_end {
        let request = buf.split_to(request_end);
        let request = &request[start..];
        let request = str::from_utf8(&request).expect("invalid utf8 data");

        Some(request.to_string())
    } else if buf.len() == request_end {
        let request = &buf[start..];
        let request = str::from_utf8(&request).expect("invalid utf8 data");
        let request = request.to_string();
        buf.clear();

        Some(request)
    } else {
        None
    }
}

impl Decoder for LspRequestCodec {
    type Error = io::Error;
    type Item = String;

    fn decode(&mut self, buf: &mut BytesMut) -> Result<Option<String>, io::Error> {
        let bufstr = str::from_utf8(&buf).expect("invalid utf8 data");

        if let Some(content_length) = self.len {
            let res = decode_request(buf, content_length, self.start_index);
            if res.is_some() {
                self.start_index = 0;
                self.len = None;
            }
            Ok(res)
        } else {
            if let Some(content_length) = bufstr.find("Content-Length: ") {
                if let Some(last_newline) = bufstr[content_length..].find("\r\n\r\n") {
                    let length_start = content_length + "Content-Length: ".len();
                    let length_end = content_length + last_newline;
                    let header_end = length_end + "\r\n\r\n".len();

                    let length = &buf[length_start..length_end];
                    let length = str::from_utf8(&length).expect("invalid utf8 data");

                    self.start_index = header_end;

                    match length.parse::<usize>() {
                        Ok(l) => self.len = Some(l),
                        Err(e) => return Err(io::Error::new(io::ErrorKind::InvalidInput, e)),
                    }

                    let res = decode_request(buf, self.len.unwrap(), self.start_index);
                    if res.is_some() {
                        self.start_index = 0;
                        self.len = None;
                    }

                    return Ok(res);
                }
            }

            Ok(None)
        }
    }
}

impl Encoder for LspRequestCodec {
    type Error = io::Error;
    type Item = String;

    fn encode(&mut self, response: String, buf: &mut BytesMut) -> Result<(), io::Error> {
        let content_length = format!("Content-Length: {}\r\n\r\n", response.len());
        buf.reserve(content_length.len() + response.len());
        buf.put(content_length);
        buf.put(response);
        buf.put("\r\n");
        Ok(())
    }
}
