/// LSP protocol is using a differential position encoding to report the tokens.
/// This encoder output this encoding.
#[derive(Default)]
pub struct TokenEncoder {
    last_line: u32,
    last_col: u32,
    line: u32,
    col: u32,
}
impl TokenEncoder {
    pub fn skip(&mut self, width: u32) {
        self.col += width;
    }
    pub fn newline(&mut self) {
        self.line += 1;
        self.col = 0;
    }
    pub fn encode(&mut self, width: u32) -> (u32, u32) {
        let delta_line = self.line - self.last_line;
        let prev_col = if delta_line > 0 { 0 } else { self.last_col };
        let delta_start = self.col - prev_col;
        self.last_line = self.line;
        self.last_col = self.col;
        self.skip(width);
        (delta_line, delta_start)
    }
}
