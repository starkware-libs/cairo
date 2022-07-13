use std::fmt::Display;
use std::ops::Sub;

#[derive(Copy, Clone, Debug, Default)]
pub struct TextOffset(pub u32);
impl TextOffset {
    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
    pub fn inc(&mut self) {
        self.0 += 1;
    }
}
impl Sub for TextOffset {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.as_usize() - rhs.as_usize()
    }
}
impl Display for TextOffset {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, Clone, Debug)]
pub struct TextSpan {
    pub from: TextOffset,
    pub to: TextOffset,
}
impl TextSpan {
    pub fn width(&self) -> u32 {
        (self.to - self.from) as u32
    }
}
