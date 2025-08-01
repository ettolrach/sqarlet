use std::fmt::Display;

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: i64,
    pub column: i64,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} column {}", self.line, self.column)
    }
}
