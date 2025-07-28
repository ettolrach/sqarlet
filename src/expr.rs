use crate::{parse::Position, types::{Field, Id, Type}};

#[derive(Debug, Clone)]
pub struct Expr {
    pub position: Position,
    pub expr: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Fv(Id),
    /// Variable declaration, aka. let binding.
    /// 
    /// Note: the reference is contradictory and on the one hand specifies that a variable
    /// declaration must be initialised using a value, but then shows examples of it being
    /// initialised with an expression. I will assume that you can initialise a variable with an
    /// expression to match the examples.
    Declare(Id, Option<Type>, Box<Expr>),
    DeclareFromKeyboard(Id, Option<Type>),
    /// Varaible assigment.
    Set(Id, Box<Expr>),
    /// Record declaration.
    Record(Id, Vec<Field>),
    /// Conditional.
    IfThenElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    While(Box<Expr>, Vec<Expr>),
    Repeat(Vec<Expr>, Box<Expr>),
    For(Id, Box<Expr>, Box<Expr>, Option<Box<Expr>>, Vec<Expr>),
    ForEach(Id, Box<Expr>, Vec<Expr>),
    /// Receive input from the keyboard (i.e. STDIN). The only documented input device is the
    /// keyboard, so this is always STDIN.
    Receive(Id),
}
