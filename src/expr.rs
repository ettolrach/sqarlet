use std::fmt::Display;

use crate::{
    parse::Position,
    types::{Field, Id, Type},
};

#[derive(Debug, Clone, Copy)]
pub enum Literal {
    /// Integer, [`i64`] in this implementation.
    Integer(i64),
    /// Real, [`f64`] in this implementation.
    Real(f64),
    /// Boolean
    Boolean(bool),
    /// Character, here [`char`].
    Character(char),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Integer(x) => write!(f, "{x}"),
            Literal::Real(x) => write!(f, "{x}"),
            Literal::Boolean(b) => write!(f, "{b}"),
            Literal::Character(c) => write!(f, "{c}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Plus,
}

impl Operator {
    pub fn arg_types(&self) -> (Type, Type) {
        match self {
            Self::Plus => (Type::Integer, Type::Integer),
        }
    }

    pub fn result_type(&self) -> &Type {
        match self {
            Self::Plus => &Type::Integer,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Free variables.
    Fv(Id),
    /// Literal.
    Lit(Literal),
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
    /// Binary operation.
    BinOp(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub position: Position,
    pub expr: ExprKind,
}
