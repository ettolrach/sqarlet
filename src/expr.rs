// sqarlet - a compiler for the SQA Reference Language. Copyright 2026 ettolrach et al.
// Licensed under the EUPL-1.2 or later.

use std::fmt::Display;

use crate::{
    parse::Position,
    typechecking::{TypeError, TypecheckedExpr},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOperator {
    Ampersand,
    Plus,
}

impl BinOperator {
    /// Returns the expected types of the lhs and rhs arguments.
    ///
    /// # Return value
    ///
    /// A tuple where the `.0` is the expected lhs type and `.1` the rhs type. An operator may
    /// accept several types. If the vec is empty, then any type is accepted.
    pub fn types_to_check(&self) -> (Vec<Type>, Vec<Type>) {
        match self {
            // Ampersand (concat) always works because it'll coerce anything to a string.
            Self::Ampersand => (Vec::new(), Vec::new()),
            Self::Plus => (
                vec![Type::Integer, Type::Real],
                vec![Type::Integer, Type::Real],
            ),
        }
    }

    /// TODO: explain
    ///
    /// # Errors
    ///
    /// The only error this function produces is [`TypeError::IncorrectOperatorType`].
    pub fn check_and_join(&self, lhs: &Type, rhs: &Type, pos: Position) -> Result<Type, TypeError> {
        match (self) {
            Self::Ampersand => match (lhs, rhs) {
                (Type::Array(b), _) | (_, Type::Array(b)) if matches!(**b, Type::Character) => {
                    Ok(Type::Array(Box::new(Type::Character)))
                }
                _ => todo!(),
            },
            Self::Plus => {
                let Some(joined) = lhs.join(rhs) else {
                    return Err(TypeError::IncorrectBinOperatorType {
                        op: self.clone(),
                        expected: vec![Type::Integer, Type::Real],
                        lhs: Box::new(lhs.clone()),
                        rhs: Box::new(rhs.clone()),
                        pos,
                    });
                };
                if matches!(joined, Type::Integer | Type::Real) {
                    Ok(joined)
                } else {
                    Err(TypeError::IncorrectBinOperatorType {
                        op: self.clone(),
                        expected: vec![Type::Integer, Type::Real],
                        lhs: Box::new(lhs.clone()),
                        rhs: Box::new(rhs.clone()),
                        pos,
                    })
                }
            }
        }
    }
}

impl Display for BinOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ampersand => write!(f, "&"),
            Self::Plus => write!(f, "+"),
        }
    }
}

#[derive(Debug, Clone)]
/// A source for a value. Either an expression or a source, such as a `KEYBOARD`.
pub enum ValueSource {
    /// An expression.
    Expr(Expr),
    /// The only current other source.
    Keyboard,
    // In the future, we also accept a file or online source.
    // FileOrUrl(String),
}

/// An output device, such as a `DISPLAY`.
#[derive(Debug, Clone)]
pub enum Output {
    Display,
    // In the future, we also accept a file or online source.
    // FileOrUrl(String),
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// Free variables.
    Fv(Id),
    /// Literal.
    Lit(Literal),
    /// Array
    Array(Vec<Expr>),
    /// Array indexing. In the form `Id[u64][u64]...`.
    Index(Id, Vec<Expr>),
    /// Record declaration.
    Record(Id, Vec<Field>),
    /// Binary operation.
    BinOp(Box<Expr>, BinOperator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub position: Position,
    pub expr: ExprKind,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Variable declaration, a.k.a. let binding.
    ///
    /// Note: the reference is contradictory and on the one hand specifies that a variable
    /// declaration must be initialised using a value, but then shows examples of it being
    /// initialised with an expression. I will assume that you can initialise a variable with an
    /// expression to match the examples.
    Declare(Id, Option<Type>, ValueSource),
    /// Varaible assigment.
    Set(Id, Expr),
    /// Conditional.
    IfThenElse(Expr, Vec<Stmt>, Vec<Stmt>),
    /// A while loop.
    While(Expr, Vec<Stmt>),
    /// A repeat-until loop, also known as a do-while loop.
    ///
    /// # Example
    ///
    /// ```sqarl
    /// REPEAT
    ///     RECEIVE score FROM (INTEGER) KEYBOARD
    ///     IF score ˂1 OR score˃ 99 THEN
    ///         SEND "Error, please enter a score between 1 and 99 inclusive" TO DISPLAY
    ///     END IF
    /// UNTIL score ˃=1 AND score ˂=99
    /// ```
    ///
    /// In this example, the initial `Vec<Expr>` will be `[Receive, IfThenElse]` and the
    /// `Box<Expr>` will be `BinOp`.
    ///
    /// From BBC Bitesize (no date). 'Implementation: Algorithm specification, Input validation
    /// algorithm'. Available at: <https://www.bbc.co.uk/bitesize/guides/z3gnqhv/revision/2>
    /// (Accessed on: 2026-10-08)
    Repeat(Vec<Stmt>, Expr),
    /// Receive input from the keyboard (i.e. STDIN). The only documented input device is the
    /// keyboard, so this is always STDIN.
    ///
    /// RECEIVE is also used to get a file/URL contents, but that will be implemented later.
    Receive(Id),
    /// For loop. ID of counter, low expr, high expr, maybe step expr, and then body.
    For(Id, Expr, Expr, Option<Expr>, Vec<Stmt>),
    ForEach(Id, Expr, Vec<Stmt>),
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub position: Position,
    pub stmt: StmtKind,
}
