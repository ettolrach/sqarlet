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
pub enum Operator {
    Ampersand,
    Plus,
}

impl Operator {
    pub fn check_types(
        &self,
        lhs: &TypecheckedExpr,
        rhs: &TypecheckedExpr,
    ) -> Result<(), TypeError> {
        match self {
            // Ampersand (concat) always works because it'll coerce anything to a string.
            Self::Ampersand => Ok(()),
            Self::Plus => {
                for expr in [lhs, rhs] {
                    if !matches!(expr.ty, Type::Integer | Type::Indeterminate) {
                        return Err(TypeError::IncorrectOperatorType(
                            *self,
                            Box::new(Type::Integer),
                            Box::new(expr.ty.clone()),
                            expr.position,
                        ));
                    }
                }
                Ok(())
            }
        }
    }

    /// Types resulting from an operator.
    pub fn result_type(&self) -> Type {
        match self {
            Self::Ampersand => Type::Array(Box::new(Type::Indeterminate)),
            Self::Plus => Type::Integer,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ampersand => write!(f, "&"),
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
    /// Array indexing. In the form `Id[u64][u64]...`.
    Index(Id, Vec<Expr>),
    /// Record declaration.
    Record(Id, Vec<Field>),
    /// Conditional.
    IfThenElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    /// A while loop.
    While(Box<Expr>, Vec<Expr>),
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
    Repeat(Vec<Expr>, Box<Expr>),
    /// For loop. ID of counter, low expr, high expr, maybe step expr, and then body.
    For(Id, Box<Expr>, Box<Expr>, Option<Box<Expr>>, Vec<Expr>),
    ForEach(Id, Box<Expr>, Vec<Expr>),
    /// Receive input from the keyboard (i.e. STDIN). The only documented input device is the
    /// keyboard, so this is always STDIN.
    ///
    /// RECEIVE is also used to get a file/URL contents, but that will be implemented later.
    Receive(Id),
    /// Binary operation.
    BinOp(Box<Expr>, Operator, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub position: Position,
    pub expr: ExprKind,
}
