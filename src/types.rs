use std::fmt::Display;

use crate::expr::Literal;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id(String);

impl Id {
    pub fn new(s: String) -> Option<Self> {
        if s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
            && s.chars().any(|c| c.is_ascii_lowercase())
        {
            Some(Id(s))
        } else {
            None
        }
    }
}

impl Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub type Field = (Id, Type);

fn print_types(types: &[Type]) -> String {
    assert!(!types.is_empty());
    let mut to_return = types[0].to_string();
    for ty in &types[1..] {
        to_return.push_str(", ");
        to_return.push_str(ty.to_string().as_str())
    }
    to_return
}

fn print_fields(fields: &[Field]) -> String {
    fn print_field(field: &Field) -> String {
        format!("{} {}", field.1, field.0)
    }
    assert!(!fields.is_empty());
    let mut to_return = print_field(&fields[0]);
    for field in &fields[1..] {
        to_return.push_str(", ");
        to_return.push_str(print_field(field).to_string().as_str())
    }
    to_return
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// A free type variable. Since there are no generics, this will always be the ID of a record
    /// or class.
    Ftv(Id),
    /// The unit type. While not explicitly defined in the spec, it's the only possible type for
    /// expressions like a variable declaration.
    Unit,
    Integer,
    Real,
    Boolean,
    Character,
    Array(Box<Type>),
    Record(Id, Vec<Field>),
    // Class(Id),
    /// A function with the types of the parameters and the output type.
    Function(Vec<Type>, Box<Type>),
    NumOrString,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Ftv(id) => write!(f, "{id}"),
            Type::Unit => write!(f, "UNIT"),
            Type::Integer => write!(f, "INTEGER"),
            Type::Real => write!(f, "REAL"),
            Type::Boolean => write!(f, "BOOLEAN"),
            Type::Character => write!(f, "CHARACTER"),
            Type::Array(ty) => write!(f, "ARRAY OF {ty}"),
            Type::Record(id, fields) => write!(f, "{id} {{ {} }}", print_fields(fields)),
            // Type::Class(id) => write!(f, "{id} (CLASS)"),
            Type::Function(from, to) => write!(f, "FUNCTION ({}) RETURNS {to}", print_types(from)),
            Type::NumOrString => write!(f, "INTEGER, REAL, OR STRING"),
        }
    }
}

impl From<Literal> for Type {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Integer(_) => Self::Integer,
            Literal::Real(_) => Self::Real,
            Literal::Boolean(_) => Self::Boolean,
            Literal::Character(_) => Self::Character,
        }
    }
}

impl From<&Literal> for Type {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Integer(_) => Self::Integer,
            Literal::Real(_) => Self::Real,
            Literal::Boolean(_) => Self::Boolean,
            Literal::Character(_) => Self::Character,
        }
    }
}

impl From<&Literal> for &Type {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::Integer(_) => &Type::Integer,
            Literal::Real(_) => &Type::Real,
            Literal::Boolean(_) => &Type::Boolean,
            Literal::Character(_) => &Type::Character,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    name: Id,
    fields: Vec<Field>,
}

pub struct Class {
    name: Id,
    parent: Option<Id>,
    /// Instance variables (called fields internally).
    fields: Vec<Field>,
}
