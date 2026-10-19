// sqarlet - a compiler for the SQA Reference Language. Copyright 2026 ettolrach et al.
// Licensed under the EUPL-1.2 or later.

//! We typecheck using a bidirectional algorithm. [`Stmt`]s are all checked to be of type
//! [`Type::Unit`]. We then split the typechecking of [`Expr`]s are then split into two functions:
//!
//! * [`infer`]: some expressions, usually eliminators, can infer the type that they are.
//! * [`check`]: others require the type that they're supposed to be to be supplied.
//!
//! We use the Pfenning method (Dunfield and Krishnaswami, 2013) to decide which expressions are
//! checking and which are inferring. The short of it is: introduction rules (constructors) check,
//! elimination rules infer.
//!
//! # References
//!
//! * Dunfield, J., Krishnaswami, N., 2022. Bidirectional Typing. ACM Comput. Surv. 54, 1–38.
//!   <https://doi.org/10.1145/3450952>.

use std::{collections::HashMap, iter::zip};

use thiserror::Error;

use crate::{
    expr::{BinOperator, Expr, ExprKind, Literal, Stmt, StmtKind, ValueSource},
    parse::Position,
    typechecking::TypeError::DeclWithoutKnownType,
    types::{Coercion, Field, Id, Type, write_types},
};

/// An output device, such as a `DISPLAY`.
#[derive(Debug, Clone)]
pub enum Output {
    Display,
    // In the future, we also accept a file or online source.
    // FileOrUrl(String),
}

#[derive(Debug, Clone)]
/// A source for a value. Either an expression or a source, such as a `KEYBOARD`.
pub enum TypecheckedValueSource {
    /// An expression.
    Expr(TypecheckedExpr),
    /// The only current other source.
    Keyboard,
    // In the future, we also accept a file or online source.
    // FileOrUrl(String),
}

#[derive(Debug, Clone)]
pub enum TypedStmtKind {
    /// Variable declaration, a.k.a. let binding.
    ///
    /// Note: the reference is contradictory and on the one hand specifies that a variable
    /// declaration must be initialised using a value, but then shows examples of it being
    /// initialised with an expression. I will assume that you can initialise a variable with an
    /// expression to match the examples.
    Declare(Id, Type, TypecheckedValueSource),
    /// Varaible assigment.
    Set(Id, TypecheckedExpr),
    /// Conditional.
    IfThenElse(TypecheckedExpr, Vec<TypecheckedStmt>, Vec<TypecheckedStmt>),
    /// A while loop.
    While(TypecheckedExpr, Vec<TypecheckedStmt>),
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
    Repeat(Vec<TypecheckedStmt>, TypecheckedExpr),
    /// Receive input from the keyboard (i.e. STDIN). The only documented input device is the
    /// keyboard, so this is always STDIN.
    ///
    /// RECEIVE is also used to get a file/URL contents, but that will be implemented later.
    Receive(Id),
    /// For loop. ID of counter, low expr, high expr, maybe step expr, and then body.
    For(
        Id,
        TypecheckedExpr,
        TypecheckedExpr,
        Option<TypecheckedExpr>,
        Vec<TypecheckedStmt>,
    ),
    ForEach(Id, TypecheckedExpr, Vec<TypecheckedStmt>),
}

#[derive(Debug, Clone)]
pub struct TypecheckedStmt {
    pub stmt: TypedStmtKind,
    pub position: Position,
}

impl TypecheckedStmt {
    fn new(stmt: TypedStmtKind, position: Position) -> Self {
        Self { stmt, position }
    }
}

#[derive(Debug, Clone)]
pub enum TypedExprKind {
    Coerced(Coercion, Box<TypedExprKind>),
    /// Free variables.
    Fv(Id),
    /// Literal.
    Lit(Literal),
    /// Array
    Array(Vec<TypecheckedExpr>),
    /// Array indexing. In the form `Id[u64][u64]...`.
    Index(Id, Vec<TypecheckedExpr>),
    /// Record declaration.
    Record(Id, Vec<Field>),
    /// Binary operation.
    BinOp(Box<TypecheckedExpr>, BinOperator, Box<TypecheckedExpr>),
}

#[derive(Debug, Clone)]
pub struct TypecheckedExpr {
    pub expr: TypedExprKind,
    pub position: Position,
    pub ty: Type,
}

impl TypecheckedExpr {
    fn new(expr: TypedExprKind, position: Position, ty: Type) -> Self {
        Self { expr, position, ty }
    }
}

// TODO: add doc comments for each variant.
#[derive(Debug, Error)]
pub enum TypeError {
    /// Array elemnts have different types.
    #[error(
        "Array elements weren't all of the same type. Expected {ty} but the element at index {index} was {bad_ty} and didn't match, at {pos}"
    )]
    ArrayElemsDifferingTypes {
        ty: Box<Type>,
        index: usize,
        bad_ty: Box<Type>,
        bad_position: Position,
        pos: Position,
    },
    /// An IF, WHILE, or UNTIL expression was not a boolean.
    #[error("Condition was not a BOOLEAN at {0}.")]
    CondNotBool(Position),
    /// A declaration without `AS type` annotation where the type cannot be determined from the
    /// initialiser.
    #[error(
        "A variable was declared without a type specified and with an expression whose type cannot be determined (Spec 3.4). At {0}"
    )]
    DeclWithoutKnownType(Position),
    /// Expected a non-array type, but got the empty array.
    #[error("Expected {0} but got the empty array at {1}")]
    EmptyArrayAsNonArray(Box<Type>, Position),
    /// An empty array was found in an [`infer`] position. That's not permitted by the spec.
    #[error("Tried to infer the type of an empty array, but that's not permitted. At {0}")]
    EmptyArrayInInferPosition(Position),
    /// Only ARRAYs are allowed as iterators in FOR EACH loops, but another type was supplied.
    #[error("FOR EACH expected an array, but found {0} at {1}.")]
    ExpectedArray(Type, Position),
    /// Bounds of a FOR loop weren't of type INTEGER.
    #[error("FOR received {0} instead of INTEGER for range at {1}.")]
    ForNotInt(Type, Position),
    /// Iterator of FOR EACH loop wasn't an array.
    #[error("FOR EACH received {0} instead of ARRAY at {1}.")]
    ForeachNotArr(Type, Position),
    /// The input to an operator wasn't the expected type.
    #[error(
        "Incorrect type received for binary operator {op}. Expected {}, got lhs {lhs} and rhs {rhs} at {pos}",
        {
            let mut s = String::new();
            write_types(expected, &mut s).unwrap();
            s
        },
    )]
    IncorrectBinOperatorType {
        op: BinOperator,
        expected: Vec<Type>,
        lhs: Box<Type>,
        rhs: Box<Type>,
        pos: Position,
    },
    /// The index of an array was not of type INTEGER.
    #[error("Index expression received a {0} instead of an INTEGER index at {1}.")]
    IndexExprNotNum(Type, Position),
    /// An index expression had too many indices.
    ///
    /// # Example
    ///
    /// In the below example, `arr` is an ARRAY OF BOOLEANs, but we're accessing it as if it's a 2D
    /// array.
    ///
    /// ```sqarl
    /// DECLARE arr AS ARRAY OF BOOLEAN INITIALLY [ false, false, true ]
    /// DECLARE bool AS BOOLEAN INITIALLY arr[1][0]
    /// ```
    #[error("Depth of index is too deep, array does not go to depth {0} at {1}.")]
    IndexDepthMismatch(usize, Position),
    /// When using an index expression, the thing being indexed wasn't an ARRAY.
    #[error("Variable used in index is not an array, instead {0} at {1}")]
    IndexNotArray(Type, Position),
    /// A DECLARE or SET expression that had a type specified was initialised to a value that
    /// wasn't the right type.
    #[error(
        "Variable initialisation or setting of {0} invalid, expected {1}, actual type is {2} at {3}."
    )]
    InitWrongType(Id, Box<Type>, Box<Type>, Position),
    /// Mismatched type in [`check`]. (expected type, actual type, position)
    #[error("Mismatched type; expected {expected}, actually got {actual}, at {pos}")]
    MismatchedType {
        expected: Box<Type>,
        actual: Box<Type>,
        pos: Position,
    },
    /// The free variable wasn't found in the context.
    #[error("Undefined variable {0} at {1}.")]
    UndefinedVariable(Id, Position),
}

pub(crate) fn infer(
    expr: Expr,
    context: &mut HashMap<Id, Type>,
) -> Result<TypecheckedExpr, TypeError> {
    match expr.expr {
        ExprKind::Fv(id) => {
            if let Some(ty) = context.get(&id) {
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Fv(id),
                    expr.position,
                    ty.clone(),
                ))
            } else {
                Err(TypeError::UndefinedVariable(id.clone(), expr.position))
            }
        }
        ExprKind::Lit(lit) => Ok(TypecheckedExpr::new(
            TypedExprKind::Lit(lit),
            expr.position,
            lit.into(),
        )),
        ExprKind::Array(exprs) if exprs.is_empty() => {
            Err(TypeError::EmptyArrayInInferPosition(expr.position))
        }
        ExprKind::Array(exprs) => {
            assert!(!exprs.is_empty());
            let inferred_exprs = infer_exprs(exprs, Ok, context)?;
            // Safe to get first element because we know `exprs`' length is nonzero and
            // `infer_exprs` shouldn't change the length.
            let first_type = inferred_exprs[0].ty.clone();

            let joined_ty: Type = zip(1usize.., &inferred_exprs[1..]).try_fold(
                first_type.clone(),
                |acc, (index, e)| {
                    // TODO: apply coercions here too.
                    acc.join(&e.ty)
                        .ok_or_else(|| TypeError::ArrayElemsDifferingTypes {
                            ty: Box::new(acc.clone()),
                            index,
                            bad_ty: Box::new(e.ty.to_owned()),
                            bad_position: e.position,
                            pos: expr.position,
                        })
                },
            )?;

            // Check that all other elements have the same type.
            // for (index, e) in zip(1usize.., &inferred_exprs[1..]) {
            //     if e.ty != first_type {
            //         return Err(TypeError::ArrayElemsDifferingTypes {
            //             ty: Box::new(first_type),
            //             index,
            //             bad_ty: Box::new(e.ty.clone()),
            //             bad_position: e.position,
            //             pos: expr.position,
            //         });
            //     }
            // }
            Ok(TypecheckedExpr::new(
                TypedExprKind::Array(inferred_exprs),
                expr.position,
                Type::Array(Box::new(joined_ty)),
            ))
        }
        ExprKind::Index(id, nums) => {
            fn get_innermost_type(
                ty: &Type,
                original_depth: usize,
                position: Position,
                level: usize,
            ) -> Result<&Type, TypeError> {
                if level == 0 {
                    return Ok(ty);
                }
                match ty {
                    Type::Array(inner_ty) => {
                        get_innermost_type(inner_ty, original_depth, position, level - 1)
                    }
                    _ => Err(TypeError::IndexDepthMismatch(original_depth, position)),
                }
            }

            let checked_nums = infer_exprs(
                nums,
                move |e: TypecheckedExpr| {
                    if e.ty != Type::Integer {
                        Err(TypeError::IndexExprNotNum(e.ty, e.position))
                    } else {
                        Ok(e)
                    }
                },
                context,
            )?;
            let level = checked_nums.len();

            let fv_ty = context
                .get(&id)
                .ok_or_else(|| TypeError::UndefinedVariable(id.clone(), expr.position))?;

            if !matches!(fv_ty, Type::Array(..)) {
                return Err(TypeError::IndexNotArray(fv_ty.clone(), expr.position));
            }

            let ty = get_innermost_type(fv_ty, level, expr.position, level)?.clone();

            Ok(TypecheckedExpr::new(
                TypedExprKind::Index(id, checked_nums),
                expr.position,
                ty,
            ))
        }
        // Probably not sound, but we're not dealing with records at the moment.
        ExprKind::Record(id, fields) => {
            context.insert(id.clone(), Type::Record(id.clone(), fields.clone()));
            Ok(TypecheckedExpr::new(
                TypedExprKind::Record(id, fields),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::BinOp(lhs, op, rhs) => {
            let lhs_pos = lhs.position;
            let inferred_lhs = infer(*lhs, context)?;
            let inferred_rhs = infer(*rhs, context)?;

            let joined = op.check_and_join(&inferred_lhs.ty, &inferred_rhs.ty, expr.position)?;

            Ok(TypecheckedExpr::new(
                TypedExprKind::BinOp(Box::new(inferred_lhs), op, Box::new(inferred_rhs)),
                expr.position,
                joined,
            ))
        }
    }
}

/// Check an expression's type matches the `expected` type.
///
/// Note: usually in a bidirectional algorithm, we wouldn't have a return type. We would return
/// `Result<(), TypeError>`. However, because we need to change from one IR ([`Expr`]) to another
/// ([`TypecheckedExpr`]), it's easiest to return the typechecked expression. However, we do *not*
/// infer any types here.
pub(crate) fn check(
    expr: Expr,
    expected: &Type,
    context: &mut HashMap<Id, Type>,
) -> Result<TypecheckedExpr, TypeError> {
    match expr.expr {
        ExprKind::Record(id, fields) => {
            todo!("records are yet to be implemented");
        }
        ExprKind::Array(exprs) if exprs.is_empty() => {
            if !expected.is_array() {
                Err(TypeError::EmptyArrayAsNonArray(
                    Box::new(expected.to_owned()),
                    expr.position,
                ))
            } else {
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Array(Vec::new()),
                    expr.position,
                    expected.to_owned(),
                ))
            }
        }
        ExprKind::Array(exprs) if expected.is_array() => {
            assert!(!exprs.is_empty());
            let Type::Array(inner_ty) = expected.clone() else {
                unreachable!("we checked for `expected.is_array()` above");
            };
            let checked_exprs = check_exprs(exprs, &inner_ty, context)?;
            Ok(TypecheckedExpr::new(
                TypedExprKind::Array(checked_exprs),
                expr.position,
                Type::Array(inner_ty),
            ))
        }
        _ => {
            let pos = expr.position;
            let inferred_expr = infer(expr, context)?;
            if let Some(coercion) = inferred_expr.ty.is_subtype_with_coercion(expected) {
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Coerced(coercion, Box::new(inferred_expr.expr)),
                    inferred_expr.position,
                    expected.clone(),
                ))
            } else {
                Err(TypeError::MismatchedType {
                    expected: Box::new(expected.to_owned()),
                    actual: Box::new(inferred_expr.ty),
                    pos,
                })
            }
        }
    }
}

/// Infer many expressions.
///
/// # Arguments
///
/// * `exprs` - expressions to infer.
/// * `with_check` - after calling [`infer`] on the current expression, also run an extra
///   check function. If you want. Or don't by giving [`Ok`] as the callback.
/// * `context` - the context required to call [`infer`].
fn infer_exprs(
    exprs: impl IntoIterator<Item = Expr>,
    mut with_check: impl FnMut(TypecheckedExpr) -> Result<TypecheckedExpr, TypeError>,
    context: &mut HashMap<Id, Type>,
) -> Result<Vec<TypecheckedExpr>, TypeError> {
    exprs
        .into_iter()
        .map(|e| infer(e, context).and_then(&mut with_check))
        .collect()
}

/// Check many expressions.
///
/// # Arguments
///
/// * `exprs` - expressions to check.
/// * `context` - the context required to call [`check`].
fn check_exprs(
    exprs: impl IntoIterator<Item = Expr>,
    expected: &Type,
    context: &mut HashMap<Id, Type>,
) -> Result<Vec<TypecheckedExpr>, TypeError> {
    exprs
        .into_iter()
        .map(|e| check(e, expected, context))
        .collect()
}

fn check_stmt(stmt: Stmt, context: &mut HashMap<Id, Type>) -> Result<TypecheckedStmt, TypeError> {
    match stmt.stmt {
        StmtKind::Declare(id, None, ValueSource::Expr(expr)) => {
            let inferred_expr = infer(expr, context)?;
            context.insert(id.clone(), inferred_expr.ty.clone());
            let kind = TypedStmtKind::Declare(
                id,
                inferred_expr.ty.clone(),
                TypecheckedValueSource::Expr(inferred_expr),
            );
            Ok(TypecheckedStmt::new(kind, stmt.position))
        }
        StmtKind::Declare(id, None, ValueSource::Keyboard) => {
            Err(DeclWithoutKnownType(stmt.position))
        }
        StmtKind::Declare(id, Some(annotated_ty), ValueSource::Expr(expr)) => {
            let checked_expr = check(expr, &annotated_ty, context)?;
            context.insert(id.clone(), annotated_ty.clone());
            Ok(TypecheckedStmt::new(
                TypedStmtKind::Declare(
                    id,
                    annotated_ty,
                    TypecheckedValueSource::Expr(checked_expr),
                ),
                stmt.position,
            ))
        }
        StmtKind::Declare(id, Some(annotated_ty), ValueSource::Keyboard) => {
            context.insert(id.clone(), annotated_ty.clone());
            let kind = TypedStmtKind::Declare(id, annotated_ty, TypecheckedValueSource::Keyboard);
            Ok(TypecheckedStmt::new(kind, stmt.position))
        }
        StmtKind::Set(id, set_expr) => {
            // unfortunate to require a clone here, can probably avoid it somehow.
            let Some(ty) = context.get(&id).map(ToOwned::to_owned) else {
                return Err(TypeError::UndefinedVariable(id, stmt.position));
            };
            let typed_set_expr = check(set_expr, &ty, context)?;
            Ok(TypecheckedStmt::new(
                TypedStmtKind::Set(id, typed_set_expr),
                stmt.position,
            ))
        }
        StmtKind::IfThenElse(cond_expr, then_stmts, else_stmts) => {
            let checked_cond = check(cond_expr, &Type::Boolean, context)?;
            let checked_then = check_stmts(then_stmts, context)?;
            let checked_else = check_stmts(else_stmts, context)?;
            Ok(TypecheckedStmt::new(
                TypedStmtKind::IfThenElse(checked_cond, checked_then, checked_else),
                stmt.position,
            ))
        }
        StmtKind::While(cond, body) => {
            let checked_cond = check(cond, &Type::Boolean, context)?;
            let checked_body = check_stmts(body, context)?;
            Ok(TypecheckedStmt::new(
                TypedStmtKind::While(checked_cond, checked_body),
                stmt.position,
            ))
        }
        StmtKind::Repeat(body, cond) => {
            // it should be okay to check the cond first as it shouldn't see variables declared in
            // the body. well, the spec never says one way or the other, so let's just do the
            // easier thing and copy-paste from the `While` case above.
            let checked_cond = check(cond, &Type::Boolean, context)?;
            let checked_body = check_stmts(body, context)?;
            Ok(TypecheckedStmt::new(
                TypedStmtKind::Repeat(checked_body, checked_cond),
                stmt.position,
            ))
        }
        StmtKind::Receive(id) => {
            let Some(ty) = context.get(&id).map(ToOwned::to_owned) else {
                return Err(TypeError::UndefinedVariable(id, stmt.position));
            };
            // Nothing to add to the context, that's all.
            Ok(TypecheckedStmt::new(
                TypedStmtKind::Receive(id),
                stmt.position,
            ))
        }
        StmtKind::For(id, low, high, maybe_step, body) => {
            let checked_low = check(low, &Type::Integer, context)?;
            let checked_high = check(high, &Type::Integer, context)?;
            let checked_step = if let Some(step) = maybe_step {
                Some(check(step, &Type::Integer, context)?)
            } else {
                None
            };
            context.insert(id.clone(), Type::Integer);
            let checked_body = check_stmts(body, context)?;
            Ok(TypecheckedStmt::new(
                TypedStmtKind::For(id, checked_low, checked_high, checked_step, checked_body),
                stmt.position,
            ))
        }
        StmtKind::ForEach(id, arr_expr, body) => {
            let inferred_arr = infer(arr_expr, context)?;
            if let Type::Array(inner_type) = inferred_arr.ty.clone() {
                context.insert(id.clone(), *inner_type.clone());
                let checked_body = check_stmts(body, context)?;
                Ok(TypecheckedStmt::new(
                    TypedStmtKind::ForEach(id, inferred_arr, checked_body),
                    stmt.position,
                ))
            } else {
                Err(TypeError::ForeachNotArr(inferred_arr.ty, stmt.position))
            }
        }
    }
}

fn check_stmts(
    stmts: impl IntoIterator<Item = Stmt>,
    context: &mut HashMap<Id, Type>,
) -> Result<Vec<TypecheckedStmt>, TypeError> {
    stmts
        .into_iter()
        .map(|stmt| check_stmt(stmt, context))
        .collect()
}

pub fn typecheck_program(stmts: Vec<Stmt>) -> Result<Vec<TypecheckedStmt>, TypeError> {
    let mut context: HashMap<Id, Type> = HashMap::new();
    check_stmts(stmts, &mut context)
}

#[cfg(test)]
mod tests {
    use crate::expr::Literal;

    use super::*;

    #[test]
    fn int_to_real_coercion() {
        let expr = Expr {
            position: Position { line: 1, column: 1 },
            expr: ExprKind::BinOp(
                Box::new(Expr {
                    position: Position { line: 1, column: 1 },
                    expr: ExprKind::Lit(Literal::Integer(2)),
                }),
                BinOperator::Plus,
                Box::new(Expr {
                    position: Position { line: 1, column: 5 },
                    expr: ExprKind::Lit(Literal::Real(2.0)),
                }),
            ),
        };
        let checked: TypecheckedExpr = check(expr, &Type::Real, &mut HashMap::new()).unwrap();
        assert_eq!(Type::Real, checked.ty);
    }

    #[test]
    fn array_of_integer() {
        let expr = Expr {
            position: Position { line: 1, column: 1 },
            expr: ExprKind::Array(vec![
                Expr {
                    position: Position { line: 1, column: 2 },
                    expr: ExprKind::Lit(Literal::Integer(0)),
                },
                Expr {
                    position: Position { line: 1, column: 5 },
                    expr: ExprKind::Lit(Literal::Integer(1)),
                },
            ]),
        };
        let checked = check(
            expr,
            &Type::Array(Box::new(Type::Integer)),
            &mut HashMap::new(),
        )
        .unwrap();
        assert_eq!(Type::Array(Box::new(Type::Integer)), checked.ty);
    }

    #[test]
    fn disallow_array_elems_differing_types() {
        // [0, false]
        let expr = Expr {
            position: Position { line: 1, column: 1 },
            expr: ExprKind::Array(vec![
                Expr {
                    position: Position { line: 1, column: 2 },
                    expr: ExprKind::Lit(Literal::Integer(0)),
                },
                Expr {
                    position: Position { line: 1, column: 5 },
                    expr: ExprKind::Lit(Literal::Boolean(false)),
                },
            ]),
        };
        let res = check(
            expr,
            &Type::Array(Box::new(Type::Integer)),
            &mut HashMap::new(),
        );
        assert!(res.is_err());
    }

    #[test]
    fn error_on_non_array_declared_using_empty_array() {
        // DECLARE x AS INTEGER INITIALLY []
        let stmt = Stmt {
            position: Position { line: 1, column: 1 },
            stmt: StmtKind::Declare(
                Id::new(String::from("x")).unwrap(),
                Some(Type::Integer),
                ValueSource::Expr(Expr {
                    position: Position {
                        line: 1,
                        column: 32,
                    },
                    expr: ExprKind::Array(Vec::new()),
                }),
            ),
        };
        assert!(check_stmt(stmt, &mut HashMap::new()).is_err());
    }

    #[test]
    fn infer_integer_and_real_array() {
        // DECLARE x INITIALLY [1, 2.5]
        let stmt = Stmt {
            position: Position { line: 1, column: 1 },
            stmt: StmtKind::Declare(
                Id::new(String::from("x")).unwrap(),
                None,
                ValueSource::Expr(Expr {
                    position: Position {
                        line: 1,
                        column: 21,
                    },
                    expr: ExprKind::Array(vec![
                        Expr {
                            position: Position {
                                line: 1,
                                column: 22,
                            },
                            expr: ExprKind::Lit(Literal::Integer(1)),
                        },
                        Expr {
                            position: Position {
                                line: 1,
                                column: 25,
                            },
                            expr: ExprKind::Lit(Literal::Real(2.5)),
                        },
                    ]),
                }),
            ),
        };
        let checked_stmt = check_stmt(stmt, &mut HashMap::new()).unwrap();
        let TypedStmtKind::Declare(id, ty, val_src) = checked_stmt.stmt else {
            panic!("expected stmt was not a Declare");
        };
        assert_eq!(Type::Array(Box::new(Type::Real)), ty);
    }

    #[test]
    fn cannot_assign_real_to_integer_decl() {
        let id = Id::new(String::from("x")).unwrap();
        let mut context: HashMap<Id, Type> = {
            let mut map = HashMap::new();
            map.insert(id.clone(), Type::Integer);
            map
        };
        // SET x TO 2.5
        let stmt = Stmt {
            position: Position { line: 1, column: 1 },
            stmt: StmtKind::Set(
                id,
                Expr {
                    position: Position {
                        line: 1,
                        column: 10,
                    },
                    expr: ExprKind::Lit(Literal::Real(2.5)),
                },
            ),
        };

        let res = check_stmt(stmt, &mut context);
        assert!(res.is_err(), "{:?}", res.unwrap());
    }

    #[test]
    fn declare_real() {
        // DECLARE x AS REAL INITIALLY 42
        let stmt = Stmt {
            position: Position { line: 1, column: 1 },
            stmt: StmtKind::Declare(
                Id::new(String::from("x")).unwrap(),
                Some(Type::Real),
                ValueSource::Expr(Expr {
                    position: Position {
                        line: 1,
                        column: 32,
                    },
                    expr: ExprKind::Lit(Literal::Integer(42)),
                }),
            ),
        };
        let checked_stmt = check_stmt(stmt, &mut HashMap::new()).unwrap();
        println!("{checked_stmt:#?}");
    }

    #[test]
    fn running_total() {
        //         let code = String::from("
        // DECLARE total INITIALLY 0
        // FOR loop FROM 1 TO 10 DO
        //     DECLARE number AS INTEGER INITIALLY FROM KEYBOARD
        //     SET total TO total + number
        // END FOR
        // ");
        let mut program = vec![
            Stmt {
                position: Position { line: 1, column: 1 },
                stmt: StmtKind::Declare(
                    Id::new(String::from("total")).unwrap(),
                    None,
                    ValueSource::Expr(Expr {
                        position: Position {
                            line: 1,
                            column: 25,
                        },
                        expr: ExprKind::Lit(Literal::Integer(0)),
                    }),
                ),
            },
            Stmt {
                position: Position { line: 2, column: 1 },
                stmt: StmtKind::For(
                    Id::new(String::from("loop")).unwrap(),
                    Expr {
                        position: Position {
                            line: 2,
                            column: 15,
                        },
                        expr: ExprKind::Lit(Literal::Integer(1)),
                    },
                    Expr {
                        position: Position {
                            line: 2,
                            column: 20,
                        },
                        expr: ExprKind::Lit(Literal::Integer(10)),
                    },
                    None,
                    vec![
                        Stmt {
                            position: Position { line: 3, column: 4 },
                            //stmt: StmtKind::Receive(Id::new(String::from("number")).unwrap()),
                            stmt: StmtKind::Declare(
                                Id::new(String::from("number")).unwrap(),
                                Some(Type::Integer),
                                ValueSource::Keyboard,
                            ),
                        },
                        Stmt {
                            position: Position { line: 4, column: 4 },
                            stmt: StmtKind::Set(
                                Id::new(String::from("total")).unwrap(),
                                Expr {
                                    position: Position {
                                        line: 4,
                                        column: 18,
                                    },
                                    expr: ExprKind::BinOp(
                                        Box::new(Expr {
                                            position: Position {
                                                line: 4,
                                                column: 18,
                                            },
                                            expr: ExprKind::Fv(
                                                Id::new(String::from("total")).unwrap(),
                                            ),
                                        }),
                                        BinOperator::Plus,
                                        Box::new(Expr {
                                            position: Position {
                                                line: 4,
                                                column: 26,
                                            },
                                            expr: ExprKind::Fv(
                                                Id::new(String::from("number")).unwrap(),
                                            ),
                                        }),
                                    ),
                                },
                            ),
                        },
                    ],
                ),
            },
        ];

        let checked_program_res = typecheck_program(program);
        match checked_program_res {
            Ok(p) => println!("{p:#?}"),
            Err(e) => println!("{e}"),
        }
    }
}
