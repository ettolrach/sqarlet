use std::collections::HashMap;

use thiserror::Error;

use crate::{
    expr::{Expr, ExprKind, Literal, Operator},
    parse::Position,
    types::{Field, Id, Type},
};

#[derive(Debug, Clone)]
pub enum TypedExprKind {
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
    Declare(Id, Option<Type>, Box<TypecheckedExpr>),
    DeclareFromKeyboard(Id, Option<Type>),
    /// Varaible assigment.
    Set(Id, Box<TypecheckedExpr>),
    /// Array indexing. In the form `Id[u64][u64]...`.
    Index(Id, Vec<TypecheckedExpr>),
    /// Record declaration.
    Record(Id, Vec<Field>),
    /// Conditional.
    IfThenElse(
        Box<TypecheckedExpr>,
        Vec<TypecheckedExpr>,
        Vec<TypecheckedExpr>,
    ),
    While(Box<TypecheckedExpr>, Vec<TypecheckedExpr>),
    Repeat(Vec<TypecheckedExpr>, Box<TypecheckedExpr>),
    /// For loop. ID of counter, low expr, high expr, maybe step expr, and then body.
    For(
        Id,
        Box<TypecheckedExpr>,
        Box<TypecheckedExpr>,
        Option<Box<TypecheckedExpr>>,
        Vec<TypecheckedExpr>,
    ),
    ForEach(Id, Box<TypecheckedExpr>, Vec<TypecheckedExpr>),
    /// Receive input from the keyboard (i.e. STDIN). The only documented input device is the
    /// keyboard, so this is always STDIN.
    Receive(Id),
    /// Binary operation.
    BinOp(Box<TypecheckedExpr>, Operator, Box<TypecheckedExpr>),
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
    /// An IF, WHILE, or UNTIL expression was not a boolean.
    #[error("Condition was not a BOOLEAN at {0}.")]
    CondNotBool(Position),
    /// Only ARRAYs are allowed as iterators in FOR EACH loops, but another type was supplied.
    #[error("FOR EACH expected an array, but found {0} at {1}.")]
    ExpectedArray(Type, Position),
    /// Bounds of a FOR loop weren't of type INTEGER.
    #[error("FOR received {0} instead of INTEGER for range at {1}.")]
    ForNotInt(Type, Position),
    /// The input to an operator wasn't the expected type.
    #[error("Incorrect type received for operator {0}. Expected {1}, got {2} at {3}.")]
    IncorrectOperatorType(Operator, Box<Type>, Box<Type>, Position),
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
    MismatchedType(Id, Box<Type>, Box<Type>, Position),
    /// The free variable wasn't found in the context.
    #[error("Undefined variable {0} at {1}.")]
    UndefinedVariable(Id, Position),
}

pub(crate) fn check(
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
        ExprKind::Declare(id, Some(ty), init_expr) => {
            let pos = init_expr.position;
            let init_expr_typechecked = check(*init_expr, context)?;
            if ty == init_expr_typechecked.ty {
                context.insert(id.clone(), ty.clone());
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Declare(id, Some(ty), Box::new(init_expr_typechecked)),
                    expr.position,
                    Type::Unit,
                ))
            } else {
                Err(TypeError::MismatchedType(
                    id.clone(),
                    Box::new(ty.clone()),
                    Box::new(init_expr_typechecked.ty),
                    pos,
                ))
            }
        }
        ExprKind::Declare(id, None, init_expr) => {
            let init_expr_typechecked = check(*init_expr, context)?.clone();
            let ty = init_expr_typechecked.ty.clone();
            context.insert(id.clone(), ty.clone());

            // Should we change `None` to `Some(ty)` here? Probably not because that's not what the
            // user typed and we've got the actual type in `init_expr_typechecked`.
            Ok(TypecheckedExpr::new(
                TypedExprKind::Declare(id, None, Box::new(init_expr_typechecked)),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::DeclareFromKeyboard(id, Some(ty)) => {
            // Whether the user typed something of the correct type cannot be checked at compile
            // time. The spec says "The type of the value read from the input device/file is
            // inferred from the variable array element / record field type." We can only assume
            // that it's a runtime error if the type isn't as expected.
            context.insert(id.clone(), ty.clone());
            Ok(TypecheckedExpr::new(
                TypedExprKind::DeclareFromKeyboard(id, Some(ty)),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::DeclareFromKeyboard(id, None) => {
            context.insert(id.clone(), Type::Indeterminate);
            Ok(TypecheckedExpr::new(
                TypedExprKind::DeclareFromKeyboard(id, None),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::Set(id, set_expr) => {
            let pos = set_expr.position;
            let ty = context
                .get(&id)
                .ok_or_else(|| TypeError::UndefinedVariable(id.clone(), pos))?
                .clone();

            let expr_checked = check(*set_expr, context)?;
            if ty == expr_checked.ty {
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Set(id, Box::new(expr_checked)),
                    pos,
                    Type::Unit,
                ))
            } else {
                Err(TypeError::MismatchedType(
                    id.clone(),
                    Box::new(ty),
                    Box::new(expr_checked.ty),
                    pos,
                ))
            }
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

            let checked_nums = check_exprs(
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
        ExprKind::Record(id, fields) => {
            context.insert(id.clone(), Type::Record(id.clone(), fields.clone()));
            Ok(TypecheckedExpr::new(
                TypedExprKind::Record(id, fields),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::IfThenElse(cond, then_exprs, else_exprs) => {
            let cond_pos = cond.position;
            let checked_cond = check(*cond, context)?;
            if checked_cond.ty != Type::Boolean {
                return Err(TypeError::CondNotBool(cond_pos));
            }
            let checked_then = check_exprs(then_exprs, Ok, context)?;
            let checked_else = check_exprs(else_exprs, Ok, context)?;

            // SQARL's ifs evaluate to unit (like C), unlike the classic if-then-else.
            Ok(TypecheckedExpr::new(
                TypedExprKind::IfThenElse(Box::new(checked_cond), checked_then, checked_else),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::While(cond, body) => {
            let cond_pos = cond.position;
            let checked_cond = check(*cond, context)?;
            if checked_cond.ty == Type::Boolean {
                let checked_body = check_exprs(body, Ok, context)?;
                Ok(TypecheckedExpr::new(
                    TypedExprKind::While(Box::new(checked_cond), checked_body),
                    expr.position,
                    Type::Unit,
                ))
            } else {
                Err(TypeError::CondNotBool(cond_pos))
            }
        }
        ExprKind::Repeat(body, cond) => {
            let cond_pos = cond.position;
            let checked_cond = check(*cond, context)?;
            if checked_cond.ty == Type::Boolean {
                let checked_body = body
                    .into_iter()
                    .map(|e| check(e, context))
                    .collect::<Result<_, _>>()?;
                Ok(TypecheckedExpr::new(
                    TypedExprKind::Repeat(checked_body, Box::new(checked_cond)),
                    expr.position,
                    Type::Unit,
                ))
            } else {
                Err(TypeError::CondNotBool(cond_pos))
            }
        }
        ExprKind::For(id, from_expr, to_expr, maybe_step_expr, body_expr) => {
            let from_pos = from_expr.position;
            let checked_from = check(*from_expr, context)?;
            if checked_from.ty != Type::Integer {
                return Err(TypeError::ForNotInt(checked_from.ty, from_pos));
            }

            let to_pos = to_expr.position;
            let checked_to = check(*to_expr, context)?;
            if checked_to.ty != Type::Integer {
                return Err(TypeError::ForNotInt(checked_to.ty, to_pos));
            }

            let checked_expr = if let Some(step_expr) = maybe_step_expr {
                let step_expr_pos = step_expr.position;
                let checked_step_expr = check(*step_expr, context)?;
                if checked_step_expr.ty != Type::Integer {
                    return Err(TypeError::ForNotInt(checked_step_expr.ty, step_expr_pos));
                }
                Some(Box::new(checked_step_expr))
            } else {
                None
            };

            context.insert(id.clone(), Type::Integer);
            let for_body_checked = check_exprs(body_expr, Ok, context)?;
            Ok(TypecheckedExpr::new(
                TypedExprKind::For(
                    id,
                    Box::new(checked_from),
                    Box::new(checked_to),
                    checked_expr,
                    for_body_checked,
                ),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::ForEach(id, meta_iterator, body) => {
            let checked_meta_iterator = check(*meta_iterator, context)?;
            let inner_type: Type = match &checked_meta_iterator.ty {
                Type::Array(ty) => *ty.clone(),
                ty => return Err(TypeError::ExpectedArray(ty.clone(), expr.position)),
            };
            context.insert(id.clone(), inner_type);
            let checked_body = check_exprs(body, Ok, context)?;
            Ok(TypecheckedExpr::new(
                TypedExprKind::ForEach(id, Box::new(checked_meta_iterator), checked_body),
                expr.position,
                Type::Unit,
            ))
        }
        ExprKind::Receive(id) => {
            let ty = match context.get(&id) {
                Some(ty) => ty.clone(),
                None => Type::Indeterminate,
            };
            context.insert(id.clone(), ty.clone());
            Ok(TypecheckedExpr::new(
                TypedExprKind::Receive(id),
                expr.position,
                ty,
            ))
        }
        ExprKind::BinOp(lhs, op, rhs) => {
            let lhs_pos = lhs.position;
            let checked_lhs = check(*lhs, context)?;
            let checked_rhs = check(*rhs, context)?;
            let result_type = op.result_type();

            op.check_types(&checked_lhs, &checked_rhs)?;

            Ok(TypecheckedExpr::new(
                TypedExprKind::BinOp(Box::new(checked_lhs), op, Box::new(checked_rhs)),
                expr.position,
                result_type,
            ))
        }
    }
}

/// Check many expressions.
///
/// # Arguments
///
/// * `exprs` - expressions to check.
/// * `maybe_with_check` - after calling [`check`] on the current expression, also run an extra
///   check function. If you want. Or don't.
/// * `context` - the context required to call [`check`].
fn check_exprs(
    exprs: impl IntoIterator<Item = Expr>,
    mut with_check: impl FnMut(TypecheckedExpr) -> Result<TypecheckedExpr, TypeError>,
    context: &mut HashMap<Id, Type>,
) -> Result<Vec<TypecheckedExpr>, TypeError> {
    // if let Some(mut with_check) = maybe_with_check {
    exprs
        .into_iter()
        .map(|e| check(e, context).and_then(&mut with_check))
        .collect()
    // } else {
    //     exprs.into_iter().map(|e| check(e, context)).collect()
    // }
}

pub fn typecheck_program(exprs: Vec<Expr>) -> Result<Vec<TypecheckedExpr>, TypeError> {
    let mut context: HashMap<Id, Type> = HashMap::new();
    check_exprs(exprs, Ok, &mut context)
}

#[cfg(test)]
mod tests {
    use crate::expr::Literal;

    use super::*;

    #[test]
    fn running_total() {
        //         let code = String::from("
        // DECLARE total INITIALLY 0
        // FOR loop FROM 1 TO 10 DO
        //     RECEIVE number FROM KEYBOARD
        //     SET total TO total + number
        // END FOR
        // ");
        let mut program = vec![
            Expr {
                position: Position { line: 1, column: 1 },
                expr: ExprKind::Declare(
                    Id::new(String::from("total")).unwrap(),
                    None,
                    Box::new(Expr {
                        position: Position {
                            line: 1,
                            column: 25,
                        },
                        expr: ExprKind::Lit(Literal::Integer(0)),
                    }),
                ),
            },
            Expr {
                position: Position { line: 2, column: 1 },
                expr: ExprKind::For(
                    Id::new(String::from("loop")).unwrap(),
                    Box::new(Expr {
                        position: Position {
                            line: 2,
                            column: 15,
                        },
                        expr: ExprKind::Lit(Literal::Integer(1)),
                    }),
                    Box::new(Expr {
                        position: Position {
                            line: 2,
                            column: 20,
                        },
                        expr: ExprKind::Lit(Literal::Integer(10)),
                    }),
                    None,
                    vec![
                        Expr {
                            position: Position { line: 3, column: 4 },
                            expr: ExprKind::Receive(Id::new(String::from("number")).unwrap()),
                        },
                        Expr {
                            position: Position { line: 4, column: 4 },
                            expr: ExprKind::Set(
                                Id::new(String::from("total")).unwrap(),
                                Box::new(Expr {
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
                                        Operator::Plus,
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
                                }),
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
