use std::collections::HashMap;

use thiserror::Error;

use crate::{
    expr::{Expr, ExprKind, Operator},
    parse::Position,
    types::{Id, Type},
};

#[derive(Debug, Error)]
pub enum TypeError {
    #[error("Condition was not a BOOLEAN at {0}.")]
    CondNotBool(Position),
    #[error("FOR EACH expected an array, but found {0} at {1}.")]
    ExpectedArray(Type, Position),
    #[error("FOR received {0} instead of INTEGER for range at {1}.")]
    ForNotInt(Type, Position),
    #[error("Incorrect type received for operator {0}. Expected {1}, got {2} at {3}.")]
    IncorrectOperatorType(Operator, Type, Type, Position),
    #[error("Variable declaration of {0} invalid, expected {1}, actual type is {2} at {3}.")]
    MismatchedType(Id, Type, Type, Position),
    #[error("Undefined Variable {0} at {1}")]
    UndefinedVariable(Id, Position),
}

fn check<'a>(expr: &'a Expr, context: &'a mut HashMap<Id, Type>) -> Result<&'a Type, TypeError> {
    match &expr.expr {
        ExprKind::Fv(id) => {
            if let Some(ty) = context.get(id) {
                Ok(ty)
            } else {
                Err(TypeError::UndefinedVariable(id.clone(), expr.position))
            }
        }
        ExprKind::Lit(lit) => Ok(lit.into()),
        ExprKind::Declare(id, Some(ty), expr) => {
            let declared_type = check(expr, context)?;
            if ty != declared_type {
                Err(TypeError::MismatchedType(
                    id.clone(),
                    ty.clone(),
                    declared_type.clone(),
                    expr.position,
                ))
            } else {
                Ok(ty)
            }
        }
        ExprKind::Declare(id, None, expr) => {
            let expr_type: Type = check(expr, context)?.clone();
            context.insert(id.clone(), expr_type);
            Ok(&Type::Unit)
        }
        ExprKind::DeclareFromKeyboard(id, Some(ty)) => {
            context.insert(id.clone(), ty.clone());
            Ok(&Type::Unit)
        }
        ExprKind::DeclareFromKeyboard(id, None) => {
            context.insert(id.clone(), Type::NumOrString);
            Ok(&Type::Unit)
        }
        ExprKind::Set(id, expr) => {
            if let Some(ty) = context.get(id) {
                // This is requried to release the immutable borrow of the HashMap.
                let ty = ty.clone();
                let new_type = check(expr, context)?;
                if &ty == new_type {
                    Ok(&Type::Unit)
                } else {
                    Err(TypeError::MismatchedType(
                        id.clone(),
                        ty.clone(),
                        new_type.clone(),
                        expr.position,
                    ))
                }
            } else {
                Err(TypeError::UndefinedVariable(id.clone(), expr.position))
            }
        }
        ExprKind::Record(id, items) => {
            context.insert(id.clone(), Type::Record(id.clone(), items.clone()));
            Ok(&Type::Unit)
        }
        ExprKind::IfThenElse(cond, then_exprs, else_exprs) => {
            let cond_type = check(cond, context)?;
            if cond_type != &Type::Boolean {
                Err(TypeError::CondNotBool(cond.position))
            } else {
                check_exprs(then_exprs, context)?;
                check_exprs(else_exprs, context)?;
                Ok(&Type::Unit)
            }
        }
        ExprKind::While(cond, body) | ExprKind::Repeat(body, cond) => {
            let cond_type = check(cond, context)?;
            if cond_type != &Type::Boolean {
                Err(TypeError::CondNotBool(cond.position))
            } else {
                check_exprs(body, context)?;
                Ok(&Type::Unit)
            }
        }
        ExprKind::For(id, from_expr, to_expr, maybe_step_expr, body_expr) => {
            let from_type = check(from_expr, context)?;
            if from_type != &Type::Integer {
                return Err(TypeError::ForNotInt(from_type.clone(), from_expr.position));
            }

            let to_type = check(to_expr, context)?;
            if to_type != &Type::Integer {
                return Err(TypeError::ForNotInt(to_type.clone(), to_expr.position));
            }

            if let Some(step_expr) = maybe_step_expr {
                let step_type = check(step_expr, context)?;
                if step_type != &Type::Integer {
                    return Err(TypeError::ForNotInt(step_type.clone(), step_expr.position));
                }
            }

            context.insert(id.clone(), Type::Integer);
            check_exprs(body_expr, context)?;
            Ok(&Type::Unit)
        }
        ExprKind::ForEach(id, expr, body) => {
            let iter_ty = check(expr, context)?;
            let inner_type: Type;
            match iter_ty {
                Type::Array(ty) => {
                    inner_type = *ty.clone();
                }
                ty => return Err(TypeError::ExpectedArray(ty.clone(), expr.position)),
            }
            context.insert(id.clone(), inner_type);
            check_exprs(body, context)?;
            Ok(&Type::Unit)
        }
        ExprKind::Receive(id) => {
            if !context.contains_key(id) {
                context.insert(id.clone(), Type::NumOrString);
            }
            Ok(&Type::Unit)
        }
        ExprKind::BinOp(lhs, op, rhs) => {
            // We have to do some weird checking since we need to support the unspecified type of a
            // STDIN input.
            let (lhs_type, rhs_type) = op.arg_types();
            let actual_lhs_type = check(lhs, context)?;
            let lhs_is_num_or_string = actual_lhs_type == &Type::NumOrString;
            if (actual_lhs_type != &lhs_type)
                && (lhs_is_num_or_string
                    && ((lhs_type != Type::Integer)
                        && (lhs_type != Type::Real)
                        && (lhs_type != Type::Array(Box::new(Type::Character)))))
            {
                return Err(TypeError::IncorrectOperatorType(
                    op.clone(),
                    lhs_type,
                    actual_lhs_type.clone(),
                    lhs.position,
                ));
            }

            let actual_rhs_type = check(rhs, context)?;
            let rhs_is_num_or_string = actual_rhs_type == &Type::NumOrString;
            if (actual_rhs_type != &rhs_type)
                && (rhs_is_num_or_string
                    && ((rhs_type != Type::Integer)
                        && (lhs_type != Type::Real)
                        && (rhs_type != Type::Array(Box::new(Type::Character)))))
            {
                return Err(TypeError::IncorrectOperatorType(
                    op.clone(),
                    rhs_type,
                    actual_rhs_type.clone(),
                    rhs.position,
                ));
            }

            Ok(op.result_type())
        }
    }
}

fn check_exprs(exprs: &[Expr], context: &mut HashMap<Id, Type>) -> Result<(), TypeError> {
    for expr in exprs {
        check(expr, context)?;
    }
    Ok(())
}

pub fn typecheck_program(exprs: &[Expr]) -> Result<(), TypeError> {
    let mut context: HashMap<Id, Type> = HashMap::new();
    check_exprs(exprs, &mut context)
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
        let program = vec![
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

        typecheck_program(&program).unwrap();
    }
}
