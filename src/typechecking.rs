use std::collections::HashMap;

use thiserror::Error;

use crate::{
    expr::{Expr, ExprKind},
    parse::Position,
    types::{Field, Id, Type},
};

#[derive(Debug, Error)]
enum TypeError {
    #[error("Condition was not a BOOLEAN at {0}.")]
    CondNotBool(Position),
    #[error("FOR EACH expected an array, but found {0} at {1}.")]
    ExpectedArray(Type, Position),
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
        ExprKind::Declare(_, None, expr) => check(expr, context),
        ExprKind::DeclareFromKeyboard(id, Some(ty)) => Ok(ty),
        ExprKind::DeclareFromKeyboard(id, None) => Ok(&Type::NumOrString),
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
        ExprKind::For(id, expr, expr1, expr2, exprs) => todo!(),
        ExprKind::ForEach(id, expr, body) => {
            let iter_ty = check(expr, context)?;
            match iter_ty {
                &Type::Array(_) => {
                    check_exprs(body, context)?;
                    Ok(&Type::Unit)
                }
                ty => Err(TypeError::ExpectedArray(ty.clone(), expr.position)),
            }
        }
        ExprKind::Receive(id) => {
            if !context.contains_key(id) {
                context.insert(id.clone(), Type::NumOrString);
            }
            Ok(&Type::Unit)
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
