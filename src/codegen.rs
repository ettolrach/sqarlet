#![allow(unused)]

use std::collections::HashMap;

use crate::{
    typechecking::{TypecheckedExpr, TypedExprKind},
    types::{Id, Type},
};

#[derive(Default)]
struct Generator {
    n: i64,
}

impl Generator {
    fn var(&mut self) -> String {
        self.n += 1;
        format!("var{}", self.n)
    }

    /// Generate code for an [`Expr`].
    ///
    /// # Safety
    ///
    /// Will panic if free variable wasn't declared. To handle this error, the expr must be
    /// typechecked first.
    fn generate(
        &mut self,
        TypecheckedExpr {
            position: _,
            expr,
            ty,
        }: &TypecheckedExpr,
        env: &mut HashMap<Id, String>,
    ) -> String {
        match expr {
            TypedExprKind::Fv(id) => {
                if let Some(name) = env.get(id) {
                    name.clone()
                } else {
                    panic!("Unknown free variable {id}.");
                }
            }
            TypedExprKind::Lit(lit) => lit.to_string(),
            TypedExprKind::Declare(id, maybe_ty, init_expr) => {
                let name = self.var();
                let to_return = if let Some(ty) = maybe_ty {
                    format!(
                        "let {}: {} = {}",
                        &name,
                        ty.rust_type(),
                        self.generate(init_expr, env)
                    )
                } else {
                    format!("let {} = {}", &name, self.generate(init_expr, env))
                };
                env.insert(id.clone(), name);
                to_return
            }
            TypedExprKind::DeclareFromKeyboard(id, Some(ty)) => {
                let name = self.var();

                let to_return = format!(
                    "let mut {name}: {} = read_from_keyboard().parse().unwrap()",
                    ty.rust_type()
                );
                env.insert(id.clone(), name);
                to_return
            }
            TypedExprKind::DeclareFromKeyboard(id, None) => {
                let name = self.var();
                let to_return =
                    format!("let mut {name}: NumOrString = read_from_keyboard().into()");
                env.insert(id.clone(), name);
                to_return
            }
            TypedExprKind::Set(id, set_expr) => {
                let name = if let Some(name) = env.get(id) {
                    name.clone()
                } else {
                    panic!("Unknown free variable {id}.");
                };
                match ty {
                    Type::Ftv(id) => todo!(),
                    Type::Unit | Type::Integer | Type::Real | Type::Boolean | Type::Character => {
                        format!("{name} = {}", self.generate(set_expr, env))
                    }
                    Type::Array(_) => {
                        format!("*{name}.borrow_mut() = {}", self.generate(set_expr, env))
                    }
                    Type::Indeterminate => todo!(),
                    Type::Record(id, fields) => todo!(),
                    Type::Function(args_ty, output_ty) => todo!(),
                }
            }
            TypedExprKind::Index(id, nums) => {
                todo!()
            }
            TypedExprKind::Record(id, fields) => todo!(),
            TypedExprKind::IfThenElse(cond, then_exprs, else_exprs) => {
                let mut then_env = env.clone();
                let mut else_env = env.clone();
                format!(
                    "if {} {{\n{}\n}} else {{\n{}\n}}",
                    self.generate(cond, env),
                    self.generate_several(then_exprs, &mut then_env),
                    self.generate_several(else_exprs, &mut else_env)
                )
            }
            TypedExprKind::While(cond, exprs) => format!(
                "while {} {{\n{}\n}}",
                self.generate(cond, env),
                self.generate_several(exprs, env)
            ),
            TypedExprKind::Repeat(exprs, cond) => {
                let exprs_code = self.generate_several(exprs, env);
                format!(
                    "{exprs_code}\nwhile {} {{\n{exprs_code}\n}}",
                    self.generate(cond, env)
                )
            }
            TypedExprKind::For(id, low_expr, high_expr, maybe_step_expr, body) => {
                let low_code = self.generate(low_expr, &mut env.clone());
                let high_code = self.generate(high_expr, &mut env.clone());
                let step_code = if let Some(step_expr) = maybe_step_expr {
                    format!(".step_by({})", self.generate(step_expr, &mut env.clone()))
                } else {
                    String::new()
                };
                let name = self.var();
                env.insert(id.clone(), name.clone());
                format!(
                    "for {name} in ({low_code}..{high_code}){step_code} {{\n{}\n}}",
                    self.generate_several(body, env)
                )
            }
            TypedExprKind::ForEach(id, expr, exprs) => todo!(),
            TypedExprKind::Receive(id) => {
                if let Some(name) = env.get(id) {
                    match ty {
                        Type::Ftv(id) => todo!(),
                        Type::Unit
                        | Type::Integer
                        | Type::Real
                        | Type::Boolean
                        | Type::Character => {
                            format!("{name} = read_from_keyboard().parse().unwrap()")
                        }
                        Type::Array(_) => {
                            panic!("Tried to receive from keyboard for an ARRAY type.")
                        }
                        Type::Indeterminate => unreachable!(),
                        Type::Record(id, fields) => {
                            panic!("Tried to receive from keyboard for a RECORD type.")
                        }
                        Type::Function(args_ty, output_ty) => unreachable!(),
                    }
                } else {
                    let name = self.var();
                    let to_return =
                        format!("let mut {name}: NumOrString = read_from_keyboard().into()");
                    env.insert(id.clone(), name);
                    to_return
                }
            }
            TypedExprKind::BinOp(lhs, operator, rhs) => {
                format!(
                    "{} {operator} {}",
                    self.generate(lhs, env),
                    self.generate(rhs, env)
                )
            }
        }
    }

    /// Generate code for several [`Expr`].
    ///
    /// # Safety
    ///
    /// Will panic if free variable wasn't declared. To handle this error, the expr must be
    /// typechecked first.
    fn generate_several(
        &mut self,
        exprs: &[TypecheckedExpr],
        env: &mut HashMap<Id, String>,
    ) -> String {
        let mut to_return = String::new();

        for expr in exprs {
            to_return.push_str(self.generate(expr, env).as_str());
            to_return.push('\n');
        }

        to_return
    }
}

pub fn generate_code(program: &[TypecheckedExpr]) -> String {
    let mut to_return = String::new();
    let mut generator = Generator::default();
    let mut env: HashMap<Id, String> = HashMap::new();

    for expr in program {
        to_return.push_str(generator.generate(expr, &mut env).as_str());
        to_return.push('\n');
    }

    to_return
}

#[cfg(test)]
mod tests {
    use crate::{
        expr::{Expr, ExprKind, Literal, Operator},
        parse::Position,
        typechecking::typecheck_program,
    };

    use super::*;

    #[test]
    fn running_total() {
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

        let typechecked_program = typecheck_program(program).unwrap();
        let rust_code = generate_code(&typechecked_program);
        println!("{rust_code}");
    }
}
