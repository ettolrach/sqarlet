use std::collections::HashMap;

use crate::{expr::Expr, types::Id};

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
        Expr {
            position: _,
            expr: expr,
        }: &Expr,
        env: &mut HashMap<Id, String>,
    ) -> String {
        match expr {
            crate::expr::ExprKind::Fv(id) => {
                if let Some(name) = env.get(id) {
                    name.clone()
                } else {
                    panic!("Unknown free variable {id}");
                }
            }
            crate::expr::ExprKind::Lit(lit) => lit.to_string(),
            crate::expr::ExprKind::Declare(id, maybe_ty, expr) => {
                let name = self.var();
                let to_return = if let Some(ty) = maybe_ty {
                    format!(
                        "let {}: {} = {}",
                        &name,
                        ty.rust_type(),
                        self.generate(expr, env)
                    )
                } else {
                    format!("let {} = {}", &name, self.generate(expr, env))
                };
                env.insert(id.clone(), name);
                to_return
            }
            crate::expr::ExprKind::DeclareFromKeyboard(id, Some(ty)) => {
                let name = self.var();
                let to_return = format!(
                    "let {name}: {} = read_from_keyboard().parse().unwrap()",
                    ty.rust_type()
                );
                env.insert(id.clone(), name);
                to_return
            }
            crate::expr::ExprKind::DeclareFromKeyboard(id, None) => {
                let name = self.var();
                let to_return = format!("let {name}: NumOrString = read_from_keyboard().into()");
                env.insert(id.clone(), name);
                to_return
            }
            crate::expr::ExprKind::Set(id, expr) => todo!(),
            crate::expr::ExprKind::Record(id, items) => todo!(),
            crate::expr::ExprKind::IfThenElse(cond, then_exprs, else_exprs) => {
                let mut then_env = env.clone();
                let mut else_env = env.clone();
                format!(
                    "if {} {{\n{}\n}} else {{\n{}\n}}",
                    self.generate(cond, env),
                    self.generate_several(then_exprs, &mut then_env),
                    self.generate_several(else_exprs, &mut else_env)
                )
            }
            crate::expr::ExprKind::While(cond, exprs) => format!(
                "while {} {{\n{}\n}}",
                self.generate(cond, env),
                self.generate_several(exprs, env)
            ),
            crate::expr::ExprKind::Repeat(exprs, cond) => {
                let exprs_code = self.generate_several(exprs, env);
                format!(
                    "{exprs_code}\nwhile {} {{\n{exprs_code}\n}}",
                    self.generate(cond, env)
                )
            }
            crate::expr::ExprKind::For(id, expr, expr1, expr2, exprs) => todo!(),
            crate::expr::ExprKind::ForEach(id, expr, exprs) => todo!(),
            crate::expr::ExprKind::Receive(id) => todo!(),
            crate::expr::ExprKind::BinOp(expr, operator, expr1) => todo!(),
        }
    }

    /// Generate code for several [`Expr`].
    ///
    /// # Safety
    ///
    /// Will panic if free variable wasn't declared. To handle this error, the expr must be
    /// typechecked first.
    fn generate_several(&mut self, exprs: &[Expr], env: &mut HashMap<Id, String>) -> String {
        let mut to_return = String::new();

        for expr in exprs {
            to_return.push_str(self.generate(expr, env).as_str());
            to_return.push('\n');
        }

        to_return
    }
}

pub fn generate_code(program: &[Expr]) -> String {
    let mut to_return = String::new();
    let mut generator = Generator::default();
    let mut env: HashMap<Id, String> = HashMap::new();

    for expr in program {
        to_return.push_str(generator.generate(expr, &mut env).as_str());
        to_return.push('\n');
    }

    to_return
}
