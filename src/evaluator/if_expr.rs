use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_if_expr(&self, env: &mut Environment) -> Object {
        let if_expr = self.clone().unwrap_if_expr();
        let condition = if_expr.0.eval(env);
        if is_error_object(&condition) {
            return condition;
        }

        if condition == Object::Boolean(true) {
            if_expr.1.eval(env)
        } else if let Some(else_expr) = &if_expr.2 {
            else_expr.eval(env)
        } else {
            Object::Null
        }
    }
}
