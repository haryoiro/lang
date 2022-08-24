use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_array_expr(&self, env: &mut Environment) -> Object {
        let array_expr = self.clone().unwrap_array_expr();
        let mut result = vec![];
        for expression in array_expr {
            let evaluated = expression.eval(env);
            if is_error_object(&evaluated) {
                return evaluated;
            }
            result.push(evaluated);
        }
        Object::Array(result)
    }
}
