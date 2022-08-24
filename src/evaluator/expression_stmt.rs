use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_expr_stmt(&self, env: &mut Environment) -> Object {
        let exp = self.clone().unwrap_expr_stmt();
        let value = exp.to_owned().eval(env);
        if is_error_object(&value) {
            return value;
        }
        value
    }
}
