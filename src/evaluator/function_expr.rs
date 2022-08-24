use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_function_expr(&self, env: &mut Environment) -> Object {
        let function = self.clone().unwrap_function_expr();
        let parameters = function.0;
        let body = function.1;
        Object::Function(parameters, body, env.clone())
    }
}
