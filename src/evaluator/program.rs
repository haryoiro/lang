use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_program(&self, env: &mut Environment) -> Object {
        let mut result = Object::Null;
        for stmt in &self.clone().unwrap_program() {
            result = stmt.eval(env);
            if is_error_object(&result) {
                return result;
            }
        }
        result
    }
}
