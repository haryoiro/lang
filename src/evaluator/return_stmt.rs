use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_return_stmt(&self, env: &mut Environment) -> Object {
        let return_stmt = self.clone().unwrap_return_stmt();
        let value = return_stmt.to_owned().eval(env);
        if is_error_object(&value) {
            return value;
        }
        Object::Return(Box::new(value))
    }
}
