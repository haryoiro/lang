use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_let_stmt(&self, env: &mut Environment) -> Object {
        let let_stmt = self.clone().unwrap_let_stmt();
        let value = let_stmt.1.eval(env);
        if is_error_object(&value) {
            return value;
        }
        let key = let_stmt.0.to_owned();
        let key = key.unwrap_identifier_lit();

        env.set(key, value.clone());
        value
    }
}
