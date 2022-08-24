use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_block_stmt(&self, env: &mut Environment) -> Object {
        let block_stmt = self.clone().unwrap_block_stmt();
        let mut result = vec![];
        for stmt in &block_stmt {
            let evaluated = stmt.eval(env);
            if is_error_object(&evaluated) {
                return evaluated;
            }
            result.push(evaluated);
        }
        Object::Array(result)
    }
}
