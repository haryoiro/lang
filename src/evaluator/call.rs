use super::{is_error_object, Eval};
pub use crate::ast::Node;
use crate::{environment::Environment, object::Object};

impl Node {
    pub(crate) fn eval_call_expr(&self, env: &mut Environment) -> Object {
        let call_expr = self.clone().unwrap_call_expr();
        let function = call_expr.0.eval(env);
        if is_error_object(&function) {
            return function;
        }

        let args = call_expr
            .1
            .iter()
            .map(|arg| arg.eval(env))
            .collect::<Vec<_>>();

        if args.iter().any(|arg| is_error_object(arg)) {
            return Object::Error(format!("wrong number of arguments"));
        }

        // apply function
        match function {
            Object::Function(params, body, env) => {
                let mut env = env.clone();
                for (i, arg) in args.iter().enumerate() {
                    env.set(params[i].clone().unwrap_identifier_lit(), arg.clone());
                }
                body.eval(&mut env)
            }
            Object::Builtin(name, builtin) => builtin(args),
            _ => Object::Error(format!("not a function: {}", function)),
        }
    }
}
