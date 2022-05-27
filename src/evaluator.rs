
use std::process::id;

use crate::{
    ast::{ Expression,  Program, Statement},
    environment::Environment,
    object::{IObject, Object, ERROR_TYPE, RETURN_VALUE_TYPE}, builtins::BUILTINS,
};

fn is_error_object(obj: &Object) -> bool {
    match obj {
        Object::Error(_) => true,
        _ => false,
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Object;
}

impl Eval for Program {
    fn eval(&self, env: &mut Environment) -> Object {
        let mut result = Object::Null;
        for statement in &self.statements {
            result = statement.eval(env);

            match result {
                Object::ReturnValue(obj) => return *obj,
                Object::Error(err) => return Object::Error(err),
                _ => continue,
            }
        }
        result
    }
}

impl Eval for Statement {
    fn eval(&self, env: &mut Environment) -> Object {
        match self {
            Statement::Let { name, value } => {
                let value_obj = value.eval(env);
                if is_error_object(&value_obj) {
                    return value_obj;
                }
                env.set(name.to_string(), value_obj.clone());
                value_obj
            }
            Statement::Return { value } => {
                let res = value.eval(env);
                if is_error_object(&res) {
                    return res;
                }
                return Object::ReturnValue(Box::new(res));
            }
            Statement::Expression { expression } => expression.eval(env),
            Statement::Block { statements } => {
                return eval_block_statement(statements, env);
            }
        }
    }
}

impl Eval for Expression {
    fn eval(&self, env: &mut Environment) -> Object {
        match self {
            Expression::Integer { value } => Object::Integer(*value),
            Expression::String { value } => Object::String(value.to_string()),
            Expression::Boolean { value } => Object::Boolean(*value),
            Expression::Array { elements } => {
                if elements.len() == 1 && is_error_object(&elements[0].eval(env)) {
                    return elements[0].eval(env);
                }
                return Object::Array(elements.iter().map(|e| e.eval(env)).collect());
            },
            Expression::Index { left, index } => {
                let left = left.eval(env);
                if is_error_object(&left) {
                    return left;
                }
                let index = index.eval(env);
                if is_error_object(&index) {
                    return index;
                }
                return eval_index_expression(left, index);
            },
            Expression::Prefix { operator, right } => {
                let right_obj = right.eval(env);
                if is_error_object(&right_obj) {
                    return right_obj;
                }
                return eval_prefix_expression(operator.to_string(), right_obj);
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                let left_obj = left.eval(env);
                if is_error_object(&left_obj) {
                    return left_obj;
                }
                let right_obj = right.eval(env);
                if is_error_object(&right_obj) {
                    return right_obj;
                }
                return eval_infix_expression(operator.to_string(), left_obj, right_obj);
            }
            Expression::If {
                condition,
                consequence,
                alternative,
            } => {
                let cond = condition.eval(env);
                if is_error_object(&cond) {
                    return cond;
                }

                if is_truthy(cond) {
                    consequence.eval(env)
                } else if let Some(alt) = alternative {
                    alt.eval(env)
                } else {
                    Object::Null
                }
            }
            Expression::Identifier { value } => {
                return eval_identifier(value, env).clone();
            }
            Expression::Function { parameters, body } => {
                return Object::Function { parameters:parameters.to_vec(), body: body.clone(), env:env.clone() };
            }
            Expression::Call { function, arguments } => {
                let func = function.eval(env);
                if is_error_object(&func) {
                    return func;
                }
                let args = eval_expressions(arguments, env);
                if arguments.len() == 1 && is_error_object(&args[0]) {
                    return args[0].clone();
                }
                return apply_function(func, &args);
            }
        }
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    if is_error_object(&right) {
        return right;
    }

    match operator.as_str() {
        "!" => return eval_bang_operator_expression(right),
        "-" => return eval_minus_prefix_operator_expression(right),
        _ => return Object::Error(format!("unknown operator: {}{}", operator, right.typ())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(value) => Object::Boolean(!value),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.typ())),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match (&left, &right) {
        // 4 + 5 -> 9
        (&Object::Integer(left), &Object::Integer(right)) => {
            return eval_integer_infix_expression(operator, left, right);
        }
        // true == false -> false
        (&Object::Boolean(left), &Object::Boolean(right)) => {
            return eval_boolean_infix_expression(operator, left, right);
        }
        // <string|integer|boolean|null> +|([!=]={1,2}) <string|integer|boolean|null>
        (Object::String(_)|Object::Integer(_)|Object::Boolean(_)|Object::Null,
        Object::String(_)|Object::Integer(_)|Object::Boolean(_)|Object::Null) => {
            return eval_string_infix_expression(operator, left, right);
        }
        _ => {
            return Object::Error(format!(
                "type mismatch: {} {} {}",
                left.typ(),
                operator,
                right.typ()
            ))
        }

    }
}

fn eval_integer_infix_expression(operator: String, left: i64, right: i64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: {} {} {}", left, operator, right)),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_str() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
    }
}

fn eval_string_infix_expression(operator: String, left: Object, right: Object) -> Object {
    let left_str = match left.clone() {
        Object::String(value) => value,
        Object::Boolean(value) => value.to_string(),
        Object::Integer(value) => value.to_string(),
        Object::Null => "".to_string(),
        _ => return Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    };
    let right_str = match right.clone() {
        Object::String(value) => value,
        Object::Boolean(value) => value.to_string(),
        Object::Integer(value) => value.to_string(),
        Object::Null => "".to_string(),
        _ => return Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    };
    match operator.as_str() {
        "+" => Object::String(left_str + &right_str),
        "==" => Object::Boolean(left_str == right_str),
        "!=" => Object::Boolean(left_str != right_str),
        "===" => Object::Boolean(left == right),
        "!==" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    }
}

fn eval_index_expression(left: Object, index: Object) -> Object {
    match (&left, &index) {
        (Object::Array(_), Object::Integer(_)) => {
            return eval_array_index_expression(left, index);
        }
        _ => {
            return Object::Error(format!(
                "index operator not supported: {}",
                index.typ()
            ))
        }
    }
}

fn eval_array_index_expression(left: Object, index: Object) -> Object {
    match left {
        Object::Array(array) => {
            let idx = match index {
                Object::Integer(value) => value,
                _ => return Object::Error(format!("index operator not supported: {}", index.typ())),
            };
            if idx < 0 || idx > array.len() as i64 - 1 {
                return Object::Null;
            }
            return array[idx as usize].clone();
        }
        _ => {
            return Object::Error(format!(
                "index operator not supported: {}",
                left.typ()
            ))
        }
    }

}


fn eval_block_statement(block: &[Statement], env: &mut Environment) -> Object {
    let mut result = Object::Null;
    for statement in block {
        result = statement.eval(env);
        if result.typ() == RETURN_VALUE_TYPE || result.typ() == ERROR_TYPE {
            return result;
        }
    }
    result
}

fn eval_expressions(expressions: &[Expression], env: &mut Environment) -> Vec<Object> {
    let mut result = vec![];
    for expression in expressions {
        let evaluated = expression.eval(env);
        if is_error_object(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn eval_identifier<'a>(identifier: &'a str, env: &'a mut Environment) ->  Object {
    let obj = env.get(identifier);
    if let Ok(obj) = obj {
        return obj;
    }

    let builtin = BUILTINS.get(identifier);
    if let Some(builtin) = builtin {
        return builtin.clone();
    }

    return Object::Error(format!("identifier not found: {}", identifier));
}

fn apply_function(func: Object, args: &Vec<Object>) -> Object {
    match &func {
        Object::Function { parameters:_, body, env:_ } => {
            let extended_env = extend_function_env(func.clone(), &args);
            let mut extended_env = match extended_env {
                Some(env) => env,
                None => return Object::Error(format!("not enough arguments")),
            };
            let evaluated = body.eval(&mut extended_env);
            return unwrap_return_value(evaluated);
        }
        Object::Builtin { func, .. } => {
             func((&args).to_vec())
        }
        _ => return Object::Error(format!("not a function: {}", func.typ())),
    }
}

fn extend_function_env(func: Object, args: &[Object]) -> Option<Environment> {
    match func {
        Object::Function { parameters, body:_, env } => {
            let mut extended_env = Environment::new_enclosed(&env);


            for (i, param) in parameters.iter().enumerate() {
                match param {
                    Expression::Identifier { value } => {
                        if args.len() >= i {
                            extended_env.set(value.to_string(), args[i].clone());
                        } else {
                            continue;
                        }
                    }
                    _ => {
                        continue;
                    }
                }
            }

            return Some(extended_env);
        }
        _ => unreachable!(),
    }
}

fn unwrap_return_value(obj: Object) -> Object {
    match obj {
        Object::ReturnValue(obj) => *obj,
        _ => obj,
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null | Object::Boolean(false) => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::fmt::Display;

    use super::*;
    use crate::{
        error::print_parser_errors,
        object::{self, IObject}, lexer::Lexer, parser::Parser,
    };

    #[test]
    fn test_eval_integer_expression() {
        let inputs = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("5 + 2 * 10", 25),
            ("20 + 2 * -10", 0),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("{}", evaluated);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_eval_boolean_expression() {
        let inputs = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("{}", evaluated);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_bang_operator() {
        let inputs = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("{}", evaluated);
            test_boolean_object(evaluated, expected);
        }
    }

    #[test]
    fn test_if_else_expressoin() {
        let inputs = vec![
            ("if (true) { 10 }", 10),
            ("if (1) { 10 }", 10),
            ("if (1 < 2) { 10 }", 10),
            ("if (1 > 2) { 10 } else { 20 }", 20),
            ("if (1 < 2) { 10 } else { 20 }", 10),
        ];

        let null_inputs = vec![
            ("if (false) { 10 }", Object::Null),
            ("if (1 > 2) { 10 }", Object::Null),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval: {}", evaluated);
            test_integer_object(evaluated, expected);
        }
        for (input, _) in null_inputs {
            let evaluated = test_eval(input.to_string());
            test_null_object(evaluated);
        }
    }

    #[test]
    fn test_return_statements() {
        let inputs = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            (
                r#"if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                return 1;
            }"#,
                10,
            ),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let inputs = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            (
                r#"
                if (10 > 1) {
                    if (10 > 1) {
                        return true + false;
                    }
                    return 1;
                }
                "#,
                "unknown operator: BOOLEAN + BOOLEAN",
            ),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            test_error_object(evaluated, expected);
        }
    }

    #[test]
    fn test_let_statements() {
        let inputs = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; }";

        let evaluated = test_eval(input.to_string());
        println!("eval {}", evaluated);
        match evaluated {
            Object::Function{
                parameters,
                body,
                ..
            } => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters[0].to_string(), "x");

                let expected_body = "(x + 2)".to_string();
                assert_eq!(body.to_string(), expected_body);
            }
            _ => panic!("not a function object"),
        }
    }

    #[test]
    fn test_function_application() {
        let inputs = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            test_integer_object(evaluated, expected);
        }
    }

    #[test]
    fn test_closures() {

        let input = r#"
        let loop = fn(x) {
            fn(y) {
                if (y == 0) {
                    x;
                } else {
                    loop(x + 1)(y - 1);
                }
            };
        };
        loop(0)(3);

        let newAdder = fn(x) {
            fn(y) { x + y };
        };

        let addTwo = newAdder(2);
        addTwo(2);
        "#;

        let evaluated = test_eval(input.
        to_string());
        println!("eval {}", evaluated);
        test_integer_object(evaluated, 4);
    }

    #[test]
    fn test_string_literals() {
        let input = vec![
        ("\"Hello World!\"", "Hello World!"),
        ("\"Hello\\nWorld!\"", "Hello\nWorld!"),
        ("\"Hello\\rWorld!\"", "Hello\rWorld!"),
        ("\"\\n\\r\\t\\\"\\\\\"", "\n\r\t\"\\"),
        ];

        for (input, expected) in input {
            let evaluated = test_eval(input.to_string());
            println!("=====->\neval\n---\n{}\n---\nexpect\n---\n{}\n====<-\n\n", evaluated, expected);
            match evaluated {
                Object::String(s) => assert_eq!(s, expected),
                _ => panic!("not a string object"),
            }
        }
    }

    #[test]
    fn test_builtin_function() {
        let inputs:Vec<(&str, Box<dyn Display>)> = vec![
            ("len(\"\")", Box::new(0)),
            ("len(\"four\")", Box::new(4)),
            ("len(\"hello world\")", Box::new(11)),
            ("len(1)", Box::new("argument to `len` not supported, got INTEGER")),
            ("len(\"one\", \"two\")", Box::new("wrong number of arguments. got=2, want=1")),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            match evaluated {
                Object::String(s) => assert_eq!(s, expected.to_string()),
                Object::Integer(i) => assert_eq!(i, expected.to_string().parse::<i64>().unwrap()),
                Object::Error(e) => assert_eq!(e, expected.to_string()),
                _ => panic!("not an error object"),
            }
        }
    }

    #[test]
    fn test_parsing_array_literals() {
        let input = vec![
            "[1, 2 * 2, 3 + 3]",
        ];

        for input in input {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            match evaluated {
                Object::Array(a) => {
                    assert_eq!(a.len(), 3);
                    test_integer_object(a[0].clone(), 1);
                    test_integer_object(a[1].clone(), 4);
                    test_integer_object(a[2].clone(), 6);
                }
                _ => panic!("not an array object"),
            }
        }
    }

    #[test]
    fn test_array_index_expressions() {
        let inputs:Vec<(&str, Box<dyn Display>)> = vec![
            ("[1, 2, 3][0]", Box::new(1)),
            ("[1, 2, 3][1]", Box::new(2)),
            ("[1, 2, 3][2]", Box::new(3)),
            ("let i = 0; [1][i];", Box::new(1)),
            ("[1, 2, 3][1 + 1];", Box::new(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Box::new(3)),
            ("let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];", Box::new(6)),
            ("let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]", Box::new(2)),
            ("[1, 2, 3][3]", Box::new(Object::Null)),
            ("[1, 2, 3][-1]", Box::new(Object::Null)),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            match evaluated {
                Object::Integer(i) => assert_eq!(i, expected.to_string().parse::<i64>().unwrap()),
                Object::Null => assert_eq!(expected.to_string(), "null"),
                _ => panic!("not an integer object"),
            }
        }
    }

    fn test_eval(input: String) -> Object {
        let mut env = Environment::new();
        let mut lexer = Lexer::new(&input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            print_parser_errors(&parser.errors);
            return Object::Null;
        }

        program.eval(&mut env)
    }

    fn test_integer_object(obj: object::Object, expected: i64) -> bool {
        let out = match obj {
            Object::Integer(i) => i,
            _ => {
                panic!("object is not Integer");
            }
        };
        if out != expected {
            println!("{}", out);
            println!("{}", expected);
            return false;
        }

        return true;
    }

    fn test_boolean_object(obj: object::Object, expected: bool) -> bool {
        let out = match obj {
            Object::Boolean(b) => b,
            _ => {
                panic!("object is not Boolean. got={}", obj.typ());
            }
        };
        if out != expected {
            println!("{}", out);
            println!("{}", expected);
            return false;
        }

        return true;
    }

    fn test_null_object(obj: object::Object) -> bool {
        let out = match obj {
            Object::Null => true,
            _ => {
                panic!("object is not Null. got={}", obj.typ());
            }
        };
        if out != true {
            println!("{}", out);
            return false;
        }

        return true;
    }

    fn test_error_object(obj: object::Object, expected: &str) -> bool {
        let out = match obj {
            Object::Error(e) => e,
            _ => {
                println!("----");
                println!("object is not Error. got={}", obj.typ());
                println!("----");
                return false;
            }
        };
        if out != expected {
            println!("----");
            println!("out {}", out);
            println!("exp {}", expected);
            println!("----");
            return false;
        }

        return true;
    }
}
