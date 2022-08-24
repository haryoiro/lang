use crate::{ast::Node, environment::Environment, object::Object};

mod array;
mod block;
mod call;
mod expression_stmt;
mod function_expr;
mod hash;
mod if_expr;
mod index;
mod infix;
mod let_stmt;
mod literal;
mod prefix;
mod program;
mod return_stmt;

fn is_error_object(obj: &Object) -> bool {
    match obj {
        Object::Error(_) => true,
        _ => false,
    }
}

pub trait Eval {
    fn eval(&self, env: &mut Environment) -> Object;
}

impl Eval for Node {
    fn eval(&self, env: &mut Environment) -> Object {
        match self {
            Node::Program(..) => self.eval_program(env),
            Node::ArrayExpr(..) => self.eval_array_expr(env),
            Node::HashExpr(..) => self.eval_hash_expr(env),
            Node::IndexExpr(..) => self.eval_index_expr(env),
            Node::IfExpr(..) => self.eval_if_expr(env),
            Node::CallExpr(..) => self.eval_call_expr(env),
            Node::PrefixExpr(..) => self.eval_prefix_expr(env),
            Node::InfixExpr(..) => self.eval_infix_expr(env),
            Node::FunctionExpr(..) => self.eval_function_expr(env),

            Node::BlockStmt(_) => self.eval_block_stmt(env),
            Node::LetStmt(..) => self.eval_let_stmt(env),
            Node::ReturnStmt(_) => self.eval_return_stmt(env),
            Node::ExprStmt(..) => self.eval_expr_stmt(env),

            Node::IdentifierLit(_) => self.eval_identifier_lit(env),
            Node::IntegerLit(_) => self.eval_integer_lit(env),
            Node::StringLit(_) => self.eval_string_lit(env),
            Node::BooleanLit(_) => self.eval_boolean_lit(env),
        }
    }
}

#[cfg(test)]
mod tests {
    use core::panic;
    use std::fmt::Display;

    use super::*;
    use crate::{
        ast::Node,
        error::print_parser_errors,
        lexer::Lexer,
        object::{self, IObject},
        parser::Parser,
    };

    #[test]
    fn test_eval_integer_expr() {
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
    fn test_eval_boolean_expr() {
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
    fn test_return_stmts() {
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
    fn test_let_stmts() {
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
            Object::Function(parameters, body, ..) => {
                assert_eq!(parameters.len(), 1);
                assert_eq!(parameters[0].token_lit(), "x");

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

        let evaluated = test_eval(input.to_string());
        println!("eval {}", evaluated);
        test_integer_object(evaluated, 4);
    }

    #[test]
    fn test_string_lits() {
        let input = vec![
            ("\"Hello World!\"", "Hello World!"),
            ("\"Hello\\nWorld!\"", "Hello\nWorld!"),
            ("\"Hello\\rWorld!\"", "Hello\rWorld!"),
            ("\"\\n\\r\\t\\\"\\\\\"", "\n\r\t\"\\"),
        ];

        for (input, expected) in input {
            let evaluated = test_eval(input.to_string());
            println!(
                "=====->\neval\n---\n{}\n---\nexpect\n---\n{}\n====<-\n\n",
                evaluated, expected
            );
            match evaluated {
                Object::String(s) => assert_eq!(s, expected),
                _ => panic!("not a string object"),
            }
        }
    }

    #[test]
    fn test_builtin_function() {
        let inputs: Vec<(&str, Box<dyn Display>)> = vec![
            ("len(\"\")", Box::new(0)),
            ("len(\"four\")", Box::new(4)),
            ("len(\"hello world\")", Box::new(11)),
            (
                "len(1)",
                Box::new("argument to `len` not supported, got INTEGER"),
            ),
            (
                "len(\"one\", \"two\")",
                Box::new("wrong number of arguments. got=2, want=1"),
            ),
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
    fn test_parsing_array_lits() {
        let input = vec!["[1, 2 * 2, 3 + 3]"];

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
    fn test_array_index_exprs() {
        let inputs: Vec<(&str, Box<dyn Display>)> = vec![
            ("[1, 2, 3][0]", Box::new(1)),
            ("[1, 2, 3][1]", Box::new(2)),
            ("[1, 2, 3][2]", Box::new(3)),
            ("let i = 0; [1][i];", Box::new(1)),
            ("[1, 2, 3][1 + 1];", Box::new(3)),
            ("let myArray = [1, 2, 3]; myArray[2];", Box::new(3)),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Box::new(6),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
                Box::new(2),
            ),
            (
                "[1, 2, 3][3]",
                Box::new("index out of bounds: 3 > 2".to_string()),
            ),
            ("[1, 2, 3][-1]", Box::new(3)),
            ("[1, 2, 3][-2]", Box::new(2)),
            ("[1, 2, 3][-3]", Box::new(1)),
            (
                "[1, 2, 3][-4]",
                Box::new("index out of bounds: -4 > -3".to_string()),
            ),
        ];

        for (input, expected) in inputs {
            let evaluated = test_eval(input.to_string());
            println!("eval {}", evaluated);
            match evaluated {
                Object::Integer(i) => assert_eq!(i, expected.to_string().parse::<i64>().unwrap()),
                Object::Null => assert_eq!(expected.to_string(), "null"),
                Object::Error(e) => assert_eq!(e, expected.to_string()),
                _ => panic!("not an integer object"),
            }
        }
    }

    #[test]
    fn test_parsing_hash_lits_string_keys() {
        let input = r#"{"one": 1, "two": 2, "three": 3}"#;

        let mut lex = Lexer::new(&input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        let program = match program {
            Node::Program(p) => p,
            _ => panic!("not a program node"),
        };

        let stmts = &program[0];
        match stmts {
            Node::HashExpr(pairs) => {
                assert_eq!(pairs.0.len(), 3);
                for i in pairs.0.keys() {
                    if i.to_string() == "one" {
                        assert_eq!(pairs.0.get(i).unwrap().to_string(), "1");
                    }
                    if i.to_string() == "two" {
                        assert_eq!(pairs.0.get(i).unwrap().to_string(), "2");
                    }
                    if i.to_string() == "three" {
                        assert_eq!(pairs.0.get(i).unwrap().to_string(), "3");
                    }
                }
            }
            _ => panic!("not an expression stmt"),
        }
    }

    #[test]
    fn test_parsing_empty_hash_lit() {
        let input = "{}";

        let mut lex = Lexer::new(&input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        let program = match program {
            Node::Program(p) => p,
            _ => panic!("not a program node"),
        };

        let stmts = &program[0];
        match stmts {
            Node::HashExpr(pairs) => {
                assert_eq!(pairs.0.len(), 0);
            }
            _ => panic!("not an expression stmt"),
        }
    }

    #[test]
    fn test_parsing_hash_lits_with_exprs() {
        let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;

        let mut lex = Lexer::new(&input);
        let mut parser = Parser::new(&mut lex);
        let program = parser.parse_program();

        let program = match program {
            Node::Program(p) => p,
            _ => panic!("not a program node"),
        };

        let stmts = &program[0];
        match stmts {
            Node::HashExpr(pairs) => {
                assert_eq!(pairs.0.len(), 3);
                for i in pairs.0.keys() {
                    let a = pairs.0.get(i).unwrap();
                    if i == "one" {
                        test_infix_expr(a.clone(), 0, "+", 1);
                    } else if i == "two" {
                        test_infix_expr(a.clone(), 10, "-", 8);
                    } else if i == "three" {
                        test_infix_expr(a.clone(), 15, "/", 5);
                    }
                }
            }
            _ => panic!("not an expression stmt"),
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

    fn test_infix_expr(exp: Node, left: i64, operator: &str, right: i64) -> bool {
        match exp {
            Node::InfixExpr(i_left, i_operator, i_right) => {
                if i_operator != operator {
                    println!("----");
                    println!("operator is not {}. got={}", operator, i_operator);
                    println!("----");
                    return false;
                }

                let out = match *i_left {
                    Node::IntegerLit(i) => {
                        if left != i {
                            println!("----");
                            println!("left is not {}. got={}", left, i);
                            println!("----");
                            return false;
                        }
                        return true;
                    }
                    _ => false,
                };

                if out != true {
                    println!("----");
                    println!("left is not Integer. got={}", i_left);
                    println!("----");
                    return false;
                }

                let out = match *i_right {
                    Node::IntegerLit(i) => {
                        if right != i {
                            println!("----");
                            println!("right is not {}. got={}", right, i);
                            println!("----");
                            return false;
                        }
                        return true;
                    }
                    _ => false,
                };

                if out != true {
                    println!("----");
                    println!("right is not Integer. got={}", i_right);
                    println!("----");
                    return false;
                }

                return true;
            }
            _ => {
                println!("----");
                println!("exp is not Infix. got={}", exp);
                println!("----");
                return false;
            }
        };
    }
}
