use std::io::{self, BufRead, Write};

use crate::{
    environment::Environment,
    error::print_parser_errors,
    evaluator::Eval,
    lexer::Lexer,
    parser::Parser,
};

const PROMPT: &str = ">> ";

pub fn start(stdin: &mut io::Stdin, stdout: &mut io::Stdout) {
    let mut env = Environment::new();
    loop {
        let mut input = String::new();
        print!("{} ", PROMPT);
        let _ = stdout.flush();
        stdin.lock().read_line(&mut input).unwrap();

        if input.is_empty() {
            continue;
        }

        let mut lexer = Lexer::new(&input);
        let mut parser = Parser::new(&mut lexer);
        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            print_parser_errors(&parser.errors);
            continue;
        }

        program.eval(&mut env);
    }
}
