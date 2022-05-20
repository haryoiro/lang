use std::io::{self, BufRead, Write};

use crate::{error::MError, lexer::Lexer, parser::Parser, token};

const PROMPT: &str = ">> ";

pub fn start(stdin: &mut io::Stdin, stdout: &mut io::Stdout) {
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

        loop {
            let program = parser.parse_program();
            if parser.errors.len() > 0 {
                print_parser_errors(&parser.errors);
                break;
            }

            println!("{}\n", program.to_string());
            break;
        }
    }
}

fn print_parser_errors(errors: &Vec<MError>) {
    for error in errors.iter() {
        println!("{}", error);
    }
}
