use std::io::{self, BufRead, Write};

use crate::{lexer::Lexer, token};

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

        loop {
            let token = lexer.next_token();
            if token.type_ == token::EOF {
                break;
            }
            println!("{:?}", token);
        }
    }
}
