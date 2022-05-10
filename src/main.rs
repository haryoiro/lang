extern crate monkey;

use std::io;

use monkey::repl;

fn main() {
    println!("Welcome to Monkey Language!");
    println!("Feel free to type in commands");
    repl::start(&mut io::stdin(), &mut io::stdout());
}
