use std::fs::{read, write};

mod lexer;
mod parser;
mod generator;

fn main() {
    let tokens = lexer::tokenize(&String::from_utf8(read("./examples/test.game").expect("No file test.gs")).unwrap());
    for tok in tokens.iter() {
        println!("{:?}", tok);
    }

    println!();

    let stmts = parser::parse(tokens);
    for stmt in stmts.iter() {
        println!("{:?}", stmt);
    }

    let result = generator::generate(stmts);
    write("./build/out.asm", &result).expect("No file");
}
