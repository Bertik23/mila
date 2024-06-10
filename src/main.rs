#![allow(clippy::redundant_closure_call)]
use clap::Parser;
use std::{collections::LinkedList, fs};
// use std::io::stdin;
mod parser;
mod tokenizer;

#[derive(Parser, Debug)]
struct Args {
    /// Interactive mode, evaluates one line at a time. Really dumb. Not nice to use.
    #[arg(short, long, default_value_t = false)]
    interactive: bool,

    /// File to run
    file: String,
}

fn main() -> Result<(), String> {
    let args = Args::parse();
    let file = args.file;
    let raw_code = fs::read_to_string(file).expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();
    println!("{:?}", &tokens);

    let program = parser::parse(tokens.into());

    dbg!(program);

    // let p = parser::parse(&mut tokenizer::tokenize(raw_code.as_str()));
    // let out_code = codegen::compile(p).unwrap();
    // // println!("{}", out_code.clone().borrow());
    // let mut m = secd::mashine::SECD::new(out_code);
    // m.eval();
    // println!("{}", m);
    Ok(())
}
