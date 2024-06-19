#![allow(clippy::redundant_closure_call)]
#![allow(clippy::upper_case_acronyms)]
use clap::Parser;
use std::process::Command;
use std::{fs, io::Write};

mod codegen;
mod parser;
#[cfg(test)]
mod test;
mod tokenizer;

#[derive(Parser, Debug)]
struct Args {
    /// File to run
    file: String,
    /// Path where to write output
    #[arg(short, long)]
    output: Option<String>,

    /// Compile?
    #[arg(short, long, default_value_t = false)]
    compile: bool,

    /// Print AST?
    #[arg(long, default_value_t = false)]
    print_ast: bool,
}

fn main() -> Result<(), String> {
    let args = Args::parse();
    let file = args.file;
    let raw_code = fs::read_to_string(&file).expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();

    let program = parser::parse(tokens.into());

    if args.print_ast {
        dbg!(&program);
    }

    let file_name = if let Some(o) = args.output {
        o
    } else if !args.compile {
        let file_name = file.split_at(file.rfind('/').map_or(0, |x| x + 1)).1;
        file_name
            .split_at(file_name.rfind('.').unwrap_or(file.len()))
            .0
            .to_owned()
            + ".ll"
    } else {
        "a.out".to_string()
    };

    codegen::codegen(
        program,
        if !args.compile {
            file_name.as_str()
        } else {
            "/tmp/tmp.ll"
        },
    );

    if args.compile {
        let output = Command::new("clang")
            .arg("-fcolor-diagnostics")
            .arg("/tmp/tmp.ll")
            .arg("src/stdlib.c")
            .arg("-o")
            .arg(file_name)
            .output()
            .unwrap();
        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();
    }
    Ok(())
}
