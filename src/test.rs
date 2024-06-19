use std::process::Stdio;

use super::*;

#[test]
fn gcd() {
    let test_files =
        fs::read_dir("tests/run").unwrap().filter_map(|x| match x {
            Ok(y) => y
                .file_name()
                .to_str()
                .to_owned()
                .filter(|z| z.starts_with("gcd"))
                .map(|x| x.to_owned().to_string()),
            Err(_) => None,
        });

    let raw_code =
        fs::read_to_string("samples/gcd.mila").expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();

    let program = parser::parse(tokens.into());
    codegen::codegen(program, "/tmp/gcd.ll");
    Command::new("clang")
        .arg("/tmp/gcd.ll")
        .arg("src/stdlib.c")
        .arg("-o")
        .arg("/tmp/gcd.out")
        .output()
        .unwrap();
    for tf in test_files {
        let output = Command::new("/tmp/gcd.out").output().unwrap();
        let input =
            fs::read_to_string(("tests/run/".to_owned() + &tf).as_str())
                .unwrap();
        assert_eq!(input, String::from_utf8_lossy(&output.stdout))
    }
}

#[test]
fn consts() {
    let test_files =
        fs::read_dir("tests/run").unwrap().filter_map(|x| match x {
            Ok(y) => y
                .file_name()
                .to_str()
                .to_owned()
                .filter(|z| z.starts_with("consts"))
                .map(|x| x.to_owned().to_string()),
            Err(_) => None,
        });

    let raw_code =
        fs::read_to_string("samples/consts.mila").expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();

    let program = parser::parse(tokens.into());
    codegen::codegen(program, "/tmp/consts.ll");
    assert!(Command::new("clang")
        .arg("/tmp/consts.ll")
        .arg("src/stdlib.c")
        .arg("-o")
        .arg("/tmp/consts.out")
        .output()
        .unwrap()
        .status
        .success());
    for tf in test_files {
        let output = Command::new("/tmp/consts.out").output().unwrap();
        let input =
            fs::read_to_string(("tests/run/".to_owned() + &tf).as_str())
                .unwrap();
        assert_eq!(input, String::from_utf8_lossy(&output.stdout))
    }
}

#[test]
fn factorization() {
    let test_files =
        fs::read_dir("tests/run").unwrap().filter_map(|x| match x {
            Ok(y) => y
                .file_name()
                .to_str()
                .to_owned()
                .filter(|z| z.starts_with("factorization"))
                .map(|x| x.to_owned().to_string()),
            Err(_) => None,
        });

    let raw_code = fs::read_to_string("samples/factorization.mila")
        .expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();

    let program = parser::parse(tokens.into());
    codegen::codegen(program, "/tmp/factorization.ll");
    Command::new("clang")
        .arg("/tmp/factorization.ll")
        .arg("src/stdlib.c")
        .arg("-o")
        .arg("/tmp/factorization.out")
        .output()
        .unwrap();
    for tf in test_files {
        let output = Command::new("/tmp/factorization.out").output().unwrap();
        let input =
            fs::read_to_string(("tests/run/".to_owned() + &tf).as_str())
                .unwrap();
        assert_eq!(input, String::from_utf8_lossy(&output.stdout))
    }
}
#[test]
fn expressions() {
    let mut test_files_in: Vec<_> = fs::read_dir("tests/run")
        .unwrap()
        .filter_map(|x| match x {
            Ok(y) => y
                .file_name()
                .to_str()
                .to_owned()
                .filter(|z| z.starts_with("expressions.") && z.ends_with(".in"))
                .map(|x| x.to_owned().to_string()),
            Err(_) => None,
        })
        .collect();
    let mut test_files_out: Vec<_> = fs::read_dir("tests/run")
        .unwrap()
        .filter_map(|x| match x {
            Ok(y) => y
                .file_name()
                .to_str()
                .to_owned()
                .filter(|z| {
                    z.starts_with("expressions.") && z.ends_with(".out")
                })
                .map(|x| x.to_owned().to_string()),
            Err(_) => None,
        })
        .collect();

    test_files_in.sort();
    test_files_out.sort();

    let raw_code = fs::read_to_string("samples/expressions.mila")
        .expect("File not found.");
    let tokens = tokenizer::tokenize(&raw_code).unwrap();

    let program = parser::parse(tokens.into());
    codegen::codegen(program, "/tmp/tmp.ll");
    Command::new("clang")
        .arg("/tmp/tmp.ll")
        .arg("src/stdlib.c")
        .arg("-o")
        .arg("/tmp/tmp.out")
        .output()
        .unwrap();
    for (tfi, tfo) in test_files_in.iter().zip(test_files_out.iter()) {
        let mut c = Command::new("/tmp/tmp.out")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let input =
            fs::read_to_string(("tests/run/".to_owned() + &tfi).as_str())
                .unwrap();
        if let Some(stdin) = c.stdin.as_mut() {
            stdin.write_all(input.as_bytes()).unwrap();
        }
        let ref_out =
            fs::read_to_string(("tests/run/".to_owned() + &tfo).as_str())
                .unwrap();
        let output = c.wait_with_output().unwrap();
        assert_eq!(ref_out, String::from_utf8_lossy(&output.stdout))
    }
}
