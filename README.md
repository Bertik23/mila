# Semestral Work

A Pascal like expression driven language

# Semestral work structure

- `main.rs` - main function
- `tokenizer.rs` - Tokenization (lexical analysis) related sources
- `parser.rs` - parser of the token stream provided by the tokenizer
- `test.rs` - some tests
- `stdlib.c` - "standard library" for mila (suplies IO functions and some more)
- `samples` - directory with samples describing syntax and some custom ones
- `mila` - wrapper script for your compiler

# Dependencies

* `llvm 17`
* `cargo`

# How to run

`cargo build --release`

`./mila [INPUT_FILE] [OUTPUT_FILE]`


# Implemented features

* Base implementation
* Number literals in decimal, octal and hexadecimal base
* If and while loop with break
* For (to and downto) with break
* Nested blocks
* Functions, procedures local variables, exit
* Function parameters
* Recursion
* Indirect recursion
