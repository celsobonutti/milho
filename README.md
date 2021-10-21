# 🌽

Milho (corn in portuguese) is a toy dialect of Lisp written as a way to learn more about compilers.
There are implementations in:
- [Haskell](https://github.com/celsobonutti/milho)
- [Rust](https://github.com/celsobonutti/milho-rust)
- [Go](https://github.com/danfragoso/milho)
- [OCaml](https://github.com/renatoalencar/milho-ocaml)

## Running locally

You'll need `stack` installed on your computer. Then, simply run:

- `stack run repl` to run the milho repl; or
- `stack run <path-to-file>` to run the code from a file.
- `stack test` to run the tests

## Using milho

If you want to know more about the language, check [the language docs](LANGUAGE.md).  

For a simple yet comprehensible documentation for the built-in functions, check [the built-in docs](BUILTINS.md).  

Standard library is still a TO-DO, but you can check its code on `std/`
