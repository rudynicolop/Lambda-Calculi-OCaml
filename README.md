# Lambda-Calculi-OCaml

λ-calculus implementations in OCaml, including the λ-cube.
Meant as a tool to understand various λ-calculi & their type-systems.

## Features

All language implementations support
call-by-value, call-by-name,
full normal-order, & full applicative-order.

Some language implementations support
call-by-value & call-by-name in higher-order abstract syntax.

All typed languages are Church style with explicit type-annotations.

### TODOs

- λ→ extended with arithmetic (System-T), with a compiler to λ→.
- System-F,Fω extended with existential types,
with a compiler to λ2,Fω respectively.
- Type erasure to vanilla λ-calculus.
- Dependent types, the rest of the λ-cube, including COC.
- Lexical support for comments.
- Languages with Subtyping?
- System-U.
- A Hindley-Milner type-system.
- GADT-based higher-order implementations for typed-calculi.
- Call-by-need implementations.

## Execution

To build, enter:
```bash
make
```

To run a lambda calculus program, enter:

```bash
dune exec ./bin/main.exe <which lambda calculi> -- <ARGS>
```

See each directory for specific command line arguments.

## Sources

My syntax, semantics, and implementations are based upon
those as described in
[Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)
and the lecture notes for
[Cornell CS 4110, Fall 2018](https://www.cs.cornell.edu/courses/cs4110/2020fa/schedule.html).
