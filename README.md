# Lambda-Calculi-OCaml
Lambda Calculus implementations in OCaml.

## Build
Enter:
```bash
make
```

## Run
To execute a lambda calculus program, enter:
```bash
dune exec ./bin/main.exe <which lambda calculi> -- <ARGS>
```

For example, to execute a vanilla lambda-calculus program under call-by-value enter:
```bash
dune exec ./bin/main.exe vanilla -- -cbv
```
