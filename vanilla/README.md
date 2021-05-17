# Vanilla Lambda Calculus

Plain, untyped lambda calculus.

## Syntax

Let the meta-variable `x` range over variable names, & `e` range over expressions.

```
e ::= x | fun x => e | e1 e2
```

## Running a program

The command line template to run some program is:
```
dune exec ./bin/main.exe vanilla -- <SEMANTICS> <FILENAME>
```

For example, to run `vanilla/samples/id.lambda` under call-by-value, enter:
```
dune exec ./bin/main.exe vanilla -- -cbv vanilla/samples/id.lambda
```
