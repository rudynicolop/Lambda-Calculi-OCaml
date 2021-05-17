# Vanilla Lambda Calculus

Plain, untyped lambda calculus.

## Syntax

Let the meta-variable `x` range over variable names, & `e` range over expressions.

```
e ::= x | fun x => e | e1 e2
```

The internal representation relies upon de bruijn indexing, so free variables are distinct from closed terms.

## Running a program

To run a program under call-by-value, enter:
```
dune exec ./bin/main.exe vanilla -- -cbv <FILENAME>
```

```
dune exec ./bin/main.exe vanilla -- -cbv vanilla/samples/id.lambda
```