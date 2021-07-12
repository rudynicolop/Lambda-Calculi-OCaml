# The Simply-Typed Lambda Calculus

Lambda calculus with simple types.

## Syntax

Let the meta-variable `x` range over variable names,
`t` range over types,
& `e` range over expressions.

```
t ::= False | t1 -> t2
e ::= x | fun x:t => e | e1 e2
```

## Running a program

The command line template to run some program is:
```
dune exec ./bin/main.exe stlc -- <SEMANTICS> <FILENAME>
```

For example, to run `stlc/samples/ill1.stlc` under call-by-value, enter:
```
dune exec ./bin/main.exe stlc -- -cbv stlc/samples/ill1.stlc
```
