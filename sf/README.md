# System-F

Lambda calculus with parametric polymorphism.

## Syntax

Let the meta-variable 'T' range over type-variable names,
`x` range over variable names,
`t` range over types,
& `e` range over expressions.

```
t ::= T | forall T, t | t1 -> t2
e ::= x | fun x:t => e | e1 e2 | Lam T. e | e [t]
```

## Running a program

The command line template to run some program is:
```
dune exec ./bin/main.exe sf -- <SEMANTICS> <FILENAME>
```

For example, to type `sf/samples/id.sf`, enter:
```
dune exec ./bin/main.exe sf -- -type sf/samples/id.sf
```
