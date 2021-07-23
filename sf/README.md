# (λ2) System-F

λ-calculus with parametric polymorphism.

## Syntax

Let the meta-variable 'T' range over type-variable names,
`x` range over variable names,
`t` range over types,
& `e` range over expressions.

```
t ::= T | forall T, t | t1 -> t2
e ::= x | fun x:t => e | e1 e2 | Lam T. e | e [t]
```

### λ-Cube

System-F extends λ→ with terms parameterized by types:
```
fun x:t => e (* term-binding term *)
e1 e2 (* term binding a term *)
Lam T. e (* type-binding term *)
e [t] (* term binding a type *)
```
System-F ***does not*** feature types parameterized by types,
do not be beguiled by types `forall T, t`!
`forall T, t` is merely the type of type-binding terms `Lam T. e`.
There is no accompanying syntax to substitute a type into a `forall T, t`.

## Running a program

The command line template to run some program is:
```
dune exec ./bin/main.exe sf -- <SEMANTICS> <FILENAME>
```

For example, to type `sf/samples/id.sf`, enter:
```
dune exec ./bin/main.exe sf -- -type sf/samples/id.sf
```
