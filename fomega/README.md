# Fω

System-F extended with type constructors of λω.

## Syntax

Let the meta-variables
`k` range over kinds,
'T' range over type-variable names,
`x` range over variable names,
`t` range over types,
& `e` range over expressions.

```
k ::= * | k1 => k2
t ::= T | forall T::k, t | t1 -> t2 | fun T::k. t | t1 t2
e ::= x | fun x:t. e | e1 e2 | Lam T::k. e | e [t]
```

### λ-Cube

This system combines the powers of System-F & λω.
```
fun x:t. e (* term-binding term *)
e1 e2 (* term binding a term *)
Lam T::k. e (* type-binding term *)
e [t] (* term binding a type *)
fun T::k. t (* type-binding type *)
t1 t2 (* type binding a type *)
```

## Running a program

The command line template to run some program is:
```
dune exec ./bin/main.exe fomega -- <SEMANTICS> <FILENAME>
```

For example, to type `fomega/samples/id.fo`, enter:
```
dune exec ./bin/main.exe fomega -- -type fomega/samples/id.fo
```
