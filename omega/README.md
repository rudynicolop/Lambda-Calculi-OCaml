# (λω)

The simply-typed lambda-calculus extended with type operators/constructors.

## Syntax

Let the meta variable
`T` range over type variables,
`x` range over term variables,
`k` range over kinds,
`t` range over types,
& `e` range over terms.
```
k ::= * | k1 --> k2

t ::= Bot | T | Fun T::k. t | t1 t2 | t1 -> t2

e ::= x | fun x:t => e | e1 e2
```
