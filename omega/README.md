# λω

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

### Lambda Cube
λω extends λ→ with types parameterized by types:
```
fun x:t => e (* term-binding term *)
e1 e2 (* term binding a term *)
Fun T::k. t (* type-binding type *)
t1 t2 (* type binding a type *)
```
