open Core

(** Identity function. *)
let id (a: 'a) : 'a = a

(** Function composition. *)
let (>>) f g = fun x -> x |> g |> f

(** Haskell-style dollar-sign *)
let ($) f x = f x

let my_ignore (_:'a) : unit = ()

(** Consume unused argument. *)
let consume (f: 'b -> 'c) : 'a -> 'b -> 'c = fun _ -> f

(** Apply a function [n] times. *)
let rec napply (n: int) (f: 'a -> 'a) (a: 'a) : 'a =
  if n <= 0 then a else f $ napply (n-1) f a

(** Multi-reduction. *)
let rec multi_red
    (red : 'a -> 'a option)
    (f : 'a -> unit) (e : 'a) : 'a option =
  let open Option in
  f e; red e >>= multi_red red f
