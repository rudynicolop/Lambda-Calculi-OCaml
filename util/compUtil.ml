open Core

type cmp = LT | EQ | GT

module type Comp = sig
  type t
  val (<=>) : t -> t -> cmp
end

module IntComp : Comp
  with type t = int = struct
  type t = int
  let (<=>) (m : int) (n : int) : cmp =
    let a = Int.compare m n in
    if a < 0 then LT
    else if a > 0 then GT
    else EQ
end
