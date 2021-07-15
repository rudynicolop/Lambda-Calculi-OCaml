open Core

(** Get index of (first occurence of) element in list *)
let rec index_of (eqb : 'a -> 'a -> bool) (a : 'a)
  : 'a list -> int option = function
  | [] -> None
  | h :: t ->
    if eqb h a then Some 0 else Option.(index_of eqb a t >>| (+) 1)

let index_of_default (eqb: 'a -> 'a -> bool) (a: 'a) (l: 'a list) : int =
  match index_of eqb a l with
  | Some n -> n
  | None -> List.length l
