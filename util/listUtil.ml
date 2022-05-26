open Core

(** Get index of (first occurence of) element in list *)
let rec index_of ~eq:(eq : 'a -> 'a -> bool) (a : 'a)
  : 'a list -> int option = function
  | [] -> None
  | h :: t ->
    if eq h a then Some 0 else Option.(index_of ~eq:eq a t >>| (+) 1)

(** Get index of optional list. *)
let rec index_of_options ~eq:(eq : 'a -> 'a -> bool) (a : 'a)
  : 'a option list -> int option = function
  | [] -> None
  | Some h :: _ when eq h a -> Some 0
  | _ :: t -> Option.(index_of_options ~eq:eq a t >>| (+) 1)
