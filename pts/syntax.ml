(** Syntax. *)
type sort = Prop | Suc of sort

(** Term syntax. *)
type ('a,'b) term =
  | Sort of sort
  | Var of 'a
  | Abs of 'b * ('a,'b) term * ('a,'b) term
  | App of ('a,'b) term * ('a,'b) term
  | Pi of 'b * ('a,'b) term * ('a,'b) term

let sort s = Sort s
let var a = Var a
let abs b x y = Abs (b,x,y)
let app x y = App (x,y)
let pi b x y = Pi (b,x,y)

(** Parsed syntax. *)
type p_term = (string,string) term

(** De Bruijn syntax. *)
type b_term = (int,unit) term

let rec (=?) (s1 : sort) (s2 : sort) : bool =
  match s1, s2 with
  | Prop, Prop -> true
  | Suc s1, Suc s2 -> s1 =? s2
  | _, _ -> false

let rec string_of_sort = function
  | Prop -> "*"
  | Suc Prop -> "□"
  | Suc (Suc Prop) -> "∆"
  | Suc s -> "S " ^ string_of_sort s
