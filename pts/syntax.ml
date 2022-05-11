(** Syntax. *)

(** Sorts. *)
type sort = Type | Kind

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

(** Sort equality. *)
let (=?) (s1 : sort) (s2 : sort) : bool =
  match s1, s2 with
  | Type, Type
  | Kind, Kind -> true
  | _, _ -> false

(** Term equality. *)
let rec (==) (t1 : b_term) (t2 : b_term) : bool =
  match t1, t2 with
  | Sort s1, Sort s2 -> s1 =? s2
  | Var x1, Var x2 -> x1 = x2
  | Abs (_,x1,y1), Abs (_,x2,y2)
  | App (x1,y1), App (x2,y2)
  | Pi (_,x1,y1), Pi (_,x2,y2) -> x1 == x2 && y1 == y2
  | _, _ -> false
