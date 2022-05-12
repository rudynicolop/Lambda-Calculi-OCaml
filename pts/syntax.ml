(** Syntax. *)

(** Term syntax. *)
type ('s,'a,'b) term =
  | Sort of 's
  | Var of 'a
  | Abs of 'b * ('s,'a,'b) term * ('s,'a,'b) term
  | App of ('s,'a,'b) term * ('s,'a,'b) term
  | Pi of 'b * ('s,'a,'b) term * ('s,'a,'b) term

let sort s = Sort s
let var a = Var a
let abs b x y = Abs (b,x,y)
let app x y = App (x,y)
let pi b x y = Pi (b,x,y)

(** Parsed syntax. *)
type 's p_term = ('s,string,string) term

(** De Bruijn syntax. *)
type 's b_term = ('s,int,unit) term
