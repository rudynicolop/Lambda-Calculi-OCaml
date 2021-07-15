(** Syntax. *)

(** Kind syntax. *)
type kind = KStar | KArrow of kind * kind

let karrow k1 k2 = KArrow (k1,k2)

(** Type syntax. *)
type ('a,'b) typ =
  | TBot
  | TVar of 'a
  | TAbs of 'b * kind * ('a,'b) typ
  | TApp of ('a,'b) typ * ('a,'b) typ
  | TArrow of ('a,'b) typ * ('a,'b) typ

let tvar a = TVar a
let tabs b k t = TAbs (b,k,t)
let tapp t1 t2 = TApp (t1,t2)
let tarrow t1 t2 = TArrow (t1,t2)

(** Term syntax. *)
type ('a,'b) term =
  | Var of 'a
  | Abs of 'b * ('a,'b) typ * ('a,'b) term
  | App of ('a,'b) term * ('a,'b) term

let var a = Var a
let abs b t e = Abs (b,t,e)
let app e1 e2 = App (e1,e2)
