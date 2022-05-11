open Core

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

(** Parsed types. *)
type p_typ = (string,string) typ

(** De Bruijn types. *)
type b_typ = (int,unit) typ

(** Term syntax. *)
type ('a,'b) term =
  | Var of 'a
  | Abs of 'b * ('a,'b) typ * ('a,'b) term
  | App of ('a,'b) term * ('a,'b) term

let var a = Var a
let abs b t e = Abs (b,t,e)
let app e1 e2 = App (e1,e2)

(** Parsed terms. *)
type p_term = (string,string) term

(** De Bruijn terms. *)
type b_term = (int,unit) term

(** Kind equality. *)
let rec (=?) (k1: kind) (k2: kind) : bool =
  match k1, k2 with
  | KStar, KStar -> true
  | KArrow (k11,k12), KArrow (k21,k22) -> k11 =? k21 && k12 =? k22
  | _, _ -> false

(** Type equality of De Bruijn types. *)
    (*
let rec (==) (t1: b_typ) (t2: b_typ) : bool =
  match t1, t2 with
  | TBot, TBot -> true
  | TVar n1, TVar n2 -> n1 = n2
  | TAbs (_,k1,t1), TAbs (_,k2,t2) -> k1 =? k2 && t1 == t2
  | TArrow (t11,t12), TArrow (t21,t22)
  | TApp (t11,t12), TApp (t21,t22) -> t11 == t21 && t12 == t22
  | _, _ -> false
*)
