(** Syntax. *)

(** Kind syntax. *)
type kind =
  | KStar
  | KArrow of kind * kind

(** Type syntax. *)
type ('a,'b) typ =
  | TVar of 'a
  | TArrow of ('a,'b) typ * ('a,'b) typ
  | TForall of 'b * kind * ('a,'b) typ
  | TAbs of 'b * kind * ('a,'b) typ
  | TApp of ('a,'b) typ * ('a,'b) typ

(** Term syntax. *)
type ('a,'b) term =
  | Var of 'a
  | Abs of 'b * ('a,'b) typ * ('a,'b) term
  | App of ('a,'b) term * ('a,'b) term
  | TypAbs of 'b * kind * ('a,'b) term
  | TypApp of ('a,'b) term * ('a,'b) typ
