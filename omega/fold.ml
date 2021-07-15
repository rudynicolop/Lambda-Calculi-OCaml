open Core
open Util
open FunUtil
open Syntax

(** Context-dependent folds. *)

let rec fold_kind
    ~star:(s: 'm)
    ~arrow:(f: 'm -> 'm -> 'm)
  : kind -> 'a = function
  | KStar -> s
  | KArrow (k1,k2) ->
    f (fold_kind ~star:s ~arrow:f k1) $ fold_kind ~star:s ~arrow:f k2

let rec fold_typ
    ~ctx:(c: 'c)
    ~succ:(sc: 'b -> 'c -> 'c)
    ~bot:(bt: 'c -> 'r)
    ~var:(v: 'c -> 'a -> 'r)
    ~abs:(ab: 'c -> 'b -> kind -> 'r -> 'r)
    ~app:(ap: 'r -> 'r -> 'r)
    ~arrow:(ta: 'r -> 'r -> 'r)
  : ('a,'b) typ -> 'r = function
  | TBot -> bt c
  | TVar a -> v c a
  | TAbs (b,k,t) ->
    ab c b k
    $ fold_typ
      ~ctx:(sc b c) ~succ:sc
      ~bot:bt ~var:v ~abs:ab ~app:ap ~arrow:ta t
  | TApp (t1,t2) ->
    ap
      (fold_typ
         ~ctx:c ~succ:sc
         ~bot:bt ~var:v ~abs:ab ~app:ap ~arrow:ta t1)
    $ fold_typ
      ~ctx:c ~succ:sc
      ~bot:bt ~var:v ~abs:ab ~app:ap ~arrow:ta t2
  | TArrow (t1,t2) ->
    ta
      (fold_typ
         ~ctx:c ~succ:sc
         ~bot:bt ~var:v ~abs:ab ~app:ap ~arrow:ta t1)
    $ fold_typ
      ~ctx:c ~succ:sc
      ~bot:bt ~var:v ~abs:ab ~app:ap ~arrow:ta t2

let rec fold_term
    ~ctx:(c: 'c)
    ~succ:(sc: 'b -> 'c -> 'c)
    ~var:(v: 'c -> 'a -> 's)
    ~abs:(ab: 'c -> 'b -> ('a,'b) typ -> 's -> 's)
    ~app:(ap: 's -> 's -> 's)
  : ('a,'b) term -> 's = function
  | Var a -> v c a
  | Abs (b,t,e) ->
    ab c b t
    $ fold_term
      ~ctx:(sc b c) ~succ:sc
      ~var:v ~abs:ab ~app:ap e
  | App (e1,e2) ->
    ap
      (fold_term
         ~ctx:c ~succ:sc
         ~var:v ~abs:ab ~app:ap e1)
      (fold_term
         ~ctx:c ~succ:sc
         ~var:v ~abs:ab ~app:ap e2)      

(** Schemes for substitution. *)

let typ_scheme
    ~ctx:(c: 'c) ~succ:(sc: 'b -> 'c -> 'c)
    ~var:(v: 'c -> 'a -> ('d,'e) typ)
    ~abs:(ab: 'c -> 'b -> 'e)
  : ('a,'b) typ -> ('d,'e) typ =
  fold_typ
    ~ctx:c ~succ:sc
    ~bot:(consume TBot) ~var:v
    ~abs:(fun c b -> tabs $ ab c b)
    ~app:(tapp) ~arrow:(tarrow)

let term_scheme
    ~ctx:(c: 'c) ~succ:(sc: 'b -> 'c -> 'c)
    ~ty:(ty: ('a,'b) typ -> ('d,'e) typ)
    ~var:(v: 'c -> 'a -> ('d,'e) term)
    ~abs:(ab: 'c -> 'b -> 'e)
  : ('a,'b) term -> ('d,'e) term =
  fold_term
    ~ctx:c ~succ:sc ~var:v ~app:app
    ~abs:(fun c b t -> abs (ab c b) $ ty t)

(** Context-dependent mapping. *)

let map_typ_ctx
    ~ctx:(c: 'c) ~succ:(sc: 'b -> 'c -> 'c)
    ~var:(v: 'c -> 'a -> 'd) ~abs:(ab: 'c -> 'b -> 'e)
  : ('a,'b) typ -> ('d,'e) typ =
  typ_scheme
    ~ctx:c ~succ:sc ~abs:ab
    ~var:(fun c a -> tvar $ v c a)

let map_term_ctx
    ~ctx:(c: 'c) ~succ:(sc: 'b -> 'c -> 'c)
    ~ty:(ty: ('a,'b) typ -> ('d,'e) typ)
    ~var:(v: 'c -> 'a -> 'd) ~abs:(ab: 'c -> 'b -> 'e)
  : ('a,'b) term -> ('d,'e) term =
  term_scheme
    ~ctx:c ~succ:sc ~ty:ty ~abs:ab
    ~var:(fun c a -> var $ v c a)

(** Context-free mapping. *)

let map_typ
    ~var:(v: 'a -> 'c) ~abs:(ab: 'b -> 'd)
  : ('a,'b) typ -> ('c,'d) typ =
  map_typ_ctx
    ~ctx:() ~succ:(consume my_ignore)
    ~var:(consume v) ~abs:(consume ab)

let map_term
    ~ty:(ty: ('a,'b) typ -> ('c,'d) typ)
    ~var:(v: 'a -> 'c) ~abs:(ab: 'b -> 'd)
  : ('a,'b) term -> ('c,'d) term =
  map_term_ctx
    ~ctx:() ~succ:(consume my_ignore)
    ~ty:ty ~var:(consume v) ~abs:(consume ab)

(** String of kinds, types, & terms. *)

let string_of_kind : kind -> string =
  fold_kind
    ~star:"*" ~arrow:(fun k1 k2 -> "(" ^ k1 ^ "⇒" ^ k2 ^ ")")

let string_of_typ
    (fa: 'a -> string) (fb: 'b -> string)
  : ('a,'b) typ -> string =
  fold_typ
    ~ctx:() ~succ:(consume my_ignore)
    ~bot:(consume "⊥") ~var:(consume fa)
    ~abs:(fun _ b k t ->
        "(λ" ^ fb b ^ "::" ^ string_of_kind k ^ "." ^ t ^ ")")
    ~app:(fun t1 t2 -> "(" ^ t1 ^ " " ^ t2 ^ ")")
    ~arrow:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")

let string_of_term
    (fa: 'a -> string) (fb: 'b -> string)
  : ('a,'b) term -> string =
  fold_term
    ~ctx:() ~succ:(consume my_ignore)
    ~var:(consume fa)
    ~abs:(fun _ b t e ->
        "(λ" ^ fb b ^ ":" ^ string_of_typ fa fb t ^ "." ^ e ^ ")")
    ~app:(fun e1 e2 -> "(" ^ e1 ^ " " ^ e2 ^ ")")
