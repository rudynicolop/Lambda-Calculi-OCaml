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
    ~star:(s: 'm)
    ~karrow:(ka: 'm -> 'm -> 'm)
    ~bot:(bt: 'c -> 'r)
    ~var:(v: 'c -> 'a -> 'r)
    ~abs:(ab: 'c -> 'b -> 'm -> 'r -> 'r)
    ~app:(ap: 'r -> 'r -> 'r)
    ~arrow:(ta: 'r -> 'r -> 'r)
  : ('a,'b) typ -> 'r = function
  | TBot -> bt c
  | TVar a -> v c a
  | TAbs (b,k,t) ->
    ab c b (fold_kind ~star:s ~arrow:ka k)
    $ fold_typ ~star:s ~karrow:ka ~ctx:(sc b c)
      ~succ:sc ~bot:bt ~var:v ~abs:ab
      ~app:ap ~arrow:ta t
  | TApp (t1,t2) ->
    ap
      (fold_typ ~star:s ~karrow:ka ~ctx:c
         ~succ:sc ~bot:bt ~var:v ~abs:ab
         ~app:ap ~arrow:ta t1)
    $ fold_typ ~star:s ~karrow:ka ~ctx:c
         ~succ:sc ~bot:bt ~var:v ~abs:ab
         ~app:ap ~arrow:ta t2
  | TArrow (t1,t2) ->
    ta
      (fold_typ ~star:s ~karrow:ka ~ctx:c
         ~succ:sc ~bot:bt ~var:v ~abs:ab
         ~app:ap ~arrow:ta t1)
    $ fold_typ ~star:s ~karrow:ka ~ctx:c
         ~succ:sc ~bot:bt ~var:v ~abs:ab
         ~app:ap ~arrow:ta t2

let rec fold_term
    ~tctx:(tc: 'c)
    ~ectx:(ec: 'c)
    ~succ:(sc: 'b -> 'c -> 'c)
    ~star:(s: 'm)
    ~karrow:(ka: 'm -> 'm -> 'm)
    ~bot:(bt: 'c -> 'r)
    ~tvar:(tv: 'c -> 'a -> 'r)
    ~tabs:(tab: 'c -> 'b -> 'm -> 'r -> 'r)
    ~tapp:(tap: 'r -> 'r -> 'r)
    ~arrow:(ta: 'r -> 'r -> 'r)
    ~var:(v: 'c -> 'a -> 's)
    ~abs:(ab: 'c -> 'b -> 'r -> 's -> 's)
    ~app:(ap: 's -> 's -> 's)
  : ('a,'b) term -> 's = function
  | Var a -> v ec a
  | Abs (b,t,e) ->
    ab ec b
      (fold_typ ~ctx:tc ~succ:sc ~star:s ~karrow:ka
         ~bot:bt ~var:tv ~abs:tab ~app:tap ~arrow:ta t)
    $ fold_term
      ~tctx:tc ~ectx:(sc b ec) ~succ:sc
      ~star:s ~karrow:ka
      ~bot:bt ~tvar:tv ~tabs:tab ~tapp:tap ~arrow:ta
      ~var:v ~abs:ab ~app:ap e
  | App (e1,e2) ->
    ap
      (fold_term
         ~tctx:tc ~ectx:ec ~succ:sc
         ~star:s ~karrow:ka
         ~bot:bt ~tvar:tv ~tabs:tab ~tapp:tap ~arrow:ta
         ~var:v ~abs:ab ~app:ap e1)
      (fold_term
         ~tctx:tc ~ectx:ec ~succ:sc
         ~star:s ~karrow:ka
         ~bot:bt ~tvar:tv ~tabs:tab ~tapp:tap ~arrow:ta
         ~var:v ~abs:ab ~app:ap e2)      

(** Schemes for substitution. *)

let typ_scheme
    ~ctx:(c: 'c) ~succ:(sc: 'b -> 'c -> 'c)
    ~var:(v: 'c -> 'a -> ('d,'e) typ)
    ~abs:(ab: 'c -> 'b -> 'e) =
  fold_typ
    ~ctx:c ~succ:sc
    ~star:KStar ~karrow:(karrow)
    ~bot:(consume TBot)
    ~var:v
    ~abs:(fun c b -> tabs $ ab c b)
    ~app:(tapp) ~arrow:(tarrow)

(*let term_scheme
    ~tctx:(tc: 'c)
    ~ectx:(ec: 'c)
    ~succ:(sc: 'b -> 'c -> 'c)
*)  
