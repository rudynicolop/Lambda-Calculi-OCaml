open Util
open FunUtil
open Syntax

let rec fold_kind
    ~star:(s: 'm)
    ~arrow:(f: 'm -> 'm -> 'm)
  : kind -> 'a = function
  | KStar -> s
  | KArrow (k1,k2) ->
    f (fold_kind ~star:s ~arrow:f k1) $ fold_kind ~star:s ~arrow:f k2

let rec fold_typ
    ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~var:(v: 'o -> 'a -> 'r)
    ~arr:(ar: 'r -> 'r -> 'r)
    ~all:(al: 'o -> 'b -> kind -> 'r -> 'r)
    ~abs:(ab: 'o -> 'b -> kind -> 'r -> 'r)
    ~app:(ap: 'r -> 'r -> 'r) : ('a,'b) typ -> 'r = function
  | TVar a -> v o a
  | TArrow (t1,t2) ->
    ar (fold_typ ~ctx:o ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t1)
    $ fold_typ ~ctx:o ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t2
  | TForall (b,k,t) ->
    al o b k
    $ fold_typ ~ctx:(f b o) ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t
  | TAbs (b,k,t) ->
    ab o b k
    $ fold_typ ~ctx:(f b o) ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t
  | TApp (t1,t2) ->
    ap (fold_typ ~ctx:o ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t1)
    $ fold_typ ~ctx:o ~f:f ~var:v ~arr:ar ~all:al ~abs:ab ~app:ap t2

let rec fold_term
    ~tctx:(tyo: 'o) ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~var:(v: 'o -> 'a -> 's)
    ~abs:(ab: 'o -> 'b -> ('a,'b) typ -> 's -> 's)
    ~app:(ap: 's -> 's -> 's)
    ~tabs:(tab: 'o -> 'b -> kind -> 's -> 's)
    ~tapp:(tap: 's -> ('a,'b) typ -> 's) : ('a,'b) term -> 's = function
  | Var a -> v o a
  | Abs (b,t,e) ->
    ab o b t $ fold_term
      ~tctx:tyo ~ctx:(f b o) ~f:f
      ~var:v ~abs:ab ~app:ap ~tabs:tab ~tapp:tap e
  | App (e1,e2) ->
    ap (fold_term ~tctx:tyo ~ctx:o ~f:f
          ~var:v ~abs:ab ~app:ap ~tabs:tab ~tapp:tap e1)
    $ fold_term ~tctx:tyo ~ctx:o ~f:f
      ~var:v ~abs:ab ~app:ap ~tabs:tab ~tapp:tap e2
  | TypAbs (b,k,e) ->
    tab o b k $ fold_term
      ~tctx:(f b tyo) ~ctx:o ~f:f
      ~var:v ~abs:ab ~app:ap ~tabs:tab ~tapp:tap e
  | TypApp (e,t) ->
    tap
      (fold_term ~tctx:tyo ~ctx:o ~f:f
         ~var:v ~abs:ab ~app:ap ~tabs:tab ~tapp:tap e) t
