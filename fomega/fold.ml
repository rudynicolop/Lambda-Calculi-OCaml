open Util
open FunUtil
open Syntax

(** Folding. *)

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

(** Variable substitution schemes. *)

let typ_scheme
    ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~var:(v: 'o -> 'a -> ('c,'d) typ)
    ~all:(al: 'o -> 'b -> 'd)
    ~abs:(ab: 'o -> 'b -> 'd) : ('a,'b) typ -> ('c,'d) typ =
  fold_typ
    ~ctx:o ~f:f ~var:v
    ~all:(fun o b -> tforall $ al o b)
    ~abs:(fun o b -> tabs $ ab o b)
    ~arr:tarrow ~app:tapp

let term_scheme
    ~tctx:(tyo: 'o) ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~ty:(ty: 'o -> ('a,'b) typ -> ('c,'d) typ)
    ~var:(v: 'o -> 'a -> ('c,'d) term)
    ~abs:(ab: 'o -> 'b -> 'd)
    ~tabs:(tab: 'o -> 'b -> 'd) : ('a,'b) term -> ('c,'d) term =
  fold_term
    ~tctx:tyo ~ctx:o ~f:f
    ~var:v ~app:app
    ~abs:(fun o b t -> abs (ab o b) $ ty tyo t)
    ~tabs:(fun o b -> typabs $ (tab o b))
    ~tapp:(fun e t -> typapp e $ ty tyo t)

(** Structure-preserving map under context. *)

let map_typ_ctx
    ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~var:(v: 'o -> 'a -> 'c)
    ~all:(al: 'o -> 'b -> 'd)
    ~abs:(ab: 'o -> 'b -> 'd) : ('a,'b) typ -> ('c,'d) typ =
  typ_scheme
    ~ctx:o ~f:f ~all:al ~abs:ab
    ~var:(fun o -> tvar >> v o)

let map_term_ctx
    ~tctx:(tyo: 'o) ~ctx:(o: 'o) ~f:(f: 'b -> 'o -> 'o)
    ~ty:(ty: 'o -> ('a,'b) typ -> ('c,'d) typ)
    ~var:(v: 'o -> 'a -> 'c)
    ~abs:(ab: 'o -> 'b -> 'd)
    ~tabs:(tab: 'o -> 'b -> 'd) : ('a,'b) term -> ('c,'d) term =
  term_scheme
    ~tctx:tyo ~ctx:o ~f:f ~ty:ty ~abs:ab ~tabs:tab
    ~var:(fun o -> var >> v o)
