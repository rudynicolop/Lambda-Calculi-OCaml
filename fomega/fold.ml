open Core
open Util
open FunUtil
open Syntax

(** Folding. *)

let rec fold_kind
    ~star:(s: 'm)
    ~arr:(f: 'm -> 'm -> 'm)
  : kind -> 'm = function
  | KStar -> s
  | KArrow (k1,k2) ->
    f (fold_kind ~star:s ~arr:f k1) $ fold_kind ~star:s ~arr:f k2

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

(** Conversion between representations. *)

let b_of_p_typ (o: string list) : p_typ -> b_typ =
  map_typ_ctx
    ~ctx:o ~f:List.cons
    ~var:(switch $ ListUtil.index_of_default String.(=))
    ~all:(consume my_ignore) ~abs:(consume my_ignore)

let b_of_p_term (tyo: string list) (o: string list) : p_term -> b_term =
  map_term_ctx
    ~tctx:tyo ~ctx:o ~f:List.cons ~ty:b_of_p_typ
    ~var:(switch $ ListUtil.index_of_default String.(=))
    ~abs:(consume my_ignore) ~tabs:(consume my_ignore)

let p_of_b_typ (d: int) : b_typ -> p_typ =
  map_typ_ctx
    ~ctx:d ~f:(consume $ (+) 1)
    ~var:(fun d n -> "T" ^ (string_of_int $ d - n))
    ~all:(fun d _ -> "T" ^ (string_of_int $ d + 1))
    ~abs:(fun d _ -> "T" ^ (string_of_int $ d + 1))

let p_of_term (td: int) (d: int) : b_term -> p_term =
  map_term_ctx
    ~tctx:td ~ctx:d ~f:(consume $ (+) 1) ~ty:p_of_b_typ
    ~var:(fun d n -> "T" ^ (string_of_int $ d - n))
    ~abs:(fun d _ -> "T" ^ (string_of_int $ d + 1))
    ~tabs:(fun d _ -> "T" ^ (string_of_int $ d + 1))

(** [string_of] functions. *)

let string_of_kind : kind -> string =
  fold_kind ~star:"*"
    ~arr:(fun k1 k2 -> "(" ^ k1 ^ "⇒" ^ k2 ^ ")")

let string_of_typ
  (fa: 'a -> string) (fb: 'b -> string)
  : ('a,'b) typ -> string =
  fold_typ
    ~ctx:() ~f:(consume my_ignore) ~var:(consume fa)
    ~arr:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")
    ~app:(fun t1 t2 -> "(" ^ t1 ^ " " ^ t2 ^ ")")
    ~all:(fun _ b k t ->
        "(∀" ^ fb b ^ "::" ^ string_of_kind k ^ "." ^ t ^ ")")
    ~abs:(fun _ b k t ->
        "(λ" ^ fb b ^ "::" ^ string_of_kind k ^ "." ^ t ^ ")")

let string_of_term
    (fa: 'a -> string) (fb: 'b -> string)
  : ('a,'b) term -> string =
  fold_term
    ~tctx:() ~ctx:() ~f:(consume my_ignore)
    ~var:(consume fa)
    ~abs:(fun _ b t e ->
        "(λ" ^ fb b ^ ":" ^ string_of_typ fa fb t ^ "." ^ e ^ ")")
    ~app:(fun t1 t2 -> "(" ^ t1 ^ " " ^ t2 ^ ")")
    ~tabs:(fun _ b k e ->
        "(Λ" ^ fb b ^ "::" ^ string_of_kind k ^ "." ^ e ^ ")")
    ~tapp:(fun e t -> "(" ^ e ^ " [" ^ string_of_typ fa fb t ^ "])")

let string_of_p_typ : p_typ -> string =
  string_of_typ id id
    
let string_of_p_term : p_term -> string =
  string_of_term id id

let string_of_b_typ : b_typ -> string =
  string_of_typ string_of_int $ consume ""

let string_of_b_term : b_term -> string =
  string_of_term string_of_int $ consume ""
