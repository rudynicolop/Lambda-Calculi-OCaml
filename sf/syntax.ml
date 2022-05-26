open Core
open Util
open FunUtil

(** System-F Syntax *)

(** Types. *)
type ('a,'b) typ =
  | TVar of 'a
  | TForall of 'b * ('a,'b) typ
  | TArrow of ('a,'b) typ * ('a,'b) typ

let tvar a = TVar a
let tforall b t = TForall (b,t)
let tarrow t1 t2 = TArrow (t1,t2)

(** Terms. *)
type ('a,'b) expr =
  | Var of 'a
  | Abs of 'b * ('a,'b) typ * ('a,'b) expr
  | App of ('a,'b) expr * ('a,'b) expr
  | TypAbs of 'b * ('a,'b) expr
  | TypApp of ('a,'b) expr * ('a,'b) typ

let var a = Var a
let abs b t e = Abs (b,t,e)
let app e1 e2 = App (e1,e2)
let typabs b e = TypAbs (b,e)
let typapp e t = TypApp (e,t)

let rec typ_fold
    ~ctx:(c: 'c)
    ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> 'r)
    ~g:(g: 'c -> 'b -> 'r -> 'r)
    ~h:(h: 'r -> 'r -> 'r) : ('a,'b) typ -> 'r = function
  | TVar a -> f c a
  | TForall (b,t) ->
    g c b (typ_fold ~ctx:(succ b c) ~succ:succ ~f:f ~g:g ~h:h t)
  | TArrow (t1,t2) ->
    h (typ_fold ~ctx:c ~succ:succ ~f:f ~g:g ~h:h t1)
      (typ_fold ~ctx:c ~succ:succ ~f:f ~g:g ~h:h t2)

let rec expr_fold
    ~tctx:(tc: 'c)
    ~ectx:(ec: 'c)
    ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> 'r)
    ~g:(g: 'c -> 'b -> 'r -> 'r)
    ~h:(h: 'r -> 'r -> 'r)
    ~i:(i: 'c -> 'c -> 'a -> 's)
    ~j:(j: 'c -> 'b -> 'r -> 's -> 's)
    ~k:(k: 's -> 's -> 's)
    ~l:(l: 'c -> 'b -> 's -> 's)
    ~m:(m: 's -> 'r -> 's) : ('a,'b) expr -> 's = function
  | Var a -> i tc ec a
  | Abs (b,t,e) ->
    j ec b (typ_fold ~ctx:tc ~succ:succ ~f:f ~g:g ~h:h t)
      (expr_fold ~tctx:tc ~ectx:(succ b ec) ~succ:succ
         ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
  | App (e1,e2) ->
    k (expr_fold ~tctx:tc ~ectx:ec ~succ:succ
         ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e1)
      (expr_fold ~tctx:tc ~ectx:ec ~succ:succ
         ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e2)
  | TypAbs (b,e) ->
    l tc b (expr_fold ~tctx:(succ b tc) ~ectx:ec ~succ:succ
           ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
  | TypApp (e,t) ->
    m (expr_fold ~tctx:tc ~ectx:ec ~succ:succ
         ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
      (typ_fold ~ctx:tc ~succ:succ ~f:f ~g:g ~h:h t)

let typ_scheme
    ~ctx:(c: 'c) ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> ('d,'e) typ) ~g:(g: 'c -> 'b -> 'e)
  : ('a,'b) typ -> ('d,'e) typ =
  typ_fold
    ~ctx:c ~succ:succ
    ~f:f ~g:(fun c b -> tforall $ g c b) ~h:tarrow

let typ_map_ctx
    ~ctx:(c: 'c) ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> 'd) ~g:(g: 'c -> 'b -> 'e)
  : ('a,'b) typ -> ('d,'e) typ =
  typ_scheme
    ~ctx:c ~succ:succ
    ~f:(fun c a -> tvar $ f c a) ~g:g

let typ_map
    ~f:(f: 'a -> 'c) ~g:(g: 'b -> 'd)
  : ('a,'b) typ -> ('c,'d) typ =
  typ_map_ctx
    ~ctx:() ~succ:(consume my_ignore)
    ~f:(consume f) ~g:(consume g)

let expr_scheme
    ~tctx:(tc: 'c) ~ectx:(ec: 'c) ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> ('d,'e) typ) ~g:(g: 'c -> 'b -> 'e)
    ~h:(h: 'c -> 'c -> 'a -> ('d,'e) expr) ~i:(i: 'c -> 'b -> 'e)
  : ('a,'b) expr -> ('d,'e) expr =
  expr_fold
    ~tctx:tc ~ectx:ec ~succ:succ
    ~f:f
    ~g:(fun c b -> tforall $ g c b)
    ~h:tarrow
    ~i:h
    ~j:(fun c b -> abs $ i c b)
    ~k:app
    ~l:(fun c b -> typabs $ g c b)
    ~m:typapp

let expr_map_ctx
    ~tctx:(tc: 'c) ~ectx:(ec: 'c) ~succ:(succ: 'b -> 'c -> 'c)
    ~f:(f: 'c -> 'a -> 'd) ~g:(g: 'c -> 'b -> 'e)
    ~h:(h: 'c -> 'c -> 'a -> 'd) ~i:(i: 'c -> 'b -> 'e)
  : ('a,'b) expr -> ('d,'e) expr =
  expr_scheme
    ~tctx:tc ~ectx:ec ~succ:succ
    ~f:(fun c a -> tvar $ f c a) ~g:g
    ~h:(fun tc ec a -> var $ h tc ec a) ~i:i

let expr_map
    ~f:(f: 'a -> 'c) ~g:(g: 'b -> 'd)
    ~h:(h: 'a -> 'c) ~i:(i: 'b -> 'd)
  : ('a,'b) expr -> ('a,'b) expr =
  expr_map_ctx
    ~tctx:() ~ectx:() ~succ:(consume my_ignore)
    ~f:(consume f) ~g:(consume g)
    ~h:(consume $ consume h) ~i:(consume i)

let string_of_typ
    (f: 'a -> string) (g: 'b -> string) : ('a,'b) typ -> string =
  typ_fold
    ~ctx:() ~succ:(consume my_ignore)
    ~f:(consume f)
    ~g:(fun _ b t -> "(∀" ^ g b ^ "." ^ t ^ ")")
    ~h:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")

let string_of_expr
    (f : 'a -> string) (g : 'b -> string) : ('a,'b) expr -> string =
  expr_fold
    ~tctx:() ~ectx:() ~succ:(consume my_ignore)
    ~f:(consume f)
    ~g:(fun _ b t -> "(∀" ^ g b ^ "." ^ t ^ ")")
    ~h:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")
    ~i:(consume $ consume f)
    ~j:(fun _ b t e -> "(λ" ^ g b ^ ":" ^ t ^ "." ^ e ^ ")")
    ~k:(fun e1 e2 -> "(" ^ e1 ^ " " ^ e2 ^ ")")
    ~l:(fun _ b e -> "(Λ" ^ g b ^ "." ^ e ^ ")")
    ~m:(fun e t -> "(" ^ e ^ " [" ^ t ^ "])")

(** Parsed syntax. *)
type p_typ = (string,string) typ
type p_expr = (string,string) expr

let string_of_p_typ : p_typ -> string = string_of_typ id id

let string_of_p_expr : p_expr -> string = string_of_expr id id

(** De Bruijn syntax. *)
type b_typ = (int,unit) typ
type b_expr = (int,unit) expr

let string_of_b_typ : b_typ -> string =
  string_of_typ string_of_int (fun _ -> "")

let string_of_b_expr : b_expr -> string =
  string_of_expr string_of_int (fun _ -> "")

let rec (=?) (t1: b_typ) (t2: b_typ) : bool =
  match t1, t2 with
  | TVar n1, TVar n2 -> n1 = n2
  | TForall (_,t1), TForall (_,t2) -> t1 =? t2
  | TArrow (t11,t12), TArrow (t21, t22) ->
    t11 =? t21 && t12 =? t22
  | _, _ -> false

let b_of_p_typ
    (stk: string list) : p_typ -> b_typ =
  typ_map_ctx
    ~ctx:stk ~succ:List.cons
    ~f:(fun c x ->
        ListUtil.index_of ~eq:String.(=) x c
        |> Option.value_map
          ~default:(List.length c)
          ~f:id)
    ~g:(consume my_ignore)

let b_of_p_expr
    (tstk: string list) (estk: string list)
  : p_expr -> b_expr =
  expr_map_ctx
    ~tctx:tstk ~ectx:estk ~succ:List.cons
    ~f:(fun o a ->
        ListUtil.index_of ~eq:String.(=) a o
        |> Option.value_map
          ~default:(List.length o)
          ~f:id)
    ~g:(consume my_ignore)
    ~h:(fun _ c x ->
        ListUtil.index_of ~eq:String.(=) x c
        |> Option.value_map
          ~default:(List.length c)
          ~f:id)
    ~i:(consume my_ignore)

let p_of_b_typ
    (depth: int) : b_typ -> p_typ =
  typ_map_ctx
    ~ctx:depth ~succ:(consume $ (+) 1)
    ~f:(fun d n -> "T" ^ (string_of_int $ d - n))
    ~g:(fun d _ -> "T" ^ (string_of_int $ d + 1))

let p_of_b_expr
    (td: int) (ed: int) : b_expr -> p_expr =
  expr_map_ctx
    ~tctx:td ~ectx:ed
    ~succ:(consume $ (+) 1)
    ~f:(fun d n -> "T" ^ (string_of_int $ d - n))
    ~g:(fun d _ -> "T" ^ (string_of_int $ d + 1))
    ~h:(fun _ ed n -> "x" ^ (string_of_int $ ed - n))
    ~i:(fun ed _ -> "x" ^ (string_of_int $ ed + 1))
