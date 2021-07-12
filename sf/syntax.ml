open Core
open Util
open FunUtil

(** System-F Syntax *)

(** Types. *)
type ('a,'b) typ =
  | TVar of 'a
  | TForall of 'b * ('a,'b) typ
  | TArrow of ('a,'b) typ * ('a,'b) typ

(** Terms. *)
type ('a,'b) expr =
  | Var of 'a
  | Abs of 'b * ('a,'b) typ * ('a,'b) expr
  | App of ('a,'b) expr * ('a,'b) expr
  | TypAbs of 'b * ('a,'b) expr
  | TypApp of ('a,'b) expr * ('a,'b) typ

let rec typ_map
    ~fa:(fa: 'a -> 'c)
    ~fb:(fb: 'b -> 'd) : ('a,'b) typ -> ('c,'d) typ = function
  | TVar a -> TVar (fa a)
  | TForall (b,t) -> TForall (fb b, typ_map ~fa:fa ~fb:fb t)
  | TArrow (t1,t2) ->
    TArrow (typ_map ~fa:fa ~fb:fb t1, typ_map ~fa:fa ~fb:fb t2)

let rec expr_map
    ~fa:(fa: 'a -> 'c)
    ~fb:(fb: 'b -> 'd) : ('a,'b) expr -> ('c,'d) expr = function
  | Var a -> Var (fa a)
  | Abs (b,t,e) ->
    Abs (fb b, typ_map ~fa:fa ~fb:fb t, expr_map ~fa:fa ~fb:fb e)
  | App (e1,e2) ->
    App (expr_map ~fa:fa ~fb:fb e1, expr_map ~fa:fa ~fb:fb e2)
  | TypAbs (a,e) ->
    TypAbs (fa a, expr_map ~fa:fa ~fb:fb e)
  | TypApp (e,t) ->
    TypApp (expr_map ~fa:fa ~fb:fb e, typ_map ~fa:fa ~fb:fb t)

let rec typ_fold
    ~f:(f: 'a -> 'r)
    ~g:(g: 'b -> 'r -> 'r)
    ~h:(h: 'r -> 'r -> 'r) : ('a,'b) typ -> 'r = function
  | TVar a -> f a
  | TForall (b,t) -> g b (typ_fold ~f:f ~g:g ~h:h t)
  | TArrow (t1,t2) ->
    h (typ_fold ~f:f ~g:g ~h:h t1) (typ_fold ~f:f ~g:g ~h:h t2)

let rec expr_fold
    ~f:(f: 'a -> 'r)
    ~g:(g: 'b -> 'r -> 'r)
    ~h:(h: 'r -> 'r -> 'r)
    ~i:(i: 'a -> 's)
    ~j:(j: 'b -> 'r -> 's -> 's)
    ~k:(k: 's -> 's -> 's)
    ~l:(l: 'b -> 's -> 's)
    ~m:(m: 's -> 'r -> 's) : ('a,'b) expr -> 's = function
  | Var a -> i a
  | Abs (b,t,e) ->
    j b (typ_fold ~f:f ~g:g ~h:h t)
      (expr_fold ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
  | App (e1,e2) ->
    k (expr_fold ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e1)
      (expr_fold ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e2)
  | TypAbs (b,e) ->
    l b (expr_fold ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
  | TypApp (e,t) ->
    m (expr_fold ~f:f ~g:g ~h:h ~i:i ~j:j ~k:k ~l:l ~m:m e)
      (typ_fold ~f:f ~g:g ~h:h t)

let typ_map' ~f:(f: 'a -> 'c) ~g:(g: 'b -> 'd) : ('a,'b) typ -> ('c,'d) typ =
  typ_fold
    ~f:(fun a -> TVar (f a))
    ~g:(fun b t -> TForall (g b, t))
    ~h:(fun t1 t2 -> TArrow (t1, t2))

let expr_map' ~f:(f: 'a -> 'c) ~g:(g: 'b -> 'd) : ('a,'b) expr -> ('c,'d) expr =
  expr_fold
    ~f:(fun a -> TVar (f a))
    ~g:(fun b e -> TForall (g b,e))
    ~h:(fun t1 t2 -> TArrow (t1,t2))
    ~i:(fun a -> Var (f a))
    ~j:(fun b t e -> Abs (g b,t,e))
    ~k:(fun e1 e2 -> App (e1,e2))
    ~l:(fun b e -> TypAbs (g b, e))
    ~m:(fun t e -> TypApp (t,e))

let string_of_typ'
    (f: 'a -> string) (g: 'b -> string) : ('a,'b) typ -> string =
  typ_fold
    ~f:f
    ~g:(fun b t -> "(∀" ^ g b ^ "." ^ t ^ ")")
    ~h:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")

let string_of_expr'
    (f : 'a -> string) (g : 'b -> string) : ('a,'b) expr -> string =
  expr_fold
    ~f:f
    ~g:(fun b t -> "(∀" ^ g b ^ "." ^ t ^ ")")
    ~h:(fun t1 t2 -> "(" ^ t1 ^ "→" ^ t2 ^ ")")
    ~i:f
    ~j:(fun b t e -> "(λ" ^ g b ^ ":" ^ t ^ "." ^ e ^ ")")
    ~k:(fun e1 e2 -> "(" ^ e1 ^ " " ^ e2 ^ ")")
    ~l:(fun b e -> "(" ^ "Λ" ^ g b ^ "." ^ e ^ ")")
    ~m:(fun e t -> "(" ^ e ^ " [" ^ t ^ "])")

let rec string_of_typ
  (fa: 'a -> string) (fb: 'b -> string) : ('a,'b) typ -> string = function
  | TVar a -> fa a
  | TForall (b,t) ->
    "(∀" ^ fb b ^ "." ^ string_of_typ fa fb t ^ ")"
  | TArrow (t1,t2) ->
    "(" ^ string_of_typ fa fb t1 ^ "→" ^ string_of_typ fa fb t2 ^ ")"

let rec string_of_expr
    (fa: 'a -> string) (fb: 'b -> string) : ('a,'b) expr -> string = function
  | Var a -> fa a
  | Abs (b,t,e) ->
    "(λ" ^ fb b ^ ":" ^ string_of_typ fa fb t ^
    "." ^ string_of_expr fa fb e ^ ")"
  | App (e1,e2) ->
    "(" ^ string_of_expr fa fb e1 ^
    " " ^ string_of_expr fa fb e2 ^ ")"
  | TypAbs (b,e) ->
    "(" ^ "Λ" ^ fb b ^ "." ^ string_of_expr fa fb e ^ ")"
  | TypApp (e,t) ->
    "(" ^ string_of_expr fa fb e ^
    " [" ^ string_of_typ fa fb t ^ "])"

(** Parsed syntax. *)
type p_typ = (string,string) typ
type p_expr = (string,string) expr

let string_of_p_typ : p_typ -> string =
  string_of_typ' (fun s -> s) (fun s -> s)

let string_of_p_expr : p_expr -> string =
  string_of_expr' (fun s -> s) (fun s -> s)

(** De Bruijn syntax. *)
type b_typ = (int,unit) typ
type b_expr = (int,unit) expr

let string_b_typ : b_typ -> string =
  string_of_typ' string_of_int (fun _ -> "")

let string_b_expr : b_expr -> string =
  string_of_expr' string_of_int (fun _ -> "")

let rec (=?) (t1: b_typ) (t2: b_typ) : bool =
  match t1, t2 with
  | TVar n1, TVar n2 -> n1 = n2
  | TForall (_,t1), TForall (_,t2) -> t1 =? t2
  | TArrow (t11,t12), TArrow (t21, t22) ->
    t11 =? t21 && t12 =? t22
  | _, _ -> false

let rec b_typ_of_p_typ
    (stk: string list) : p_typ -> b_typ = function
  | TVar x ->
    TVar
    begin match ListUtil.index_of String.equal x stk with
      | Some n -> n
      | None -> List.length stk + 1
    end
  | TForall (x,t) ->
    TForall ((), b_typ_of_p_typ (x :: stk) t)
  | TArrow (t1, t2) ->
    TArrow (b_typ_of_p_typ stk t1, b_typ_of_p_typ stk t2)

let rec b_expr_of_p_expr
    (tstk: string list)
    (estk: string list) : p_expr -> b_expr = function
  | Var x ->
    Var
    begin match ListUtil.index_of String.equal x estk with
      | Some n -> n
      | None -> List.length estk + 1
    end
  | Abs (x,t,e) ->
    Abs ((), b_typ_of_p_typ tstk t,
         b_expr_of_p_expr tstk (x :: estk) e)
  | App (e1, e2) ->
    App (b_expr_of_p_expr tstk estk e1,
         b_expr_of_p_expr tstk estk e2)
  | TypAbs (x,e) ->
    TypAbs ((), b_expr_of_p_expr (x :: tstk) estk e)
  | TypApp (e,t) ->
    TypApp (b_expr_of_p_expr tstk estk e,
            b_typ_of_p_typ tstk t)

let rec p_typ_of_b_typ
    (depth: int) : b_typ -> p_typ = function
  | TVar n ->
    TVar ("T" ^ (string_of_int $ depth - n))
  | TForall (_,t) ->
    TForall ("T" ^ (string_of_int $ depth + 1),
             p_typ_of_b_typ (depth + 1) t)
  | TArrow (t1,t2) ->
    TArrow (p_typ_of_b_typ depth t1,
            p_typ_of_b_typ depth t2)

let rec p_expr_of_b_expr
    (td: int) (ed: int) : b_expr -> p_expr = function
  | Var n ->
    Var ("x" ^ (string_of_int $ ed - n))
  | Abs (_,t,e) ->
    Abs ("x" ^ (string_of_int $ ed + 1),
         p_typ_of_b_typ td t,
         p_expr_of_b_expr td (ed + 1) e)
  | App (e1,e2) ->
    App (p_expr_of_b_expr td ed e1,
         p_expr_of_b_expr td ed e2)
  | TypAbs (_,e) ->
    TypAbs ("T" ^ (string_of_int $ td + 1),
            p_expr_of_b_expr (td + 1) ed e)
  | TypApp (e,t) ->
    TypApp (p_expr_of_b_expr td ed e,
            p_typ_of_b_typ td t)
