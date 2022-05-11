open Core
open Option
open Syntax
open Util
open FunUtil
open CompUtil

(** Dynamic Semantics. *)

let rec rename (r : int -> int) : b_expr -> b_expr =
  function
  | Var x -> Var (r x)
  | Abs (_,t,e) -> Abs ((), t, rename (ext r) e)
  | App (e1,e2) -> App (rename r e1, rename r e2)

let exts (s : int -> b_expr) (x : int) : b_expr =
  if x < 1 then Var x else rename ((+) 1) $ s (x - 1)

let rec subs (s : int -> b_expr) : b_expr -> b_expr =
  function
  | Var x -> s x
  | Abs (_,t,e) -> Abs ((), t, subs (exts s) e)
  | App (e1,e2) -> App (subs s e1, subs s e2)

let sub_helper (e : b_expr) (x : int) : b_expr =
  if x = 0 then e else Var (x - 1)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : b_expr) : b_expr -> b_expr =
  subs $ sub_helper arg

(** Call-by-value *)
let rec cbv : b_expr -> b_expr option = function
  | App (Abs (_,_,e1), (Abs _ | Var _ as e2)) ->
    some $ sub ~arg:e2 e1
  | App (e1, e2) ->
    begin match cbv e1 with
    | None -> cbv e2 >>| fun e2' -> App (e1, e2')
    | Some e1' -> some $ App (e1', e2)
    end
  | _ -> None

(** Call-by-name *)
let rec cbn : b_expr -> b_expr option = function
  | App (Abs (_,_,e1), e2) -> some $ sub ~arg:e2 e1
  | App (e1, e2) -> cbn e1 >>| fun e1' -> App (e1', e2)
  | _ -> None

(** Normal-order *)
let rec normal : b_expr -> b_expr option = function
  | Abs (_,t,e) -> normal e >>| fun e' -> Abs ((),t,e')
  | App (Abs (_,_,e1), e2) -> some $ sub ~arg:e2 e1
  | App (Var _ as e1, e2) -> normal e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) -> normal e1 >>| fun e1' -> App (e1',e2)
  | _ -> None

(** Applicative-order stuck-ness *)
let rec applicative_stuck : b_expr -> bool = function
  | Var _ -> true
  | Abs (_,_,e) -> applicative_stuck e
  | App (Abs _,_) -> false
  | App (e1,e2) -> applicative_stuck e1 && applicative_stuck e2

(** Applicative-order *)
let rec applicative : b_expr -> b_expr option = function
  | Abs (_,t,e) -> applicative e >>| fun e' -> Abs ((),t,e')
  | App (Abs (_,_,e1), e2)
    when applicative_stuck e1 && applicative_stuck e2 ->
    some $ sub ~arg:e2 e1
  | App (e1, e2) when applicative_stuck e1 ->
    applicative e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) ->
    applicative e1 >>| fun e1' -> App (e1',e2)
  | _ -> None
