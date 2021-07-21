open Core
open Option
open Syntax
open Util
open FunUtil
open CompUtil
open IntComp

(** Dynamic Semantics. *)

(** Shifts free variables above a cutoff [c] by [i] *)
let rec shift (c : int) (i : int)
  : b_expr -> b_expr = function
  | Var n -> if n < c then Var n else Var (n + i)
  | Abs (_,t,e) -> Abs ((), t, shift (c + 1) i e)
  | App (e1,e2) -> App (shift c i e1, shift c i e2)


(** Substitution [e{esub/m}] *)
let rec sub (m : int) (esub : b_expr)
  : b_expr -> b_expr = function
  | Var n ->
    begin match n <=> m with
      | LT -> Var (n - 1)
      | EQ -> shift 0 m esub
      | GT -> Var n
    end
  | Abs (_,t,e) -> Abs ((), t, sub (m + 1) esub e)
  | App (e1,e2) -> App (sub m esub e1, sub m esub e2)

(** Beta-reduction of [(fun x:t => e1) e2 -> e1{e2/x}] *)
let beta_reduce (e1 : b_expr) (e2 : b_expr) : b_expr = sub 0 e2 e1

(** Call-by-value *)
let rec cbv : b_expr -> b_expr option = function
  | App (Abs (_,_,e1), (Abs _ | Var _ as e2)) ->
    some $ beta_reduce e1 e2
  | App (e1, e2) ->
    begin match cbv e1 with
    | None -> cbv e2 >>| fun e2' -> App (e1, e2')
    | Some e1' -> some $ App (e1', e2)
    end
  | _ -> None

(** Call-by-name *)
let rec cbn : b_expr -> b_expr option = function
  | App (Abs (_,_,e1), e2) -> some $ beta_reduce e1 e2
  | App (e1, e2) -> cbn e1 >>| fun e1' -> App (e1', e2)
  | _ -> None

(** Normal-order *)
let rec normal : b_expr -> b_expr option = function
  | Abs (_,t,e) -> normal e >>| fun e' -> Abs ((),t,e')
  | App (Abs (_,_,e1), e2) -> some $ beta_reduce e1 e2
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
    some $ beta_reduce e1 e2
  | App (e1, e2) when applicative_stuck e1 ->
    applicative e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) ->
    applicative e1 >>| fun e1' -> App (e1',e2)
  | _ -> None
