open Ast
open Core
open Option

(** Haskell-style dollar-sign *)
let ($) f x = f x

(** Shifts free variables above a cutoff [c] by [i] *)
let rec shift (c : int) (i : int) (e : b_expr) : b_expr =
  match e with
  | Var n -> if n < c then Var n else Var (n + i)
  | Lam (_,e) -> Lam ((), shift (c + 1) i e)
  | App (e1,e2) -> App (shift c i e1, shift c i e2)

(** Substitution [e{esub/m}] *)
let rec sub (m : int) (esub : b_expr) (e : b_expr) : b_expr =
  match e with
  | Var n -> if n = m then esub else Var n
  | Lam (_,e) -> Lam ((), sub (m + 1) (shift 0 1 esub) e)
  | App (e1,e2) -> App (sub m esub e1, sub m esub e2)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let beta_reduce (e1 : b_expr) (e2 : b_expr) : b_expr =
  shift 0 (-1) $ sub 0 (shift 0 1 e2) e1

(** Call-by-value *)
let rec cbv (e : b_expr) : b_expr option =
  match e with
  | App (Lam (_,e1), (Lam (_,_) | Var _ as e2)) -> some $ beta_reduce e1 e2
  | App (e1, e2) ->
    begin match cbv e1 with
    | None -> cbv e2 >>| fun e2' -> App (e1, e2')
    | Some e1' -> some $ App (e1', e2)
    end
  | _ -> None

(** Call-by-name *)
let rec cbn (e : b_expr) : b_expr option =
  match e with
  | App (Lam (_,e1), e2) -> some $ beta_reduce e1 e2
  | App (e1, e2) -> cbn e1 >>| fun e1' -> App (e1', e2)
  | _ -> None

(** Normal-order *)
let rec normal (e : b_expr) : b_expr option =
  match e with
  | Lam (_,e) -> normal e >>| fun e' -> Lam ((),e')
  | App (Lam (_,e1), e2) -> some $ beta_reduce e1 e2
  | App (Var _ as e1, e2) -> normal e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) -> normal e1 >>| fun e1' -> App (e1',e2)
  | _ -> None

(** Applicative-order stuck-ness *)
let rec applicative_stuck (e : b_expr) : bool =
  match e with
  | Var _ -> true
  | Lam (_,e) -> applicative_stuck e
  | App (e1,e2) -> applicative_stuck e1 && applicative_stuck e2

(** Applicative-order *)
let rec applicative (e : b_expr) : b_expr option =
  match e with
  | Lam (_, e) -> applicative e >>| fun e' -> Lam ((),e')
  | App (Lam (_,e1), e2) when applicative_stuck e1 && applicative_stuck e2
    -> some $ beta_reduce e1 e2
  | App (e1, e2) when applicative_stuck e1 ->
    applicative e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) ->
    applicative e1 >>| fun e1' -> App (e1',e2)
  | _ -> None
