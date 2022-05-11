open Ast
open Core
open Option
open Util
open FunUtil
open CompUtil

let rec rename (r : int -> int) : b_expr -> b_expr =
  function
  | Var x -> Var (r x)
  | Lam (_,e) -> Lam ((), rename (ext r) e)
  | App (e1,e2) -> App (rename r e1, rename r e2)

let exts (s : int -> b_expr) (x : int) : b_expr =
  if x < 1 then Var x else rename ((+) 1) $ s (x - 1)

let rec subs (s : int -> b_expr) : b_expr -> b_expr =
  function
  | Var x -> s x
  | Lam (_,e) -> Lam ((), subs (exts s) e)
  | App (e1,e2) -> App (subs s e1, subs s e2)

let sub_helper (e : b_expr) (x : int) : b_expr =
  if x = 0 then e else Var (x - 1)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : b_expr) : b_expr -> b_expr =
  subs $ sub_helper arg

(** Call-by-value *)
let rec cbv (e : b_expr) : b_expr option =
  match e with
  | App (Lam (_,e1), (Lam (_,_) | Var _ as e2)) -> some $ sub ~arg:e2 e1
  | App (e1, e2) ->
    begin match cbv e1 with
    | None -> cbv e2 >>| fun e2' -> App (e1, e2')
    | Some e1' -> some $ App (e1', e2)
    end
  | _ -> None

(** Call-by-name *)
let rec cbn (e : b_expr) : b_expr option =
  match e with
  | App (Lam (_,e1), e2) -> some $ sub ~arg:e2 e1
  | App (e1, e2) -> cbn e1 >>| fun e1' -> App (e1', e2)
  | _ -> None

(** Normal-order *)
let rec normal (e : b_expr) : b_expr option =
  match e with
  | Lam (_,e) -> normal e >>| fun e' -> Lam ((),e')
  | App (Lam (_,e1), e2) -> some $ sub ~arg:e2 e1
  | App (Var _ as e1, e2) -> normal e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) -> normal e1 >>| fun e1' -> App (e1',e2)
  | _ -> None

(** Applicative-order stuck-ness *)
let rec applicative_stuck (e : b_expr) : bool =
  match e with
  | Var _ -> true
  | Lam (_,e) -> applicative_stuck e
  | App (Lam _,_) -> false
  | App (e1,e2) -> applicative_stuck e1 && applicative_stuck e2

(** Applicative-order *)
let rec applicative (e : b_expr) : b_expr option =
  match e with
  | Lam (_, e) -> applicative e >>| fun e' -> Lam ((),e')
  | App (Lam (_,e1), e2) when applicative_stuck e1 && applicative_stuck e2
    -> some $ sub ~arg:e2 e1
  | App (e1, e2) when applicative_stuck e1 ->
    applicative e2 >>| fun e2' -> App (e1,e2')
  | App (e1, e2) ->
    applicative e1 >>| fun e1' -> App (e1',e2)
  | _ -> None

(** HOAS cbn evaluation: *)
let rec hoas_cbn (e : h_expr) : h_expr option =
  match e with
  | HApp (HLam f, e) -> some $ f e
  | HApp (e1, e2) ->
    hoas_cbn e1 >>| fun e1' -> HApp (e1', e2)
  | _ -> None

(** HOAS cbv evaluation: *)
let rec hoas_cbv (e : h_expr) : h_expr option =
  match e with
  | HApp (HLam f, (HLam _ as e)) -> some $ f e
  | HApp (e1, e2) ->
    begin match hoas_cbv e1 with
    | None -> hoas_cbv e2 >>| fun e2' -> HApp (e1, e2')
    | Some e1' -> some $ HApp (e1', e2)
    end
  | _ -> None

(** Call-by-need evaluation. *)

let rec nshift (c : int) (i : int) (e : l_expr) : l_expr =
  match e with
  | Var (L n) -> if n < c then Var (L n) else Var (L (n + i))
  | Var (R _) -> e
  | Lam (_,e) -> Lam ((), nshift (c + 1) i e)
  | App (e1,e2) -> App (nshift c i e1, nshift c i e2)

(** Beta-reduction substitution. *)
let rec nsub (m : int) (esub : l_expr) (e : l_expr) : l_expr =
  match e with
  | Var (L n) -> if n = m then esub else Var (L n)
  | Var (R _) -> e
  | Lam (_,e) -> Lam ((), nsub (m + 1) (nshift 0 1 esub) e)
  | App (e1,e2) -> App (nsub m esub e1, nsub m esub e2)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let nbr (e1 : l_expr) (e2 : l_expr) : l_expr =
  nshift 0 (-1) $ nsub 0 (nshift 0 1 e2) e1

(** Lazy substitution. *)
let rec lsub (m : int) (esub : l_expr) (e : l_expr) : l_expr =
  match e with
  | Var (L _) -> e
  | Var (R n) -> if n = m then esub else Var (L n)
  | Lam (_,e) -> Lam ((), lsub m (nshift 0 1 esub) e)
  | App (e1,e2) -> App (lsub m esub e1, lsub m esub e2)

type ('a,'b) state = 'a option * 'b

let (>>==) (a,st : ('a,'b) state) (f : 'a -> 'c option) : ('c,'b) state =
  a >>= f, st

let (>>||) (a,st : ('a,'b) state) (f : 'a -> 'c) : ('c,'b) state =
  a >>| f, st

let (>=>) (a,st : ('a,'b) state)
    (f : 'b -> 'a -> ('c,'b) state) : ('c,'b) state =
  match a with
  | None -> None, st
  | Some a -> f st a

let update (a,_ : ('a,'b) state) (st : 'b) = a,st

let rec insert (n : int) (a : 'a) (l : 'a list) : 'a list =
  match n, l with
  | _, [] -> []
  | 0, _ :: t -> a :: t
  | _, h :: t -> h :: insert (n - 1) a t

(** Lazy normal-order evaluation. *)
let rec need
    (env : l_expr list) (e : l_expr) : (l_expr, l_expr list) state =
  match e with
  | Lam (_,e) -> need env e >>|| fun e' -> Lam ((), e')
  | App (Lam (_,e1), e2) ->
    Some (nbr e1 $ Var (R (List.length env))), env @ [e2]
  | App (Var (R n), e2) ->
    begin match List.nth env n with
      | Some (Lam _ as e1) -> some $ App (e1, e2), env
      | Some e1 ->
        need env e1 >=> fun env e1' ->
          Some e, insert n e1' env
      | None -> None, env
    end
  | App (e1, e2) -> need env e1 >>|| fun e1' -> App (e1', e2)
  | _ -> None, env
