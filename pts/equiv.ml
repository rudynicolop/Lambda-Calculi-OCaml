open Core
open Option
open Util
open FunUtil
open Syntax
open Sub

(** Weak-head reduction. *)
let rec weak_red : b_term -> b_term option = function
  | App (Abs (_,_,t1), t2) -> some $ sub ~arg:t2 t1
  | App (t1, t2) ->
    weak_red t1 >>| fun t1' -> app t1' t2
  | Pi (_,t1,t2) ->
    begin match weak_red t1 with
      | Some t1' -> some $ pi () t1' t2
      | None     -> weak_red t2 >>| pi () t1
    end
  | _ -> None

(** Weak-head normalization. *)
let rec weak_norm (t : b_term) : b_term =
  match weak_red t with
  | Some t' -> weak_norm t'
  | None -> t
    
let rec (==) (t1 : b_term) (t2 : b_term) : bool =
    equiv_wf (weak_norm t1) $ weak_norm t2
  
and equiv_wf (t1 : b_term) (t2 : b_term) : bool =
  match t1, t2 with
  | Sort s1, Sort s2 -> s1 =? s2
  | Var x, Var y -> x = y
  | Abs (_,t1,t2), Abs (_,e1,e2) -> t1 == e1 && t2 == e2
  | App (t1,t2), App (e1,e2)
  | Pi (_,t1,t2),  Pi (_,e1,e2) -> equiv_wf t1 e1 && equiv_wf t2 e2
  | Abs (_,_,t1), t2 | t2, Abs (_,_,t1) ->
    app (rename ((+) 1) t1) $ var 0 == t2
  | _ -> false
