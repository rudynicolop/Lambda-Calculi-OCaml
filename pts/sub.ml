open Core
open Util
open CompUtil
open FunUtil
open Syntax

let rec rename (r : int -> int)
  : 's b_term -> 's b_term = function
  | Sort s -> Sort s
  | Var x -> var $ r x
  | Abs (_,t1,t2) -> abs () (rename r t1) $ rename (ext r) t2
  | App (t1,t2) -> app (rename r t1) $ rename r t2
  | Pi (_,t1,t2) -> pi () (rename r t1) $ rename (ext r) t2

let exts (s : int -> 's b_term) (x : int) : 's b_term =
  if x < 1 then Var x else rename ((+) 1) $ s x

let rec subs (s : int -> 's b_term)
  : 's b_term -> 's b_term = function
  | Sort s -> Sort s
  | Var x -> s x
  | Abs (_,t1,t2) -> abs () (subs s t1) $  subs (exts s) t2
  | App (t1,t2) -> app (subs s t1) $ subs s t2
  | Pi (_,t1,t2) -> pi () (subs s t1) $  subs (exts s) t2

let sub_helper (e : 's b_term) (x : int) : 's b_term =
  if x = 0 then e else Var (x - 1)

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : 's b_term) : 's b_term -> 's b_term =
  subs $ sub_helper arg
