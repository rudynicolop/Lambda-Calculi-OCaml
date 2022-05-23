open Core
open Util
open CompUtil
open FunUtil
open Syntax

let rec rename (r : int -> int)
  : b_term -> b_term = function
  | Sort s -> Sort s
  | Var x -> var $ r x
  | Abs (_,t1,t2) -> abs () (rename r t1) $ rename (ext r) t2
  | App (t1,t2) -> app (rename r t1) $ rename r t2
  | Pi (_,t1,t2) -> pi () (rename r t1) $ rename (ext r) t2

let exts (s : int -> b_term) (x : int) : b_term =
  if x < 1 then Var x else
    ("Extending for var " ^ string_of_int x |> print_endline;
     let sx = s (x - 1) in
     "Renaming " ^ string_of_b_term sx |> print_endline;
     rename ((+) 1) $ sx)

let rec subs (s : int -> b_term)
  : b_term -> b_term = function
  | Sort s -> Sort s
  | Var x ->
    "Subing into var " ^ string_of_int x |> print_endline;
    let sx = s x in
    "After substitution is " ^ string_of_b_term sx |> print_endline;
    sx
  | Abs (_,t1,t2) -> abs () (subs s t1) $ subs (exts s) t2
  | App (t1,t2) -> app (subs s t1) $ subs s t2
  | Pi (_,t1,t2) as p ->
    "Subing into ∏-type: " ^ string_of_b_term p
    |> print_endline;
    pi () (subs s t1) $ subs (exts s) t2

let sub_helper (e : b_term) (x : int) : b_term =
  "Sub-helper var: " ^ string_of_int x |> print_endline;
  "Sub-helper subsitute: " ^ string_of_b_term e |> print_endline;
  if x = 0 then
    (print_endline "Sub-helper subs!"; e)
  else
    ("Sub helper shifts var down to " ^ string_of_int (x - 1)
     |> print_endline; Var (x - 1))

(** Beta-reduction of [(fun x => e1) e2 -> e1{e2/x}] *)
let sub ~arg:(arg : b_term) : b_term -> b_term =
  subs $ sub_helper arg
