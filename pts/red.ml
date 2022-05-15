open Core
open Option
open Util
open FunUtil
open Syntax
open Sub

let rec stuck : b_term -> bool = function
  | Sort _
  | Var _ -> true
  | App (Abs (_,_,_), _) -> false
  | App (t1,t2)
  | Abs (_,t1,t2)
  | Pi (_,t1,t2) -> stuck t1 && stuck t2

(** Full Normal-order reduction (left-most/outer-most redux). *)
let rec normal : b_term -> b_term option = function
  | App (Abs (_,_,t1), t2) -> some $ sub ~arg:t2 t1
  | App (t1,t2) when stuck t1 -> normal t2 >>| app t1
  | App (t1,t2) -> normal t1 >>| fun t1' -> app t1' t2
  | Abs (_,t1,t2) when stuck t1 -> normal t2 >>| abs () t1
  | Abs (_,t1,t2) -> normal t1 >>| fun t1' -> abs () t1' t2   
  | Pi (_,t1,t2) when stuck t1 -> normal t2 >>| pi () t1
  | Pi (_,t1,t2) -> normal t1 >>| fun t1' -> pi () t1' t2
  | _ -> None

(** Full Applicative-order reduction (left-most/inner-most redux). *)
let rec appl : b_term -> b_term option = function
  | App (Abs (_,t1,t2), e) when stuck t1 && stuck t2 && stuck e ->
    some $ sub ~arg:t2 e
  | App (t1, t2) when stuck t1 -> appl t2 >>| app t1
  | App (t1, t2) -> appl t1 >>| fun t1' -> app t1' t2
  | Abs (_, t1, t2) when stuck t1 -> appl t2 >>| abs () t1
  | Abs (_, t1, t2) -> appl t1 >>| fun t1' -> abs () t1' t2
  | Pi (_, t1, t2) when stuck t1 -> appl t2 >>| pi () t1
  | Pi (_, t1, t2) -> appl t1 >>| fun t1' -> pi () t1' t2
  | _ -> None
