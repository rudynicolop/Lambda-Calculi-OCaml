open Core
open Syntax
open Util
open FunUtil
open Triple
open Typing

(** Lexing & parsing from a file. *)
let term_of_file (filename : string) : p_term =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let string_of_b_type_error : type_error -> string = function
  | UnboundVar (_,x) ->
    x
    |> string_of_int
    |> (^) "Unbound variable "
  | BadAbsParam (_,e1,e2,t1) ->
    "In abstraction " ^ string_of_b_term (abs () e1 e2) ^ ",\n"
    ^ "parameter type " ^ string_of_b_term e1 ^ "\n"
    ^ "is expected have a sort type,\n"
    ^ "but has type " ^ string_of_b_term t1
  | IllegalApp (_,e1,e2,t1) ->
    "In application " ^ string_of_b_term (app e1 e2) ^ ",\n"
    ^ "term " ^ string_of_b_term e1 ^ "\n"
    ^ "is expected to have a ∏-type,\n"
    ^ "but has type " ^ string_of_b_term t1
  | TypMismatch (_,e1,e2,t,t2) ->
    "In application " ^ string_of_b_term (app e1 e2) ^ ",\n"
    ^ "term " ^ string_of_b_term e2 ^ "\n"
    ^ "is expected to have type " ^ string_of_b_term t ^ ",\n"
    ^ "but has type " ^ string_of_b_term t2
  | BadPiLeft (_,e1,e2,t1) ->
    "In term " ^ string_of_b_term (pi () e1 e2) ^ ",\n"
    ^ "parameter type " ^ string_of_b_term e1 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ string_of_b_term t1
  | BadPiRight (_,e1,e2,t2) ->
    "In term " ^ string_of_b_term (pi () e1 e2) ^ ",\n"
    ^ "return type " ^ string_of_b_term e2 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ string_of_b_term t2
  | NoAxiom s ->
    "In this system there is no sort s such that " ^ string_of_sort s ^ ":s."
  | NoRulePi (_,t1,t2,s1,s2) ->
    "For term " ^ string_of_b_term (pi () t1 t2) ^ "\n"
    ^ "there is no rule relating sorts "
    ^ string_of_sort s1 ^ " and " ^ string_of_sort s2
  | NoRuleAbs (_,e1,e2,t2,s1,s2) ->
    "For term " ^ string_of_b_term (abs () e1 e2) ^ ",\n"
    ^ "of type " ^ string_of_b_term (pi () e1 t2) ^ ".\n"
    ^ "there is no rule relating sorts "
    ^ string_of_sort s1 ^ " and " ^ string_of_sort s2
  | BadAbsResult (_,e1,e2,t2,t) ->
    "For term " ^ string_of_b_term (abs () e1 e2) ^ ",\n"
    ^ "result " ^ string_of_b_term e2 ^ "\n"
    ^ "has type " ^ string_of_b_term t2 ^ ",\n"
    ^ "which is expected to have a sort type,\n"
    ^ "but has type " ^ string_of_b_term t

let string_of_p_type_error : type_error -> string = function
  | UnboundVar (g,x) ->
    x
    |> var
    |> p_of_b_term (List.length g)
    |> fancy_string_of_p_term
    |> (^) "Unbound variable "
  | BadAbsParam (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ abs () e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In abstraction " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "parameter type " ^ fancy_string_of_p_term e1 ^ "\n"
    ^ "is expected have a sort type,\n"
    ^ "but has type " ^ fancy_string_of_p_term t1
  | IllegalApp (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ app e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In application " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "term " ^ fancy_string_of_p_term e1 ^ "\n"
    ^ "is expected to have a ∏-type,\n"
    ^ "but has type " ^ fancy_string_of_p_term t1
  | TypMismatch (g,e1,e2,t,t2) ->
    let depth = List.length g in
    let e1 = p_of_b_term depth e1 in
    let e2 = p_of_b_term depth e2 in    
    let t2 = p_of_b_term depth t2 in
    let t = p_of_b_term depth t in
    "In application " ^ fancy_string_of_p_term (app e1 e2) ^ ",\n"
    ^ "term " ^ fancy_string_of_p_term e2 ^ "\n"
    ^ "is expected to have type " ^ fancy_string_of_p_term t ^ ",\n"
    ^ "but has type " ^ fancy_string_of_p_term t2
  | BadPiLeft (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ pi () e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In term " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "parameter type " ^ fancy_string_of_p_term e1 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ fancy_string_of_p_term t1
  | BadPiRight (g,e1,e2,t2) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ pi () e1 e2 in
    let e2 = p_of_b_term (1 + depth) e2 in
    let t2 = p_of_b_term (1 + depth) t2 in
    "In term " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "return type " ^ fancy_string_of_p_term e2 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ fancy_string_of_p_term t2
  | NoAxiom s ->
    "In this system there is no sort s such that " ^ string_of_sort s ^ ":s."
  | NoRulePi (g,t1,t2,s1,s2) ->
    let depth = List.length g in
    let t = p_of_b_term depth $ pi () t1 t2 in
    "For term " ^ fancy_string_of_p_term t ^ "\n"
    ^ "there is no rule relating sorts "
    ^ string_of_sort s1 ^ " and " ^ string_of_sort s2
  | NoRuleAbs (g,e1,e2,t2,s1,s2) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ abs () e1 e2 in
    let t = p_of_b_term depth $ pi  () e1 t2 in
    "For term " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "of type " ^ fancy_string_of_p_term t ^ ".\n"
    ^ "there is no rule relating sorts "
    ^ string_of_sort s1 ^ " and " ^ string_of_sort s2
  | BadAbsResult (g,e1,e2,t2,t) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ abs () e1 e2 in
    let e2 = p_of_b_term (1 + depth) e2 in
    let t2 = p_of_b_term (1 + depth) t2 in
    let t = p_of_b_term (1 + depth) t in
    "For term " ^ fancy_string_of_p_term e ^ ",\n"
    ^ "result " ^ fancy_string_of_p_term e2 ^ "\n"
    ^ "has type " ^ fancy_string_of_p_term t2 ^ ",\n"
    ^ "which is expected to have a sort type,\n"
    ^ "but has type " ^ fancy_string_of_p_term t

let repl_b_term (red : b_term -> b_term option) (e : b_term) : unit =
  e
  |> multi_red red
    begin print_endline
      >> (fun str -> str ^ " -->")
      >> fancy_string_of_p_term
      >> p_of_b_term 0
    end
  |> ignore

module Pipeline (SAR : Triple) = struct
  include SAR
  open Judge (SAR)

  let parse_and_type (filename : string) : b_term option =
  let e = filename
          |> term_of_file
          |>
          begin fun e ->
            "Input:\n" ^ string_of_p_term e |> print_endline; e
          end
          |> b_of_p_term [] in
  "Program parsed as:\n" ^ (fancy_string_of_p_term >> p_of_b_term 0) e
  |> print_endline;
  "Debug:\n" ^ string_of_b_term e |> print_endline;
  match [] |- e with
  | Result.Ok t ->
    t
    |> Equiv.weak_norm
    |> p_of_b_term 0
    |> fancy_string_of_p_term
    |> (^) "Program has type "
    |> print_endline; Some e
  | Result.Error err ->
    string_of_b_type_error err |> print_endline; None

  let run_b_term (red : b_term -> b_term option) (filename : string) : unit =
    match parse_and_type filename with
    | None -> ()
    | Some e -> repl_b_term red e
end

module STLC_Pipe                = Pipeline (STLC)
module SystemF_Pipe             = Pipeline (SystemF)
module LambdaOmgea_Pipe         = Pipeline (LambdaOmgea)
module SysFOmega_Pipe           = Pipeline (SysFOmega)
module LambdaP_Pipe             = Pipeline (LambdaP)
module LambdaP_SysF_Pipe        = Pipeline (LambdaP_SysF)
module LambdaP_Omega_Pipe       = Pipeline (LambdaP_Omega)
module COC_Pipe                 = Pipeline (COC)
module Lambda_HOL_Pipe          = Pipeline (Lambda_HOL)
module Lambda_HOL_Extended_Pipe = Pipeline (Lambda_HOL_Extended)
module System_U_Minus_Pipe      = Pipeline (System_U_Minus)
module SystemU_Pipe             = Pipeline (SystemU)
