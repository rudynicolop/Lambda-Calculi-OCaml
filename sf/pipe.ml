open Core
open Util
open FunUtil
open Syntax

(** Pipeline for lexing, parsing, typing, & running *)

let parse (filename : string) : p_expr =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let to_p_expr : b_expr -> p_expr = p_of_b_expr 0 0

let to_b_expr : p_expr -> b_expr = b_of_p_expr [] []

let repl_b_expr (red : b_expr -> b_expr option) (e : b_expr) : unit =
  e
  |> multi_red red
    begin print_endline
      >> (fun str -> str ^ " ->")
      >> string_of_p_expr
      >> to_p_expr
    end
  |> ignore

let string_of_type_error
  : Type.type_error -> string = function
  | Type.UnboundVar (td,g,n) ->
    Var n
    |> p_of_b_expr td (List.length g)
    |> string_of_p_expr
    |> (^) "Unbound variable "
  | Type.IllegalApp (td,g,t1,e1,e2) ->
    let d = List.length g in
    let t1' = p_of_b_typ td t1 in
    let e1' = p_of_b_expr td d e1 in
    let e2' = p_of_b_expr td d e2 in
    "In application " ^ (string_of_p_expr $ app e1' e2') ^
    ", sub-term " ^ string_of_p_expr e1' ^
    " is expected to have an arrow type, but has type " ^
    string_of_p_typ t1'
  | Type.TypMismatch (td,g,t,t2,e1,e2) ->
    let d = List.length g in
    let t' = p_of_b_typ td t in
    let t2' = p_of_b_typ td t2 in
    let e1' = p_of_b_expr td d e1 in
    let e2' = p_of_b_expr td d e2 in
    "In application " ^ (string_of_p_expr $ app e1' e2') ^
    ", argument " ^ string_of_p_expr e2' ^
    " is expected to have type " ^ string_of_p_typ t' ^
    ", but has type " ^ string_of_p_typ t2'
  | Type.IllegalTypApp (td,g,e,te,t) ->
    let d = List.length g in
    let t' = p_of_b_typ td t in
    let te' = p_of_b_typ td te in
    let e' = p_of_b_expr td d e in
    "In type-application " ^
    (string_of_p_expr $ typapp e' t') ^
    ", sub-term " ^ string_of_p_expr e' ^
    " is expected to have a parametric type, but has type " ^
    string_of_p_typ te'
  | Type.NotWellFounded (td,g,t,e) ->
    let d = List.length g in
    let t' = p_of_b_typ td t in
    let e' = p_of_b_expr td d e in
    "In term " ^ string_of_p_expr e' ^
    ", type " ^ string_of_p_typ t' ^
    " contains type-variables not bound by the local context."
    
let parse_and_type (filename : string) : b_expr option =
  let e = filename
          |> parse
          |> to_b_expr in
  "Program parsed as " ^ (string_of_p_expr >> to_p_expr) e
  |> print_endline;
  match Type.type_b_expr 0 [] e with
  | Result.Ok t ->
    let t' = t |> p_of_b_typ 0 |> string_of_p_typ in
    "Program has type " ^ t' |> print_endline;
    Some e
  | Result.Error err ->
    string_of_type_error err |> print_endline; None

let run_b_expr (red : b_expr -> b_expr option) (filename : string) : unit =
  match parse_and_type filename with
  | None -> ()
  | Some e -> repl_b_expr red e
