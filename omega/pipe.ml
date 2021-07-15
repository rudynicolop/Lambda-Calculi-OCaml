open Core
open Util
open FunUtil
open Syntax
open Fold

(** Pipeline for lexing, parsing, typing, & running *)

let parse (filename : string) : p_term =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let to_p_term : b_term -> p_term = p_of_b_term 0

let to_b_term : p_term -> b_term = b_of_p_term []

let repl_b_term (red : b_term -> b_term option) (e : b_term) : unit =
  e
  |> multi_red red
    begin print_endline
      >> (fun str -> str ^ " ->")
      >> string_of_p_term
      >> to_p_term
    end
  |> ignore

let string_of_type_error
  : Typing.type_error -> string = function
  | Typing.UnboundVar (g,n) ->
    Var n
    |> p_of_b_term (List.length g)
    |> string_of_p_term
    |> (^) "Unbound variable "
  | Typing.IllegalApp (g,t1,e1,e2) ->
    let d = List.length g in
    let t1' = p_of_b_typ 0 t1 in
    let e1' = p_of_b_term d e1 in
    let e2' = p_of_b_term d e2 in
    "In application " ^ (string_of_p_term $ app e1' e2') ^
    ", sub-term " ^ string_of_p_term e1' ^
    " is expected to have an arrow type, but has type " ^
    string_of_p_typ t1'
  | Typing.TypMismatch (g,t,t2,e1,e2) ->
    let d = List.length g in
    let t' = p_of_b_typ 0 t in
    let t2' = p_of_b_typ 0 t2 in
    let e1' = p_of_b_term d e1 in
    let e2' = p_of_b_term d e2 in
    "In application " ^ (string_of_p_term $ app e1' e2') ^
    ", argument " ^ string_of_p_term e2' ^
    " is expected to have type " ^ string_of_p_typ t' ^
    ", but has type " ^ string_of_p_typ t2'
  | _ -> failwith "TODO: [string_of_type_error] cases."
    
let parse_and_type (filename : string) : b_term option =
  let e = filename
          |> parse
          |> to_b_term in
  "Program parsed as " ^ (string_of_p_term >> to_p_term) e
  |> print_endline;
  (*"De Bruin: " ^ string_of_b_term e
    |> print_endline;*)
  match Typing.typing [] e with
  | Result.Ok t ->
    let t' = t |> p_of_b_typ 0 |> string_of_p_typ in
    "Program has type " ^ t' |> print_endline;
    Some e
  | Result.Error err ->
    string_of_type_error err |> print_endline; None

let run_b_term (red : b_term -> b_term option) (filename : string) : unit =
  match parse_and_type filename with
  | None -> ()
  | Some e -> repl_b_term red e
