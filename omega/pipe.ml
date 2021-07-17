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

let string_of_kind_error
  : Kinding.kind_error -> string = function
  | Kinding.UnboundTypVar (g, n) ->
    n
    |> tvar
    |> p_of_b_typ (List.length g)
    |> string_of_p_typ
    |> (^) "Unbound type variable "
  | Kinding.IllegalTypApp (g,k,t1,t2) ->
    let d = List.length g in
    let t1' = p_of_b_typ d t1 in
    let t2' = p_of_b_typ d t2 in
    "In type application " ^ (string_of_p_typ $ tapp t1' t2') ^
    ", type " ^ string_of_p_typ t1' ^
    " is expected to have an arrow kind, but has kind " ^
    string_of_kind k
  | Kinding.KindMismatch (g,k1,k2,t1,t2) ->
    let d = List.length g in
    let t1' = p_of_b_typ d t1 in
    let t2' = p_of_b_typ d t2 in
    "In type application " ^ (string_of_p_typ $ tapp t1' t2') ^
    ", argument " ^ string_of_p_typ t2' ^
    " is expected to have kind " ^ string_of_kind k1 ^
    ", but has kind " ^ string_of_kind k2
  | Kinding.BadArrowKind (g,k,ta,t) ->
    let d = List.length g in
    let ta' = p_of_b_typ d ta in
    let t' = p_of_b_typ d t in
    "In arrow-type " ^ string_of_p_typ ta' ^
    ", type " ^ string_of_p_typ t' ^
    " is expected to have kind " ^ string_of_kind KStar ^
    ", but has kind " ^ string_of_kind k

let string_of_type_error
  : Typing.type_error -> string = function
  | Typing.UnboundVar (g,n) ->
    n
    |> var
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
  | Typing.ImproperKind (g,k,t,e) ->
    let d = List.length g in
    let t' = p_of_b_typ 0 t in
    let e' = p_of_b_term d e in
    "In term " ^ string_of_p_term e' ^
    ", type " ^ string_of_p_typ t' ^
    " is expected to have kind " ^ string_of_kind KStar ^
    ", but has kind " ^ string_of_kind k
  | Typing.KindingError err -> string_of_kind_error err
    
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
    t
    |> TypeReduce.normalize
    |> p_of_b_typ 0
    |> string_of_p_typ
    |> (^) "Program has type "
    |> print_endline; Some e
  | Result.Error err ->
    string_of_type_error err |> print_endline; None

let run_b_term (red : b_term -> b_term option) (filename : string) : unit =
  match parse_and_type filename with
  | None -> ()
  | Some e -> repl_b_term red e
