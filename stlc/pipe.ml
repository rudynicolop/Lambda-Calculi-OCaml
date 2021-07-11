open Core
open Syntax

(** Pipeline for lexing, parsing, typing, & running *)

let parse (filename : string) : p_expr =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let to_p_expr : b_expr -> p_expr = p_of_b_expr 0

let to_b_expr : p_expr -> b_expr = b_of_p_expr []

let rec multi_red
    (red : 'a -> 'a option)
    (f : 'a -> unit) (e : 'a) : 'a option =
  let open Option in
  f e; red e >>= multi_red red f

let (>>) f g = fun x -> x |> g |> f

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
  | Type.UnboundVar (g,n) ->
    Var n
    |> p_of_b_expr (List.length g)
    |> string_of_p_expr
    |> (^) "Unbound variable "
  | Type.IllegalApp (g,t1,e1,e2) ->
    let d = List.length g in
    let e1' = p_of_b_expr d e1 in
    let e2' = p_of_b_expr d e2 in
    "In application " ^ string_of_p_expr (App(e1',e2')) ^
    ", sub-expression " ^ string_of_p_expr e1' ^
    " is expected to have an arrow type, but has type " ^
    string_of_typ t1
  | Type.TypMismatch (g,t,t2,e1,e2) ->
    let d = List.length g in
    let e1' = p_of_b_expr d e1 in
    let e2' = p_of_b_expr d e2 in
    "In application " ^ string_of_p_expr (App(e1',e2')) ^
    ", argument " ^ string_of_p_expr e2' ^
    " is expected to have type " ^ string_of_typ t ^
    ", but has type " ^ string_of_typ t2
    
let parse_and_type (filename : string) : b_expr option =
  let e = filename
          |> parse
          |> to_b_expr in
  "Program parsed as " ^ (string_of_p_expr >> to_p_expr) e
  |> print_endline;
  match Type.type_b_expr [] e with
  | Ok t ->
    "Program has type " ^ string_of_typ t |> print_endline;
    Some e
  | Error err ->
    string_of_type_error err |> print_endline; None

let run_b_expr (red : b_expr -> b_expr option) (filename : string) : unit =
  match parse_and_type filename with
  | None -> ()
  | Some e -> repl_b_expr red e
