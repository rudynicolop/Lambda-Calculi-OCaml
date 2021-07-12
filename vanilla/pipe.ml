open Core
open Ast
open Util
open FunUtil

(** Pipeline for lexing, parsing, running *)

let parse (filename : string) : p_expr =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let to_p_expr (e : b_expr) : p_expr = p_expr_of_b_expr 0 e

let to_b_expr (e : p_expr) : b_expr = b_expr_of_p_expr [] e

let to_h_expr (e : b_expr) : h_expr option = h_expr_of_b_expr [] e

let step_print_b_expr (s : b_expr -> b_expr option) (e : b_expr) : unit =
  e
  |>
  multi_red s
    (print_endline
     >> (fun str -> str ^ " ->")
     >> string_of_p_expr
     >> to_p_expr)
  |> ignore

let pipe_b_expr (s : b_expr -> b_expr option) (filename : string) : unit =
  filename
  |> parse
  |> to_b_expr
  |> step_print_b_expr s

let step_print_h_expr (s : h_expr -> h_expr option) (e : h_expr) : unit =
  e
  |>
  multi_red s
    (print_endline
     >> (fun str -> str ^ " ->")
     >> string_of_p_expr
     >> p_expr_of_h_expr 0
    )
  |> ignore

let pipe_h_expr (s : h_expr -> h_expr option) (filename : string) : unit =
  filename
  |> parse
  |> to_b_expr
  |> to_h_expr
  |> fun oe ->
  match oe with
  | None -> print_endline "Not a closed term!"
  | Some e -> step_print_h_expr s e
