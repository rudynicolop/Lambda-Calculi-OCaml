open Core
open Ast    

(** Pipeline for lexing, parsing, running *)

let parse (filename : string) : p_expr =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let to_p_expr (e : b_expr) : p_expr = p_expr_of_b_expr 0 e

let to_b_expr (e : p_expr) : b_expr = b_expr_of_p_expr [] e

let rec eval
    (s : b_expr -> b_expr option)
    (f : b_expr -> unit) (e : b_expr) : b_expr option =
  let open Option in
  f e; s e >>= eval s f

let (>>) f g = fun x -> x |> g |> f

let step_print (s : b_expr -> b_expr option) (e : b_expr) : unit =
  e
  |>
  eval s
    (print_endline
     >> (fun str -> str ^ " ->")
     >> string_of_p_expr
     >> to_p_expr
     (*>> string_of_b_expr*)
    )
  |> ignore

let pipe (s : b_expr -> b_expr option) (filename : string) : unit =
  filename
  |> parse
  |> to_b_expr
  |> step_print s
