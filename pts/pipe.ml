open Core
open Syntax
open Util
open FunUtil
open Triple

(** Parsed syntax to De Bruijn Syntax
    [\x.\y.x -> \.\.1] *)
let rec b_term_of_p_term (ctx : string list) : 's p_term -> 's b_term =
  function
  | Sort s -> sort s
  | Var x -> var $ ListUtil.index_of_default String.(=) x ctx
  | Abs (x,t1,t2) -> abs () (b_term_of_p_term ctx t1) $ b_term_of_p_term (x :: ctx) t2
  | App (e1,e2)   -> app (b_term_of_p_term ctx e1) $ b_term_of_p_term ctx e2
  | Pi (x,t1,t2)  -> pi () (b_term_of_p_term ctx t1) $ b_term_of_p_term (x :: ctx) t2

(** De Bruijn to Parsed Syntax
    [\.\.1 -> \x1.\x2.x1] *)
let rec p_term_of_b_term (depth : int) : 's b_term -> 's p_term =
  function
  | Sort s -> sort s
  | Var n -> var $ "x" ^ string_of_int (depth - n)
  | Abs (_,t1,t2) ->
    abs
      ("x" ^ string_of_int (depth+1))
      (p_term_of_b_term depth t1)
    $ p_term_of_b_term (depth+1) t2
  | App (e1,e2) -> app (p_term_of_b_term depth e1) $ p_term_of_b_term depth e2
  | Pi (_,t1,t2) ->
    pi
      ("x" ^ string_of_int (depth+1))
      (p_term_of_b_term depth t1)
    $ p_term_of_b_term (depth+1) t2

module type LexParse = sig
  include Triple

  (** Lexing & parsing from a file. *)
  val term_of_file : string -> sort p_term
end

let cube_term_of_file (filename : string) : cube_sort p_term =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

module Pipeline (SAR : Triple) = struct
  include SAR
  
  let rec string_of_p_term : sort p_term -> string =
    function
    | Sort s -> string_of_sort s
    | Var x  -> x
    | Abs (x,t1,t2) -> "(λ" ^ x ^ ":" ^ string_of_p_term t1 ^ "." ^ string_of_p_term t2 ^ ")"
    | Pi  (x,t1,t2) -> "(∏" ^ x ^ ":" ^ string_of_p_term t1 ^ "." ^ string_of_p_term t2 ^ ")"
    | App (t1,t2)   -> "(" ^ string_of_p_term t1 ^ " " ^ string_of_p_term t2  ^ ")"
end
