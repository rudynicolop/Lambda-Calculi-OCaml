open Core
open Syntax
open Util
open FunUtil
open Triple
open Typing

(** Parsed syntax to De Bruijn Syntax
    [\x.\y.x -> \.\.1] *)
let rec b_of_p_term (ctx : string list) : p_term -> b_term =
  function
  | Sort s -> sort s
  | Var x -> var $ ListUtil.index_of_default String.(=) x ctx
  | Abs (x,t1,t2) -> abs () (b_of_p_term ctx t1) $ b_of_p_term (x :: ctx) t2
  | App (e1,e2)   -> app (b_of_p_term ctx e1) $ b_of_p_term ctx e2
  | Pi (x,t1,t2)  -> pi () (b_of_p_term ctx t1) $ b_of_p_term (x :: ctx) t2

(** De Bruijn to Parsed Syntax
    [\.\.1 -> \x1.\x2.x1] *)
let rec p_of_b_term (depth : int) : b_term -> p_term =
  function
  | Sort s -> sort s
  | Var n -> var $ "x" ^ string_of_int (depth - n)
  | Abs (_,t1,t2) ->
    abs
      ("x" ^ string_of_int (depth+1))
      (p_of_b_term depth t1)
    $ p_of_b_term (depth+1) t2
  | App (e1,e2) -> app (p_of_b_term depth e1) $ p_of_b_term depth e2
  | Pi (_,t1,t2) ->
    pi
      ("x" ^ string_of_int (depth+1))
      (p_of_b_term depth t1)
    $ p_of_b_term (depth+1) t2

(** Lexing & parsing from a file. *)
let cube_term_of_file (filename : string) : p_term =
  filename
  |> Stdlib.open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.tokenize

let rec string_of_p_term : p_term -> string =
  function
  | Sort s -> string_of_sort s
  | Var x  -> x
  | Abs (x,t1,t2) -> "(λ" ^ x ^ ":" ^ string_of_p_term t1 ^ "." ^ string_of_p_term t2 ^ ")"
  | Pi  (x,t1,t2) -> "(∏" ^ x ^ ":" ^ string_of_p_term t1 ^ "." ^ string_of_p_term t2 ^ ")"
  | App (t1,t2)   -> "(" ^ string_of_p_term t1 ^ " " ^ string_of_p_term t2  ^ ")"

let string_of_type_error : type_error -> string = function
  | UnboundVar (g,x) ->
    x
    |> var
    |> p_of_b_term (List.length g)
    |> string_of_p_term
    |> (^) "Unbound variable "
  | BadAbsParam (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ abs () e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In abstraction " ^ string_of_p_term e ^ ",\n"
    ^ "parameter type " ^ string_of_p_term e1 ^ "\n"
    ^ "is expected have a sort type,\n"
    ^ "but has type " ^ string_of_p_term t1
  | IllegalApp (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ app e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In application " ^ string_of_p_term e ^ ",\n"
    ^ "term " ^ string_of_p_term e1 ^ "\n"
    ^ "is expected to have a ∏-type,\n"
    ^ "but has type " ^ string_of_p_term t1
  | BadPiLeft (g,e1,e2,t1) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ pi () e1 e2 in
    let e1 = p_of_b_term depth e1 in
    let t1 = p_of_b_term depth t1 in
    "In term " ^ string_of_p_term e ^ ",\n"
    ^ "parameter type " ^ string_of_p_term e1 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ string_of_p_term t1
  | BadPiRight (g,e1,e2,t2) ->
    let depth = List.length g in
    let e = p_of_b_term depth $ pi () e1 e2 in
    let e2 = p_of_b_term (1 + depth) e2 in
    let t2 = p_of_b_term (1 + depth) t2 in
    "In term " ^ string_of_p_term e ^ ",\n"
    ^ "return type " ^ string_of_p_term e2 ^ ",\n"
    ^ "is expected to have a sort type,\n"
    ^ "but has type " ^ string_of_p_term t2
  | NoAxiom s ->
    "In this system there is no sort s such that " ^ string_of_sort s ^ ":s."
  | NoRule (g,t1,t2,s1,s2) ->
    let depth = List.length g in
    let t = p_of_b_term depth $ pi () t1 t2 in
    "For term " ^ string_of_p_term t ^ "\n"
    ^ "there is no rule relating sorts "
    ^ string_of_sort s1 ^ " and " ^ string_of_sort s2

module Pipeline (SAR : Triple) = struct
  include SAR
  
end
