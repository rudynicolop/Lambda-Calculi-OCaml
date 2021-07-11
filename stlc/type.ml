open Core
open Syntax

(** Type-checking. *)

(** Typing errors. *)
type type_error =
  | UnboundVar of typ list * int
  | IllegalApp of typ list * typ * b_expr * b_expr
  | TypMismatch of typ list * typ * typ * b_expr * b_expr

(** Typing De Bruijn syntax. *)
let rec type_b_expr (g : typ list)
  : b_expr -> (typ,type_error) Result.t = function
  | Var n ->
    begin match List.nth g n with
      | Some t -> Result.return t
      | None -> UnboundVar (g,n) |> Result.fail
    end
  | Abs(_,t,e) ->
    let open Result in
    type_b_expr (t :: g) e >>| fun t' -> Arrow (t,t')
  | App (e1,e2) ->
    let open Result in
    type_b_expr g e1 >>= fun t1 ->
    type_b_expr g e2 >>= fun t2 ->
    match t1 with
    | Arrow (t,t') ->
      if t =? t2 then
        return t'
      else
        TypMismatch (g,t,t2,e1,e2) |> fail
    | _ -> IllegalApp (g,t1,e1,e2) |> fail

(** Generating typed-syntax. *)
(* I don't understand GADTs well enough... :(
type falsehood

let rec term_of_b_expr : type a.
  b_expr -> a list ->
  ((int,unit,a) term, string) Result.t = function
  | Var n ->
    begin fun g ->
      match List.nth g n with
      | Some t -> TVar (n, t) |> Result.return
      | None -> "Variable " ^ string_of_int n ^ " is unbound."
                |> Result.fail
    end
  | Abs (_,t,e) ->
    begin fun g ->
      let open Result in
      term_of_b_expr (t :: g) e >>| fun e -> TAbs ((),t,e)
    end
   | _ -> fun _ -> Result.fail "" *)
