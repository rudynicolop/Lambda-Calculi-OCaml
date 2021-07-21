open Core
open Syntax
open Sub
open Util
open FunUtil

(** Static Semantics. *)

(** Well-foundedness check. *)
let wf (typ_depth: int) : b_typ -> bool =
  typ_fold
    ~ctx:typ_depth ~succ:(consume $ (+) 1)
    ~f:(>) ~g:(consume $ consume id) ~h:(&&)

type type_error =
  | UnboundVar of int * b_typ list * int
  | IllegalApp of int * b_typ list * b_typ * b_expr * b_expr
  | TypMismatch of int * b_typ list * b_typ * b_typ * b_expr * b_expr
  | IllegalTypApp of int * b_typ list * b_expr * b_typ * b_typ
  | NotWellFounded of int * b_typ list * b_typ * b_expr

(** Type-checking. *)
let rec type_b_expr (td: int) (g : b_typ list)
  : b_expr -> (b_typ,type_error) Result.t = function
  | Var n ->
    begin match List.nth g n with
      | Some t -> Result.return t
      | None ->
        UnboundVar (td,g,n) |> Result.fail
    end
  | Abs (_,t,e) as abs ->
    if wf td t then
      let open Result in
      type_b_expr td (t :: g) e >>| tarrow t
    else
      NotWellFounded (td,g,t,abs) |> Result.fail
  | App (e1,e2) ->
    let open Result in
    type_b_expr td g e2 >>= fun t2 ->
    type_b_expr td g e1 >>=
    begin function
      | TArrow (t,t') when t =? t2 -> return t'
      | TArrow (t,_) ->
        TypMismatch (td,g,t,t2,e1,e2) |> fail
      | t1 -> IllegalApp (td,g,t1,e1,e2) |> fail
    end
  | TypAbs (_,e) ->
    let open Result in
    type_b_expr
      (td + 1)
      (List.map ~f:(shift_typ 0 1) g)
      e >>| tforall ()
  | TypApp (e,t) as tapp ->
    let open Result in
    if wf td t then
      type_b_expr td g e >>= begin function
        | TForall (_,te) -> return $ typ_sub 0 t te
        | t' -> IllegalTypApp (td,g,e,t',t) |> fail
      end
    else
      NotWellFounded (td,g,t,tapp) |> fail
