open Core
open Util
open FunUtil
open Syntax
open Kinding
open TypeReduce

(** Typing errors. *)
type type_error =
  | KindingError of kind_error
  | ImproperKind of b_typ list * kind * b_typ * b_term
  | UnboundVar of b_typ list * int
  | IllegalApp of b_typ list * b_typ * b_term * b_term
  | TypMismatch of b_typ list * b_typ * b_typ * b_term * b_term

let rec typing (g: b_typ list)
  : b_term -> (b_typ,type_error) Result.t = function
  | Var n ->
    begin match List.nth g n with
      | Some t -> Result.return t
      | None -> Result.fail $ UnboundVar (g,n)
    end
  | Abs (_,t,e) as ab ->
    let open Result in
    begin match kinding [] t with
      | Ok KStar -> typing (t :: g) e >>| tarrow t
      | Ok (KArrow _ as k) -> fail $ ImproperKind (g,k,t,ab)
      | Error err -> fail $ KindingError err
    end
  | App (e1,e2) ->
    let open Result in
    typing g e2 >>= fun t2 ->
    typing g e1 >>=
    begin (function
      | TArrow (t,t')
        when normalize t == normalize t2 -> return t'
      | TArrow (t,_) -> fail $ TypMismatch (g,t,t2,e1,e2)
      | t1 -> fail $ IllegalApp (g,t1,e1,e2))
      >> normalize
    end
