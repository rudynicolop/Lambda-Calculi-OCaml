open Core
open Result
open Util
open FunUtil
open Syntax
open Sub
open Kinding
open TypeReduce

(** Typing errors. *)
type type_error =
  | KindingError of kind_error
  | ImproperKind of kind list * b_typ list * kind * b_typ * b_term
  | UnboundVar of kind list * b_typ list * int
  | IllegalApp of kind list * b_typ list * b_typ * b_term * b_term
  | TypMismatch of kind list * b_typ list * b_typ * b_typ * b_term * b_term
  | TypAppKindMismatch of kind list * b_typ list * kind * kind * b_term * b_typ
  | TypAppIllegalApp of kind list * b_typ list * b_term * b_typ * b_typ

(** Typing judgment. *)
let rec typing (kg: kind list) (tg: b_typ list)
  : b_term -> (b_typ, type_error) Result.t = function
  | Var n ->
    begin match List.nth tg n with
      | Some k -> return k
      | None -> fail $ UnboundVar (kg,tg,n)
    end
  | Abs (_,t,e) as ab ->
    begin match kinding kg t with
      | Ok KStar -> typing kg (t :: tg) e >>| tarrow t
      | Ok k -> fail $ ImproperKind (kg,tg,k,t,ab)
      | Error err -> fail $ KindingError err
    end
  | App (e1,e2) ->
    typing kg tg e2 >>= fun t2 ->
    typing kg tg e1 >>= fun t1 ->
    begin match normalize t1 with
      | TArrow (t,t')
        when normalize t == normalize t2 -> return t'
      | TArrow (t,_) -> fail $ TypMismatch (kg,tg,t,t2,e1,e2)
      | t1 -> fail $ IllegalApp (kg,tg,t1,e1,e2)
    end
  | TypAbs (_,k,e) ->
    typing
      (k :: kg)
      (List.map ~f:(rename_typ ((+) 1)) tg)
      e >>| tforall () k
  | TypApp (e,t) ->
    begin match kinding kg t with
      | Error err -> fail $ KindingError err
      | Ok k ->
        typing kg tg e >>= fun te ->
        begin match normalize te with
          | TForall (_,ke,te)
            when ke =? k -> return $ sub_typ ~arg:t te
          | TForall (_,ke,_) ->
            fail $ TypAppKindMismatch (kg,tg,ke,k,e,t)
          | te -> fail $ TypAppIllegalApp (kg,tg,e,t,te)
        end
    end
