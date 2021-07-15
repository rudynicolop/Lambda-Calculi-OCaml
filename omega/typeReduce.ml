open Core
open Util
open FunUtil
open Syntax
open Sub
open Option

let rec normal : b_typ -> b_typ option = function
  | TAbs (_,k,t) -> normal t >>| tabs () k
  | TApp (TAbs (_,_,t1), t2) -> some $ sub_typ_top t2 t1
  | TApp (t1,t2) ->
    begin match normal t1 with
      | Some t1' -> some $ tapp t1' t2
      | None -> normal t2 >>| tapp t1
    end
  | TArrow (t1,t2) ->
    begin match normal t1 with
      | Some t1' -> some $ tarrow t1' t2
      | None -> normal t2 >>| tarrow t1
    end
  | _ -> None

let normalize : b_typ -> b_typ =
  refl_trans_clos normal my_ignore
