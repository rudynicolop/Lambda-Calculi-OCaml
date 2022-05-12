open Core
open Util
open FunUtil
open Syntax
open Equiv
open Triple
open Sub

(** Type errors. *)
type 's type_error =
  | UnboundVar of 's b_term list * int
  | BadAbsParam of 's b_term list * 's b_term * 's b_term * 's b_term
  | IllegalApp of 's b_term list * 's b_term * 's b_term * 's b_term
  | BadPiLeft of 's b_term list * 's b_term * 's b_term * 's b_term
  | BadPiRight of 's b_term list * 's b_term * 's b_term * 's b_term
  | NoAxiom of 's
  | NoRule of 's b_term list * 's b_term * 's b_term * 's * 's

module Judge (SAR : Triple) = struct
  include SAR
  include EquivWH (SAR)

  (** Typing judgement. *)
  let rec (|-) (g : sort b_term list)
    : sort b_term -> (sort b_term, sort type_error) Result.t =
    function
    | Sort s ->
      begin match axioms s with
        | Some s' -> Result.return $ sort s'
        | None    -> Result.fail $ NoAxiom s
      end
    | Var x ->
      begin match List.nth g x with
        | Some t -> Result.return t
        | None -> Result.fail $ UnboundVar (g,x)
      end
    | Abs (_,e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      begin match weak_norm t1 with
        | Sort _ -> (e1 :: g) |- e2 >>| pi () t1
        | _ -> Result.fail $ BadAbsParam (g,e1,e2,t1)
      end
    | App (e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      g |- e2 >>= fun t2 ->
      begin match weak_norm t1 with
        | Pi (_,t,t') when t == t2 ->
          Result.return $ sub ~arg:e2 t'
        | _ -> Result.fail $ IllegalApp (g,e1,e2,t1)
      end
    | Pi (_,e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      t1 :: g |- e2 >>= fun t2 ->
      begin match weak_norm t1, weak_norm t2 with
        | Sort s1, Sort s2 ->
          if rules s1 s2 then
            Result.return t2
          else
            Result.fail $ NoRule (g,e1,e2,s1,s2)
        | Sort _, t -> Result.fail $ BadPiRight (g, e1, e2, t)
        | t, _ -> Result.fail $ BadPiLeft (g, e1, e2, t)
      end
end
