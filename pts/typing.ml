open Core
open Util
open FunUtil
open Syntax
open Equiv
open Triple
open Sub

(** Type errors. *)
type type_error =
  | UnboundVar of b_term list * int
  | BadAbsParam of b_term list * b_term * b_term * b_term
  | IllegalApp of b_term list * b_term * b_term * b_term
  | BadPiLeft of b_term list * b_term * b_term * b_term
  | BadPiRight of b_term list * b_term * b_term * b_term
  | NoAxiom of sort
  | NoRule of b_term list * b_term * b_term * sort * sort

module Judge (SAR : Triple) = struct
  include SAR

  (** Typing judgement. *)
  let rec (|-) (g : b_term list)
    : b_term -> (b_term, type_error) Result.t =
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
        | _ -> fail $ BadAbsParam (g,e1,e2,t1)
      end
    | App (e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      g |- e2 >>= fun t2 ->
      begin match weak_norm t1 with
        | Pi (_,t,t') when t == t2 ->
          return $ sub ~arg:e2 t'
        | _ -> fail $ IllegalApp (g,e1,e2,t1)
      end
    | Pi (_,e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      t1 :: g |- e2 >>= fun t2 ->
      begin match weak_norm t1, weak_norm t2 with
        | Sort s1, Sort s2 ->
          begin match rules s1 s2 with
          | Some s3 -> return $ sort s3
          | None -> fail $ NoRule (g,e1,e2,s1,s2)
          end
        | Sort _, t -> fail $ BadPiRight (g, e1, e2, t)
        | t, _ -> fail $ BadPiLeft (g, e1, e2, t)
      end
end
