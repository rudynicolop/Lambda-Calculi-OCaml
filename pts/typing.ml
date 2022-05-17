open Core
open Util
open FunUtil
open Syntax
open Equiv
open Triple
open Sub

let rec string_of_list (f : 'a -> string) : 'a list -> string =
  function
  | [] -> "[]"
  | h :: t -> f h ^ "::" ^ string_of_list f t

(** Type errors. *)
type type_error =
  | NoAxiom of sort
  | UnboundVar of b_term list * int
  | BadAbsParam of b_term list * b_term * b_term * b_term
  | BadAbsResult of b_term list * b_term * b_term * b_term * b_term
  | IllegalApp of b_term list * b_term * b_term * b_term
  | BadPiLeft of b_term list * b_term * b_term * b_term
  | BadPiRight of b_term list * b_term * b_term * b_term
  | NoRulePi of b_term list * b_term * b_term * sort * sort
  | NoRuleAbs of b_term list * b_term * b_term * b_term * sort * sort

module Judge (SAR : Triple) = struct
  include SAR

  (** Typing judgement. *)
  let rec (|-) (g : b_term list)
    : b_term -> (b_term, type_error) Result.t =
    print_endline $ "Context is " ^ string_of_list string_of_b_term g;
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
    | App (e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      g |- e2 >>= fun t2 ->
      begin match weak_norm t1 with
        | Pi (_,t,t') when t == t2 ->
          return $ sub ~arg:e2 t'
        | _ -> fail $ IllegalApp (g,e1,e2,t1)
      end
    | Abs (_,e1,e2) ->
      let open Result in
      e1 :: g |- e2 >>= fun t2 ->
      g |- pi () e1 t2 >>| fun _ -> pi () e1 t2
      (*print_endline $ "Typing " ^ string_of_b_term (abs () e1 e2);
      let open Result in
        g |- e1 >>= fun t1 ->
      print_endline $ "Param " ^ string_of_b_term e1 ^ " has type " ^ string_of_b_term t1;
      e1 :: List.map ~f:(rename ((+) 1)) g |- e2 >>= fun t2 ->
      print_endline $ "Result " ^ string_of_b_term e2 ^ " has type " ^ string_of_b_term t2;
      e1 :: List.map ~f:(rename ((+) 1)) g |- t2 >>= fun t2' ->
      print_endline $ "Result type " ^ string_of_b_term t2 ^ " has type " ^ string_of_b_term t2';
      begin match weak_norm t1, weak_norm t2' with
        | Sort s1, Sort s2 ->
          begin match rules s1 s2 with 
            | Some _ -> return $ pi () e1 t2
            | None -> fail $ NoRuleAbs (g,e1,e2,t2,s1,s2)
          end
        | Sort _, t -> fail $ BadAbsResult (g,e1,e2,t2,t)
        | t, _ -> fail $ BadAbsParam (g,e1,e2,t)
        end*)
    | Pi (_,e1,e2) ->
      let open Result in
      g |- e1 >>= fun t1 ->
      t1 :: List.map ~f:(rename ((+) 1)) g |- e2 >>= fun t2 ->
      begin match weak_norm t1, weak_norm t2 with
        | Sort s1, Sort s2 ->
          begin match rules s1 s2 with
          | Some s3 -> return $ sort s3
          | None -> fail $ NoRulePi (g,e1,e2,s1,s2)
          end
        | Sort _, t -> fail $ BadPiRight (g, e1, e2, t)
        | t, _ -> fail $ BadPiLeft (g, e1, e2, t)
      end
end
