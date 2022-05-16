open Core
open Option
open Util
open FunUtil

(** Command flags. *)
let cbv_flag = "-cbv"
let cbn_flag = "-cbn"
let appl_flag = "-appl"
let normal_flag = "-normal"
let cbv_hoas_flag = "-cbv-hoas"
let cbn_hoas_flag = "-cbn-hoas"
let type_flag = "-type"

(** Command summaries. *)
let cbv_summary = "call by value"
let cbn_summary = "call by name"
let appl_summary = "full applicative order"
let normal_summary = "full normal order"
let type_summary = "typing derivation"

let command_template f msg =
  Command.basic ~summary:msg
    Command.Param.(
      map
        (anon ("filename" %: string))
        ~f:(fun filename _ -> f filename))
  
module RunVanilla = struct
  open Vanilla
  open Pipe
  open Step

  let b_expr_command red =
    red |> pipe_b_expr |> command_template

  let hoas_command red =
    red |> pipe_h_expr |> command_template

  let command =
    Command.group
      ~summary:"Run a vanilla lambda calculus programm"
      [cbv_flag, b_expr_command cbv cbv_summary;
       cbn_flag, b_expr_command cbn cbn_summary;
       appl_flag, b_expr_command applicative appl_summary;
       normal_flag, b_expr_command normal normal_summary;
       cbv_hoas_flag, hoas_command hoas_cbv cbv_summary;
       cbn_hoas_flag, hoas_command hoas_cbn cbn_summary]
end

module RunStlc = struct
  open Stlc
  open Pipe
  open Reduce

  let run_b_expr_command red =
    red |> run_b_expr |> command_template
  
  let type_b_expr_command =
    (my_ignore >> parse_and_type |> command_template) type_summary

  let command =
    Command.group
      ~summary:"Run a vanilla lambda calculus programm"
      [cbv_flag, run_b_expr_command cbv cbv_summary;
       cbn_flag, run_b_expr_command cbn cbn_summary;
       appl_flag, run_b_expr_command applicative appl_summary;
       normal_flag, run_b_expr_command normal normal_summary;
       type_flag, type_b_expr_command]
end

module RunSF = struct
  open Sf
  open Reduce
  open Pipe

  let type_b_expr_command =
    (my_ignore >> parse_and_type |> command_template) type_summary

  let run_b_expr_command red =
    red |> run_b_expr |> command_template
  
  let command =
    Command.group
      ~summary:"Run a System-F program"
      [type_flag, type_b_expr_command;
       cbv_flag, run_b_expr_command cbv cbv_summary;
       cbn_flag, run_b_expr_command cbn cbn_summary;
       appl_flag, run_b_expr_command appl appl_summary;
       normal_flag, run_b_expr_command normal normal_summary]
end

module RunOmega = struct
  open Omega
  open Reduce
  open Pipe

  let type_b_term_command =
    (my_ignore >> parse_and_type |> command_template) type_summary

  let run_b_term_command red =
    red |> run_b_term |> command_template
  
  let command =
    Command.group
      ~summary:"Run an omega program"
      [type_flag, type_b_term_command;
       cbv_flag, run_b_term_command cbv cbv_summary;
       cbn_flag, run_b_term_command cbn cbn_summary;
       appl_flag, run_b_term_command appl appl_summary;
       normal_flag, run_b_term_command normal normal_summary]
end

module RunFomega = struct
  open Fomega
  open Reduce
  open Pipe

  let type_b_term_command =
    (my_ignore >> parse_and_type |> command_template) type_summary

  let run_b_term_command red =
    red |> run_b_term |> command_template
  
  let command =
    Command.group
      ~summary:"Run an omega program"
      [type_flag, type_b_term_command;
       cbv_flag, run_b_term_command cbv cbv_summary;
       cbn_flag, run_b_term_command cbn cbn_summary;
       appl_flag, run_b_term_command appl appl_summary;
       normal_flag, run_b_term_command normal normal_summary]
end

module RunPTS = struct
  open Pts
  open Pipe
  open Red

  let type_term_command parse_and_type =
    (my_ignore >> parse_and_type |> command_template) type_summary

  let run_term_command run_term red =
    red |> run_term |> command_template

  let stlc_command =
    Command.group
      ~summary:"Simply-typed lambda calculus in a pure type system."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rule: ( * , * ).")
      [type_flag, type_term_command STLC_Pipe.parse_and_type;
       appl_flag, run_term_command STLC_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command STLC_Pipe.run_b_term normal normal_summary]

  let sysf_command =
    Command.group
      ~summary:"System F as pure type system."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( □ , * ).")
      [type_flag, type_term_command SystemF_Pipe.parse_and_type;
       appl_flag, run_term_command SystemF_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command SystemU_Pipe.run_b_term normal normal_summary]

  let omega_command =
    Command.group
      ~summary:"Lambda Omega as a pure type system."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( □ , □ ).")
      [type_flag, type_term_command LambdaOmgea_Pipe.parse_and_type;
       appl_flag, run_term_command LambdaOmgea_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command LambdaOmgea_Pipe.run_b_term normal normal_summary]

  let fomega_command =
    Command.group
      ~summary:"System F + Omega as a pure type system."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules:  ( *  , *  ) ( □ , * ) ( □ , □ ).")
      [type_flag, type_term_command SysFOmega_Pipe.parse_and_type;
       appl_flag, run_term_command SysFOmega_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command SysFOmega_Pipe.run_b_term normal normal_summary]

  let lp_command =
    Command.group
      ~summary:"Lambda P."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( * , □ ).")
      [type_flag, type_term_command LambdaP_Pipe.parse_and_type;
       appl_flag, run_term_command LambdaP_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command LambdaP_Pipe.run_b_term normal normal_summary]

  let sysfp_command =
    Command.group
      ~summary:"Lambda P + System F."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( * , □ ) ( □ , * ).")
      [type_flag, type_term_command LambdaP_SysF_Pipe.parse_and_type;
       appl_flag, run_term_command LambdaP_SysF_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command LambdaP_SysF_Pipe.run_b_term normal normal_summary]

  let pomega_command =
    Command.group
      ~summary:"Lambda P + Omega."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( * , □ ) ( □ , □ ).")
      [type_flag, type_term_command LambdaP_Omega_Pipe.parse_and_type;
       appl_flag, run_term_command LambdaP_Omega_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command LambdaP_Omega_Pipe.run_b_term normal normal_summary]

  let coc_command =
    Command.group
      ~summary:"The Calculus of Constructions."
      ?readme:(some $ consume "Sorts: *, □. Axiom: * : □. Rules: ( * , * ) ( * , □ ) ( □ , * ) ( □ , □ ).")
      [type_flag, type_term_command COC_Pipe.parse_and_type;
       appl_flag, run_term_command COC_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command COC_Pipe.run_b_term normal normal_summary]

  let hol_command =
    Command.group
      ~summary: "Higher Order Logic."
      ?readme:(some $ consume "Sorts: *, □, ∆. Axioms: * : □, □ : ∆. Rules: ( * , * ) ( □ , * ) ( □ , □ ).")
      [type_flag, type_term_command Lambda_HOL_Pipe.parse_and_type;
       appl_flag, run_term_command Lambda_HOL_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command Lambda_HOL_Pipe.run_b_term normal normal_summary]

  let holext_command =
    Command.group
      ~summary:"Higher Order Logic extended with ( ∆ , * )."
      ?readme:(some $ consume
                 "Sorts: *, □, ∆. Axioms: * : □, □ : ∆. Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , * ). ")
      [type_flag, type_term_command Lambda_HOL_Extended_Pipe.parse_and_type;
       appl_flag, run_term_command
         Lambda_HOL_Extended_Pipe.run_b_term
         appl appl_summary;
       normal_flag, run_term_command
         Lambda_HOL_Extended_Pipe.run_b_term
         normal normal_summary]

  let sysu_minus_command =
    Command.group
      ~summary:"System U-."
      ?readme:(some $ consume
                 "Sorts: *, □, ∆. Axioms: * : □, □ : ∆. Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , □ ).")
      [type_flag, type_term_command System_U_Minus_Pipe.parse_and_type;
       appl_flag, run_term_command System_U_Minus_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command System_U_Minus_Pipe.run_b_term normal normal_summary]

  let sysu_command =
    Command.group
      ~summary:"System U."
      ?readme:(some $
               consume
                 "Sorts: *, □, ∆. Axioms: * : □, □ : ∆. Rules: ( * , * ) ( □ , * ) ( □ , □ ) ( ∆ , * ) ( ∆ , □ ).")
      [type_flag, type_term_command SystemU_Pipe.parse_and_type;
       appl_flag, run_term_command SystemU_Pipe.run_b_term appl appl_summary;
       normal_flag, run_term_command SystemU_Pipe.run_b_term normal normal_summary]
  
  let command =
    Command.group
      ~summary:"Run a pure type system."
      ["-stlc",stlc_command;
       "-sysf",sysf_command;
       "-omega",omega_command;
       "-fomega",fomega_command;
       "-lp",lp_command;
       "-sysfp",sysfp_command;
       "-pomega",pomega_command;
       "-coc",coc_command;
       "-hol",hol_command;
       "-holext",holext_command;
       "-sysu-",sysu_minus_command;
       "-sysu",sysu_command]
end

let command =
  Command.group
    ~summary:"Implementations of various lambda calculi"
    ["vanilla", RunVanilla.command;
     "stlc", RunStlc.command;
     "sf",RunSF.command;
     "omega",RunOmega.command;
     "fomega",RunFomega.command;
     "pts", RunPTS.command]

let () = Command.run ~version:"1.0" command
