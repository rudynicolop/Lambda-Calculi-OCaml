open Core
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

  let command.group =
    Command.group
      ~summary:"Run a pure type system"
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
