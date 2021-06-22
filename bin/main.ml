open Core

module RunVanilla = struct
  open Vanilla
  open Pipe
  open Step

  let cbv_command =
    Command.basic ~summary:"call-by-value"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_b_expr cbv filename))

  let normal_command =
    Command.basic ~summary:"normal-order"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_b_expr normal filename))

  let cbn_command =
    Command.basic ~summary:"call-by-name"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_b_expr cbn filename))
      
  let applicative_command =
    Command.basic ~summary:"applicative"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_b_expr applicative filename))

  let hoas_cbv_command =
    Command.basic ~summary:"cbv with closed terms"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_h_expr hoas_cbv filename))

  let hoas_cbn_command =
    Command.basic ~summary:"cbn with closed terms"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe_h_expr hoas_cbn filename))

  let command =
    Command.group
      ~summary:"Run a vanilla lambda calculus programm"
      ["-cbv",cbv_command;
       "-cbn",cbn_command;
       "-app",applicative_command;
       "-normal",normal_command;
       "-cbv-hoas", hoas_cbv_command;
       "-cbn-hoas", hoas_cbn_command]
end

let command =
  Command.group
    ~summary:"Implementations of various lambda calculi"
    ["vanilla", RunVanilla.command]

let () = Command.run ~version:"1.0" command
