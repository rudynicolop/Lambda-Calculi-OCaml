open Core

let vanilla_command =
  let open Vanilla.Pipe in
  let open Vanilla.Step in
  let cbv_command =
    Command.basic ~summary:"call-by-value"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe cbv filename)) in
  let normal_command =
    Command.basic ~summary:"normal-order"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe normal filename)) in
  let cbn_command =
    Command.basic ~summary:"call-by-name"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe cbn filename)) in
  let applicative_command =
    Command.basic ~summary:"applicative"
      Command.Param.(
        map
          (anon ("filename" %: string))
          ~f:(fun filename -> fun _ -> pipe applicative filename)) in
  Command.group
    ~summary:"Run a vanilla lambda calculus programm"
    ["-cbv",cbv_command;
     "-cbn",cbn_command;
     "-app",applicative_command;
     "-normal",normal_command]
      
let command =
  Command.group
    ~summary:"Implementations of various lambda calculi"
    ["vanilla", vanilla_command]

let () = Command.run ~version:"1.0" command
