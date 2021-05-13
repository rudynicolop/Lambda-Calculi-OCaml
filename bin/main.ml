open Core

let do_vanilla_command (cbv : string list) (cbn : string list) (_ : unit) : unit =
  print_endline "executing cbv:";
  List.iter ~f:print_endline cbv;
  print_endline "executing cbn";
  List.iter ~f:print_endline cbn

let vanilla_command =
  let open Command.Spec in
  Command.basic_spec
    ~summary:"Run a vanilla lambda calculus programm"
    begin empty
          +> flag "-cbv" (listed string) ~doc:"call-by-value"
          +> flag "-cbn" (listed string) ~doc:"call-by-name"
    end
    do_vanilla_command
      
let command =
  Command.group
    ~summary:"Implementations of various lambda calculi"
    [("vanilla", vanilla_command)]

let () = Command.run ~version:"1.0" command
