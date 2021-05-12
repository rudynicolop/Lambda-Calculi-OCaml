(* TODO: command line harness *)

open Core
(* module V = Vanilla *)

let command =
  begin fun _ -> print_endline "Hello World" end
  |> Command.Param.return
  |> Command.basic ~summary:"I dunno man"
  
    

let () = Command.run ~version:"1.0" command
