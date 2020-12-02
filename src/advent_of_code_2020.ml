open! Core
open! Import

let days : (module Solution.Day) list = [ (module Day01) ]

let command =
  Command.group
    ~summary:"Solve a selected puzzle"
    (List.map days ~f:(fun (module Day) ->
         let command =
           Command.group
             ~summary:"Solve the selected part"
             (List.map Day.parts ~f:(fun (module Part) ->
                  let command =
                    Command.basic
                      ~summary:"Solve the puzzle"
                      (let%map_open.Command file =
                         flag "-filename" (required Filename.arg_type) ~doc:" "
                       in
                       fun () ->
                         Part.Input.load file
                         |> Part.solve
                         |> Part.Output.to_string
                         |> printf "%s\n")
                  in
                  Int.to_string Part.one_based_index, command))
         in
         Int.to_string Day.day_of_month, command))
;;
