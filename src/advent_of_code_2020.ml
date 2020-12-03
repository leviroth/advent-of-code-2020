open! Core
open! Import

let days : (module Solution.Day) list = [ (module Day01); (module Day02) ]

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
                         flag
                           "-filename"
                           (optional_with_default
                              (sprintf "input/day%s.txt" (pad_int Day.day_of_month))
                              Filename.arg_type)
                           ~doc:" "
                       in
                       fun () ->
                         Part.Input.load file
                         |> Part.solve
                         |> Part.Output.to_string
                         |> printf "%s\n")
                  in
                  pad_int Part.one_based_index, command))
         in
         pad_int Day.day_of_month, command))
;;
