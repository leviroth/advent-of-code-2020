open! Core
open! Import

module Seat = struct
  type t = int [@@deriving sexp]

  let parser =
    Angstrom.scan_state 0 (fun n c ->
        Option.map
          ~f:(( + ) (n lsl 1))
          (match c with
          | 'B' | 'R' -> Some 1
          | 'F' | 'L' -> Some 0
          | _ -> None))
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Seat)
  module Output = Int
end

let%expect_test _ =
  let test_cases = [ "FBFBBFFRLR"; "BFFFBBFRRR"; "FFFBBBFRRR"; "BBFFBBFRLL" ] in
  let open Common in
  List.iter test_cases ~f:(fun input ->
      let id = Input.Single.of_string input in
      print_s [%sexp { input : string; id : int }]);
  [%expect
    {|
    ((input FBFBBFFRLR) (id 357))
    ((input BFFFBBFRRR) (id 567))
    ((input FFFBBBFRRR) (id 119))
    ((input BBFFBBFRLL) (id 820)) |}]
;;

module Part_01 = struct
  include Common

  let solve i = List.max_elt i ~compare |> Option.value_exn
end

module Part_02 = struct
  include Common

  let solve ids =
    let min = List.min_elt ids ~compare |> Option.value_exn in
    let max = List.max_elt ids ~compare |> Option.value_exn in
    let set = Int.Hash_set.of_list ids in
    List.find_exn (List.range min (max + 1)) ~f:(Fn.non (Hash_set.mem set))
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
