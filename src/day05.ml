open! Core
open! Import

module Seat = struct
  type t = Int_pair.t [@@deriving sexp]

  let parser =
    let open Angstrom in
    let as_bit ~true_ ~false_ = char true_ *> return 1 <|> char false_ *> return 0 in
    let to_int bits = List.fold bits ~init:0 ~f:(fun n bit -> (n lsl 1) + bit) in
    map2
      (count 7 (as_bit ~true_:'B' ~false_:'F'))
      (count 3 (as_bit ~true_:'R' ~false_:'L'))
      ~f:(fun row col -> to_int row, to_int col)
  ;;
end

module Common = struct
  module Input = Input.Make_parseable_many (Seat)
  module Output = Int

  let to_id (row, col) = (8 * row) + col
end

let%expect_test _ =
  let test_cases = [ "FBFBBFFRLR"; "BFFFBBFRRR"; "FFFBBBFRRR"; "BBFFBBFRLL" ] in
  let open Common in
  List.iter test_cases ~f:(fun input ->
      let seat = Input.Single.of_string input in
      let id = to_id seat in
      print_s [%sexp { input : string; seat : Seat.t; id : int }]);
  [%expect
    {|
    ((input FBFBBFFRLR) (seat (44 5)) (id 357))
    ((input BFFFBBFRRR) (seat (70 7)) (id 567))
    ((input FFFBBBFRRR) (seat (14 7)) (id 119))
    ((input BBFFBBFRLL) (seat (102 4)) (id 820)) |}]
;;

module Part_01 = struct
  include Common

  let solve i = List.map i ~f:to_id |> List.max_elt ~compare |> Option.value_exn
end

module Part_02 = struct
  include Common

  let solve i =
    let ids = List.map i ~f:to_id in
    let min = List.min_elt ids ~compare |> Option.value_exn in
    let max = List.max_elt ids ~compare |> Option.value_exn in
    let set = Int.Hash_set.of_list ids in
    List.find_exn (List.range min (max + 1)) ~f:(Fn.non (Hash_set.mem set))
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
