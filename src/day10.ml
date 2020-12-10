open! Core
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve input =
    let final = Option.value_exn (List.max_elt input ~compare) + 3 in
    let in_order = List.concat [ [ 0 ]; List.sort input ~compare; [ final ] ] in
    let pairs, _rest = List.zip_with_remainder (List.drop in_order 1) in_order in
    let gaps = List.map pairs ~f:(Tuple2.uncurry ( - )) in
    let counts =
      List.fold
        gaps
        ~init:Int.Map.empty
        ~f:(Map.update ~f:(Option.value_map ~f:succ ~default:1))
    in
    Map.find_exn counts 1 * Map.find_exn counts 3
  ;;
end

let test_case = {|16
10
15
5
1
11
7
19
6
12
4|}

let%expect_test _ =
  let input = Part_01.Input.of_string test_case in
  print_s [%sexp (Part_01.solve input : int)];
  [%expect {| 35 |}]
;;

module Part_02 = struct
  include Common

  let solve input =
    let final = Option.value_exn (List.max_elt input ~compare) + 3 in
    let in_order = List.concat [ List.sort input ~compare; [ final ] ] in
    let paths_to_numbers =
      List.fold in_order ~init:(Int.Map.singleton 0 1) ~f:(fun paths_to_numbers n ->
          let previous = List.range (n - 3) n in
          let paths_to_n =
            List.sum
              (module Int)
              previous
              ~f:(fun previous ->
                Map.find paths_to_numbers previous |> Option.value ~default:0)
          in
          Map.set paths_to_numbers ~key:n ~data:paths_to_n)
    in
    Map.find_exn paths_to_numbers final
  ;;
end

let%expect_test _ =
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve input : int)];
  [%expect {| 8 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
