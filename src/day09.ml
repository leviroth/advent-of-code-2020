open! Core
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve' input ~preamble_length =
    let preamble = List.take input preamble_length in
    let rest = List.drop input preamble_length in
    let deque = Deque.of_array (Array.of_list preamble) in
    List.find_exn rest ~f:(fun n ->
        let sums =
          let recent = Deque.to_list deque in
          List.map (List.cartesian_product recent recent) ~f:(Tuple2.uncurry ( + ))
        in
        match List.mem sums n ~equal with
        | false -> true
        | true ->
          Deque.drop_front deque;
          Deque.enqueue_back deque n;
          false)
  ;;

  let solve = solve' ~preamble_length:25
end

module Part_02 = struct
  include Common

  let solve' input ~preamble_length =
    let target = Part_01.solve' input ~preamble_length in
    let input = Array.of_list input in
    let upper_bound = Array.length input - 1 in
    let totals = Int_pair.Table.create () in
    Hashtbl.set totals ~key:(0, upper_bound) ~data:(Array.sum (module Int) input ~f:ident);
    List.iter (List.range 1 upper_bound) ~f:(fun left ->
        Hashtbl.set
          totals
          ~key:(left, upper_bound)
          ~data:(Hashtbl.find_exn totals (left - 1, upper_bound) - input.(left - 1));
        List.iter
          (List.range ~stride:(-1) (upper_bound - 1) left)
          ~f:(fun right ->
            Hashtbl.set
              totals
              ~key:(left, right)
              ~data:(Hashtbl.find_exn totals (left, right + 1) - input.(right + 1))));
    let totals = Hashtbl.to_alist totals in
    let left, right =
      List.find_map_exn totals ~f:(fun (bounds, total) ->
          match total = target with
          | false -> None
          | true -> Some bounds)
    in
    let numbers = Array.slice input left (right + 1) in
    let min = Array.min_elt numbers ~compare |> Option.value_exn in
    let max = Array.max_elt numbers ~compare |> Option.value_exn in
    min + max
  ;;

  let solve = solve' ~preamble_length:25
end

let%expect_test _ =
  let test_case =
    {|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576|}
  in
  let input = Part_02.Input.of_string test_case in
  print_s [%sexp (Part_02.solve' input ~preamble_length:5 : int)];
  [%expect {| 62 |}]
;;

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
