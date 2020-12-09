open! Core
open! Import

module Common = struct
  module Input = Input.Int_list
  module Output = Int
end

module Part_01 = struct
  include Common

  let solve input ~preamble_length =
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

  let solve = solve ~preamble_length:25
end

let parts : (module Solution.Part) list = [ (module Part_01) ]
