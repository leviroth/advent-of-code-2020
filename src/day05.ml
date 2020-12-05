open! Core
open! Import

module Common = struct
  module Input = Input.Make_parseable_many (struct
    type t = int

    let parser =
      let open Angstrom in
      let as_bit ~true_ ~false_ = char true_ *> return 1 <|> char false_ *> return 0 in
      let to_int bits = List.fold bits ~init:0 ~f:(fun n bit -> (n lsl 1) + bit) in
      map2
        (count 7 (as_bit ~true_:'B' ~false_:'F'))
        (count 3 (as_bit ~true_:'R' ~false_:'L'))
        ~f:(fun row col -> (8 * to_int row) + to_int col)
    ;;
  end)

  module Output = Int
end

module Part_01 = struct
  include Common

  let solve i = List.max_elt i ~compare |> Option.value_exn
end

module Part_02 = struct
  include Common

  let solve i =
    let min = List.min_elt i ~compare |> Option.value_exn in
    let max = List.max_elt i ~compare |> Option.value_exn in
    let set = Int.Hash_set.of_list i in
    List.find_exn (List.range min (max + 1)) ~f:(Fn.non (Hash_set.mem set))
  ;;
end

let parts : (module Solution.Part) list = [ (module Part_01); (module Part_02) ]
